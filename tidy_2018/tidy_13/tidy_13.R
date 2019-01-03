library(tidyverse)
library(ggthemes)
library(rvest)
library(gridExtra)
library(scico)
library(rworldmap)
library(cowplot)

drink_data <- read_csv(here::here("data/week13_alchohol_global.csv")) %>% 
  mutate(country = case_when(
    country == "Russian Federation" ~ "Russia",
    country == "United Kingdom" ~ "UK",
    country == "DR Congo" ~ "Democratic Republic of the Congo",
    country == "Congo" ~ "Republic of Congo",
    TRUE ~ country
  ))

############################################################################
# Attempt at Creating Table
############################################################################
test <- drink_data %>% 
  select(-total_litres_of_pure_alcohol) %>% 
  gather(key = "type", value = "consumption", 2:4) %>% 
  group_by(type) %>% 
  top_n(10, consumption) %>%
  ungroup()

beer_table <- test %>% 
  filter(type == "beer_servings") %>% 
  select(-type) %>% 
  arrange(desc(consumption)) %>% 
  rename(BEER = consumption) %>% 
  tableGrob()

spirit_table <- test %>% 
  filter(type == "spirit_servings") %>% 
  select(-type) %>% 
  arrange(desc(consumption)) %>% 
  rename(SPIRITS = consumption) %>% 
  tableGrob()

wine_table <- test %>% 
  filter(type == "wine_servings") %>% 
  select(-type) %>% 
  arrange(desc(consumption)) %>% 
  rename(WINE = consumption) %>% 
  tableGrob()

grid.arrange(beer_table, spirit_table, wine_table, nrow = 1)

############################################################################
# GeoSpatial Visualizations
############################################################################
map_world = map_data(map = "world") %>% 
  filter(region != "Antarctica")

pure <- ggplot() + 
  geom_map(data = map_world, map = map_world, 
           aes(x = long, y = lat, map_id = region),
           color = "grey20", size = .5) + 
  geom_map(data = drink_data, map = map_world,
           aes(fill = total_litres_of_pure_alcohol, map_id = country)) + 
  scale_fill_gradient(low = "white", high = "#f1bb2b", labels = c(0,7,14), breaks = c(0,7,14)) + 
  labs(title = "Total Litres of Pure Alcohol Consumption Worldwide 2010",
       fill = "Litres") + 
  theme_fivethirtyeight() + 
  theme(axis.text = element_blank(),
        legend.title = element_text(face = "bold"),
        legend.position = c(.05,.2),
        legend.direction = "vertical",
        plot.title = element_text(size = 9))

beer <- ggplot() + 
  geom_map(data = map_world, map = map_world, 
           aes(x = long, y = lat, map_id = region),
           color = "grey20", size = .5) + 
  geom_map(data = drink_data, map = map_world,
           aes(fill = beer_servings, map_id = country)) + 
  scale_fill_gradient(low = "white", high = "#008FD5") + 
  labs(title = "Average Number of Servings per Capita of Beer - Worldwide 2010",
       fill = "Servings") + 
  theme_fivethirtyeight() + 
  theme(axis.text = element_blank(),
        legend.title = element_text(face = "bold"),
        legend.position = c(.05,.2),
        legend.direction = "vertical",
        plot.title = element_text(size = 9))

spirit <- ggplot() + 
  geom_map(data = map_world, map = map_world, 
           aes(x = long, y = lat, map_id = region),
           color = "grey20", size = .5) + 
  geom_map(data = drink_data, map = map_world,
           aes(fill = spirit_servings, map_id = country)) + 
  scale_fill_gradient(low = "white", high = "#77AB43") +
  labs(title = "Average Number of Servings per Capita of Spirits - Worldwide 2010",
       fill = "Servings") + 
  theme_fivethirtyeight() + 
  theme(axis.text = element_blank(),
        legend.title = element_text(face = "bold"),
        legend.position = c(.05,.2),
        legend.direction = "vertical",
        plot.title = element_text(size = 9))

wine <- ggplot() + 
  geom_map(data = map_world, map = map_world, 
           aes(x = long, y = lat, map_id = region),
           color = "grey20", size = .5) + 
  geom_map(data = drink_data, map = map_world,
           aes(fill = wine_servings, map_id = country)) + 
  scale_fill_gradient(low = "white", high = "#FF2700") + 
  labs(title = "Average Number of Servings per Capita of Wine - Worldwide 2010",
       fill = "Servings", captions = "Dark Filled in countries indicate no data collected") + 
  theme_fivethirtyeight() + 
  theme(axis.text = element_blank(),
        legend.title = element_text(face = "bold"),
        legend.position = c(.05,.2),
        legend.direction = "vertical", 
        plot.title = element_text(size = 9))

cowplot::plot_grid(plotlist = list(pure, beer, spirit, wine))

#####################################################################
# Plot 02 - Time Series Plot of Alcohol Consumption in US
#####################################################################
# Scrape the table from the below website using library(rvest)
date_data <- read_html("https://pubs.niaaa.nih.gov/publications/Surveillance95/tab1_10.htm")

# Grab the table from the HTML, make sure we just grab the table and then
# get it put into a data.frame
table_scrape <- date_data %>% 
  html_nodes(css = "table") %>% 
  .[[1]] %>% 
  html_table()

# Clean up data and clean the strings so that the "ending" year is the value we use
# *NOTE!* The regex (".*–"), contains an oddly coded value, it's not a dash ("-"),
# but some other character that I copied from the resultant data.frame values. 
time_dat <- table_scrape %>% 
  filter(Year != "Prohibition") %>% 
  mutate(Year = str_remove_all(Year, ".*–")) %>% 
  mutate_all(as.numeric) %>% 
  gather(key = "type", value = "consumption", 2:5)

text_labs <- tibble(
  drink_type = c("All beverages", "Beer", "Spirits", "Wine"), 
  x = c(2000,2005,2000,2005),
  y = c(2.07, 1.3, .8, .2), 
  colors = c("gold", "#008FD5","#77AB43", "#FF2700"))

time_1 <- time_dat %>% 
  filter(Year <= 1919)

time_2 <- time_dat %>% 
  filter(Year >= 1934)

ggplot(time_dat, aes(x = Year, y = consumption)) + 
  annotate("rect", xmin = 1920, xmax = 1934, ymin = -Inf, ymax = Inf, 
           fill = "grey", alpha = .45) +
  annotate("text", x = 1932, y = 2.6, label = "paste(italic(Prohibition))",
           parse = TRUE, size = 5, color = "grey30") + 
  annotate("segment", x = -Inf, xend = Inf, y = 0, yend = 0, color = "black") + 
  geom_line(aes(group = type, color = type), size = 1, linetype = "1111") +
  geom_line(data = time_1, aes(group = type, color = type), size = 1.1) +
  geom_line(data = time_2, aes(group = type, color = type), size = 1.1) + 
  geom_text(data = text_labs, aes(x = x, y = y, label = drink_type, color = drink_type),
            size = 5, family = "Andale Mono", fontface = "bold") +
  scale_y_continuous(labels = seq(0,2.5, .5),
                     breaks = seq(0,2.5, .5)) + 
  scale_x_continuous(breaks = seq(1860, 2000, 20),
                     labels = c(1860, "'80", 1900, "'20", "'40", "'60", "'80", 2000)) + 
  scale_color_manual(values = rep(c("#f1bb2b", "#008FD5","#77AB43", "#FF2700"),2)) + 
  labs(title = "U.S. Alcohol Consumption",
       subtitle = "Gallons of alcohol consumed per person* annually",
       caption = "*Age 14 and older since 1970, 15 and older prior") + 
  theme_fivethirtyeight() + 
  theme(legend.position = "none", 
        axis.text = element_text(family = "Cutive Mono", size = 13),
        plot.subtitle = element_text(size = 16),
        plot.title = element_text(size = 19))
