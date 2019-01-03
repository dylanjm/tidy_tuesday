library(tidyverse)
library(readxl)
library(albersusa)

coffee_list <- purrr::map(1:3, 
                   ~read_xlsx(here::here("data/week6_coffee_chains.xlsx"), 
                              sheet = .x))
map2(coffee_list, 
     c("starbucks","tim_h","dunkin_donuts"), 
     ~ assign(.y, .x, envir = .GlobalEnv))

# create our map data
us <- usa_composite()
us_map <- broom::tidy(us, region = "name") %>% 
  filter(!id %in% c("Alaska","Hawaii"))

ggplot() +
  geom_map(data = us_map, map = us_map,
           aes(x = long, y = lat, map_id = id),
           color="#ffffff", fill="#d2d3d5") + 
  geom_point(data = starbucks %>% 
               filter(Country %in% "US",
                      !`State/Province` %in% c("AK","HI")),
             aes(x = Longitude, y = Latitude)) +
  theme(plot.background = element_rect(fill = "#d2d3d5"),
        panel.background = element_rect(fill = "#d2d3d5"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())

###############################
#                             #
#       Tidy Tuesday          #
#        2018-05-07           #
#                             #
###############################

# created using taraskaduk's amazing tutorial:
# https://taraskaduk.com/2017/11/26/pixel-maps/ (crosspost: https://medium.com/taras-kaduk/r-walkthrough-create-a-pixel-map-537ce12c2f0c)


library(readxl)
library(tidyverse)
library(maps)
library(janitor)

coffee <- read_excel(here::here("data/week6_coffee_chains.xlsx")) %>% 
  clean_names()

coffee <- coffee %>% 
  mutate(long_round = round(longitude),
         lat_round = round(latitude)) %>% 
  filter(country == "US",
         !state_province %in% c("AK","HI"))

lat <- data_frame(lat = seq(-90, 90, by = 1))

long <- data_frame(long = seq(-180, 180, by = 1))

dots <- lat %>% 
  merge(long, all = TRUE)

# only include dots that are within borders
# also, exclude lakes

dots <- 
  dots %>% 
  mutate(country = map.where('usa', long, lat),
         lakes = map.where('lakes', long, lat)) %>% 
  filter(!is.na(country) & is.na(lakes)) %>% 
  select(-lakes)

theme <- 
  theme_void() +
  theme(panel.background = element_rect(fill="#d2d3d5"),
        plot.background = element_rect(fill="#d2d3d5"),
        plot.title = element_text(face="bold", colour="#3C3C3C", size = 16),
        plot.subtitle = element_text(colour="#3C3C3C", size=12),
        plot.caption = element_text(colour="#3C3C3C",size=10),  
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

ggplot() +   
  geom_map(data = us_map, map = us_map,
           aes(x = long, y = lat, map_id = id),
           color="#ffffff", fill="#d2d3d5") + 
  # base layer of map dots
  geom_point(data = coffee, 
             aes(x = long_round, y = lat_round, color = brand), size = 0.7) +
  theme

map_brands

map_empty <- 
  ggplot() +   
  # base layer of map dots
  geom_point(data = dots, aes(x = long, y = lat), col = "#F2EEE9", size = 0.7) + 
  geom_point(data = coffee, aes(x = long_round, y = lat_round), color = "#212121", size = 0.7) +
  theme

map_empty

ggsave('.//plot//map_empty.png', 
       plot = map_empty,
       device = 'png', 
       path = getwd(), 
       width = 360, 
       height = 180, 
       units = 'mm',
       dpi = 250)
