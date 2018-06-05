library(tidyverse)
library(glue)
library(lubridate)
library(ggmap)
library(patchwork)

# Read in data supa fast w/ data.table
trip_data <- list.files(here::here("data/PublicTripData"), 
           pattern = "*.csv", full.names = TRUE) %>% 
  map_df(~data.table::fread(.x))

# Clean up data
trip_clean <- trip_data %>% 
  filter(StartDate != "", StartTime != "") %>%
  mutate(Start = parse_date_time(glue("{StartDate} {StartTime}"), "mdY HM"),
         Hour = parse_factor(hour(Start), c(0,23:1)),
         Weekday = fct_relevel(wday(Start, label = TRUE),
                               c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))

# Tile Plot
t <- trip_clean %>% 
  count(Weekday, Hour) %>%
  filter(!Hour %in% c(1,2,3,4)) %>% 
  ggplot(aes(x = Weekday, y = Hour, fill = n)) + 
  geom_tile() +
  annotate("segment", y = 21, yend = 21, x = -Inf, xend = Inf,
           color = "black", size = .3) +
  annotate("segment", y = 0, yend = 0, x = -Inf, xend = Inf,
           color = "black", size = .3) +
  scale_x_discrete(position = "top") + 
  scale_y_discrete(labels = c("12 AM",glue("{c(11:1,12)} PM"), glue("{11:5} AM"))) +
  scale_fill_gradient(low = "white", high = "#f94d1f") + 
  labs(title = "Trips per Weekday/Hour") + 
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title = element_blank(),
        panel.grid = element_blank(), 
        axis.text.y = element_text(margin = margin(r = 20)),
        plot.title = element_text(family = "Verdana Bold", size = 18),
        axis.text = element_text(family = "Futura Medium"))

# Time Series Plot
l <- trip_clean %>% 
  mutate(floor_week = floor_date(Start, "weeks")) %>% 
  add_count(floor_week, PaymentPlan) %>% 
  add_count(floor_week) %>% 
  filter(PaymentPlan %in% "Subscriber") %>% 
  ggplot() + 
  geom_ribbon(aes(x = floor_week, ymin = 0, ymax = nn), 
              fill = "grey", color = "grey50") +
  geom_ribbon(aes(x = floor_week, ymin = 0, ymax = n),
              fill = "#f69366", color = "grey50") +
  scale_y_continuous(labels = glue::glue("{seq(0,15,5)}K")) + 
  scale_x_datetime(date_labels = "%b %y", 
                   breaks = seq(as_datetime("2016-08-01"), 
                                as_datetime("2018-02-01"), "6 months")) + 
  labs(title = "Trips Per Week") + 
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Verdana Bold", 
                                  size = 18, margin = margin(b = 20)),
        axis.text = element_text(family = "Futura Medium", size = 13))

# Map Plot
portland <- get_map(location = c(left = -122.7041, 
                                 bottom = 45.4879, 
                                 right = -122.6361, 
                                 top = 45.5616),
                    zoom = 13)

# Determine top 148 Starting Hubs
top_hubs <- trip_clean %>% 
  filter(!StartHub == "") %>%
  select(StartHub, StartLatitude, StartLongitude) %>% 
  add_count(StartHub) %>% 
  distinct() %>% 
  top_n(148, n)

m <- ggmap(portland) + 
  geom_point(data = top_hubs, 
             aes(x = StartLongitude, y = StartLatitude, size = n),
             color = "white", fill = "#f94d1f", shape = 21) + 
  labs(title = "Starting Stations") + 
  theme_minimal() + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(family = "Verdana Bold", size = 18),
        plot.margin = margin(0,0,0,0, "cm"))

# Use awesome library(pathwork) to create one plot.
m + {l + t + plot_layout(ncol = 1, heights = c(4,4))}

leaflet(top_hubs) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircles(lng = ~StartLongitude, lat = ~StartLatitude, radius = ~n/50,
             color = "white", opacity = 1, fillColor = "#f94d1f",
             fillOpacity = 1, weight = 2)

               