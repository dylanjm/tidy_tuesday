library(tidyverse)
library(lubridate)
library(ggmap)

trip_dat <- list.files(here::here("data/PublicTripData"), 
           pattern = "*.csv", full.names = TRUE) %>% 
  map_df(~ read_csv(.x, col_types = "iccddcccddcccicdccl"))

trip_clean <- trip_dat %>% 
  mutate(Start = parse_date_time(glue::glue("{StartDate} {StartTime}"), "mdY HM"),
         End = parse_date_time(glue::glue("{EndDate} {EndTime}"), "mdY HM"),
         Duration = parse_time(Duration))

trip_clean %>% 
  filter(!is.na(Start)) %>% 
  mutate(floor_time = as.character(hour(floor_date(Start, "hours"))),
         floor_time = fct_relevel(floor_time, rev(c(as.character(5:23), as.character(0:4)))),
         weekday = wday(Start, label = TRUE), 
         weekday = fct_relevel(weekday, c("Mon", "Tue", "Wed", "Thu", "Fri",
                                          "Sat", "Sun"))) %>% 
  group_by(weekday, floor_time) %>% 
  tally() %>% 
  filter(!floor_time %in% c("1","2","3","4")) %>% 
  ggplot(aes(x = weekday, y = floor_time, fill = n)) + 
  geom_tile() + 
  annotate("segment", y = 21, yend = 21, 
           x = -Inf, xend = Inf,
           color = "black", size = .3) +
  annotate("segment", y = 0, yend = 0, 
           x = -Inf, xend = Inf,
           color = "black", size = .3) +
  scale_x_discrete(position = "top") + 
  scale_y_discrete(labels = c("12 AM", glue::glue("{c(11:1, 12)} PM"), glue::glue("{11:5} AM"))) +
  scale_fill_gradient(low = "white", high = "#f94d1f") + 
  labs(title = "Trips per Weekday/Hour") + 
  guides(fill = FALSE) + 
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_text(margin = margin(r = 30)))

trip_clean %>% 
  filter(!is.na(Start)) %>%
  mutate(floor_week = floor_date(Start, "weeks"),
         floor_day = floor_date(Start, "days")) %>% 
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
        axis.title = element_blank())

get_portland <- get_stamenmap(bbox = c(left = -122.7682, bottom = 45.4840, right = -122.5401, top = 45.5825), 
                              zoom = 13)

map_dat <- trip_clean %>% 
  add_count(StartHub) %>%
  filter(!is.na(StartHub)) %>% 
  select(StartHub, StartLatitude, StartLongitude) %>% 
  add_count(StartHub) %>% 
  distinct() %>% 
  top_n(148, n) 

ggmap(get_portland) +
  geom_point(data = map_dat, aes(x = StartLongitude, y = StartLatitude, size = n),
            color = "white", fill = "#f94d1f", shape = 21)
