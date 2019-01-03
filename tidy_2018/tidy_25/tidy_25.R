library(tidyverse)

hypoxia_dat <- read_csv(here::here("data/week25_hypoxia.csv"))

airports_dat <- read_csv(here::here("data/week25_us-airports.csv"))

airports_dat %>% 
  group_by(loc_id) %>% 
  summarise(total_passengers = sum(passengers)) %>% 
  top_n(16, total_passengers) %>% 
  mutate(loc_id = fct_reorder(loc_id, total_passengers)) %>% 
  ggplot(aes(x = loc_id, y = total_passengers)) + 
  geom_bar(stat = "identity") + 
  coord_flip()

airports_dat %>% 
  group_by(year) %>% 
  summarise(yearly_passengers = sum(passengers, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = yearly_passengers)) + 
  geom_bar(stat = "identity")
