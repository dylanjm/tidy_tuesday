library(tidyverse)
library(ggthemes)


airline_dat <- read_csv(here::here("data/week19_airline_safety.csv")) %>% 
  select(-X1)

airline_dat %>% 
  mutate(type_of_event = fct_relevel(type_of_event,
                                     c("incidents", "fatal_accidents", "fatalities"))) %>% 
  ggplot(aes(x = type_of_event, y = airline)) +
  geom_tile(aes(fill = n_events, group = airline)) + 
  geom_text(aes(label = n_events)) + 
  scale_x_discrete(position = "top") + 
  scale_fill_gradientn(colors = c("green", "yellow", "red")) + 
  facet_wrap(~year_range) + 
  labs(title = "Incidents, Fatal Accidents and Fatalities",
       subtitle = "Per trillion available seat kilometers\n*Includes regional subsidaries") + 
  theme_fivethirtyeight() + 
  theme(legend.position = "none")

airline_dat %>% 
  spread(key = year_range, value = n_events) %>% 
  replace_na(list(`00_14` = 0, `85_99` = 0)) %>%
  ggplot(aes(x = `85_99`, y = `00_14`)) + 
  geom_point()
