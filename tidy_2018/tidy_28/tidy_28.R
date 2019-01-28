library(tidyverse)
library(USAboundaries)
library(gganimate)

voter_dat <- read_csv(here::here("data/data_2018/week28_voter_turnout.csv"))

oc_voter <- read_csv(here::here("data/data_2018/week28_official_voter_turnout.csv"))

state_choice <- state.name[!state.name %in% c("Alaska", "Hawaii")]

votes_map <- voter_dat %>% 
  mutate(pct_turnout = votes/eligible_voters) %>% 
  left_join(us_states(state = state_choice), by = c("state" = "name")) %>% 
  filter(!is.na(state_abbr)) %>% 
  sf::st_as_sf()


ggplot(data = votes_map) + 
  geom_sf(aes(fill = pct_turnout)) +
  labs(title = 'Year: {frame_time}') +
  transition_time(year) +
  ease_aes('linear')
