library(tidyverse)
library(albersusa)

voter_dat <- read_csv(here::here("data/week28_voter_turnout.csv"))

votes_map <- voter_dat %>% 
  filter(year == 2016) %>% 
  mutate(pct_turnout = votes/eligible_voters) %>% 
  left_join(usa_sf(), by = c("state" = "name"))


ggplot(data = votes_map) + 
  geom_sf(aes(fill = pct_turnout))
