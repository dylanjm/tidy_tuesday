library(tidyverse)
library(USAboundaries)

voter_dat <- read_csv(here::here("data/data_2018/week28_voter_turnout.csv"))

votes_map <- voter_dat %>% 
  filter(year == 2016) %>% 
  mutate(pct_turnout = votes/eligible_voters) %>% 
  left_join(us_states(), by = c("state" = "name"))


ggplot(data = votes_map) + 
  geom_sf(aes(fill = pct_turnout))
