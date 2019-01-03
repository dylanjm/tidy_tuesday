library(tidyverse)

nfl_dat <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-08-28/nfl_2010-2017.csv")

nfl_dat %>% 
  group_by(team, game_year) %>% 
  summarise(rsh_yds = sum(rush_yds, na.rm = T)) %>% 
  ggplot(aes(x = game_year, y = rsh_yds)) + 
  geom_point()
