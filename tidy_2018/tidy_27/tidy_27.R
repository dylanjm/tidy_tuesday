library(tidyverse)

birth_dat <- read_csv(here::here("data/week27_us_births_2000-2014.csv"))
View(birth_dat)

t <- birth_dat %>% 
  group_by(day_of_week, date_of_month) %>% 
  summarise(n_births = sum(births)) %>% 
  filter(date_of_month == 13)

d <- birth_dat %>% 
  filter(date_of_month %in% c(6,20)) %>% 
  group_by(day_of_week, date_of_month) %>% 
  summarise(mean_births = mean(births)) %>% 
  ungroup() %>% 
  group_by(day_of_week) %>% 
  summarise(mean_births = sum(mean_births))

t$n_births-d$mean_births
