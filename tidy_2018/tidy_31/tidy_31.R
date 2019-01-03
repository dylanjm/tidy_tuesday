library(tidyverse)

# read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-30/r-downloads.csv") %>% 
#   write_csv(here::here("data/week31_r-downloads.csv"))
# 
# read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-30/r_downloads_year.csv") %>% 
#   write_csv(here::here("data/week31_r-downloads_year.csv"))

r_downloads <- read_csv(here::here("data/week31_r-downloads.csv"))

r_downloads_year <- read_csv(here::here("data/week31_r-downloads_year.csv"))

r_downloads %>% 
  ggplot(aes(x = fct_reorder(os, os, length, .desc = TRUE))) + 
  geom_bar() + 
  scale_x_discrete(labels = c("Windows", "macOS", "Src", "Not Available")) + 
  ggthemes::theme_fivethirtyeight()

r_downloads_year %>% 
  ggplot(aes(x = fct_reorder(os, os, length, .desc = TRUE))) + 
  geom_bar() + 
  scale_x_discrete(labels = c("Windows", "macOS", "Src", "Not Available")) + 
  scale_y_continuous(labels = scales::comma) + 
  ggthemes::theme_fivethirtyeight()
