library(tidyverse)


grad_dat <- read_csv(here::here("data/data_2018/week29_recent_grads.csv"))

grad_dat %>% 
  group_by(Major) %>% 
  top_n(5, wt = Median) %>% 
  ggplot(aes(x = Unemployment_rate, y = Median, size = Sample_size,
             color = Major)) + 
  geom_point()
