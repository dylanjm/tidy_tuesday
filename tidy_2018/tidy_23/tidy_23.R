library(tidyverse)
library(broom)


food_dat <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-09-04/fastfood_calories.csv")

mean_cal <- food_dat %>% 
  group_by(restaurant) %>% 
  summarise(mean_cal = mean(calories)) %>% 
  mutate(restaurant = fct_reorder(restaurant, mean_cal, max, .desc = T))

ggplot(mean_cal, aes(x = restaurant, y = mean_cal, fill = restaurant)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = round(mean_cal,0)),
            position = position_nudge(y = 10)) + 
  ggpomological::scale_fill_pomological() + 
  labs(title = "Average Number of Calories per Menu Item") + 
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(), 
        panel.background = element_rect(fill = "#e9fbfe", color = NA),
        plot.background = element_rect(fill = "#e9fbfe"),
        legend.position = "none")

food_dat %>% 
  nest(-restaurant) %>% 
  mutate(fit = map(data, ~lm(calories ~ sodium, data = .)), 
         results = map(fit, augment)) %>% 
  unnest(results) %>% 
  ggplot(aes(x = calories, y = .fitted)) +
  geom_abline(intercept = 0, slope = 1, alpha = .2) +  # Line of perfect fit
  geom_point() +
  facet_grid(restaurant ~ .) +
  labs(x = "calories", y = "Predicted Value") +
  theme_bw()
