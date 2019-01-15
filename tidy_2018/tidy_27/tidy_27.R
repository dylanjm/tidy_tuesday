library(tidyverse)
library(hrbrthemes)

birth_dat <- read_csv(here::here("data/data_2018/week27_us_births_2000-2014.csv"))
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

birth_dat %>% 
  group_by(year, month) %>% 
  summarise(total_births = sum(births)) %>% 
  mutate(month_abb = fct_relevel(month.abb[month], rev(month.abb))) %>%
  ggplot(aes(x = year, y = month_abb, fill = total_births/10^5)) +
  geom_tile(color = 'white') + 
  scale_x_continuous(breaks = 2000:2014, 
                     expand = c(0,0)) + 
  scale_fill_viridis_c(limits = c(2.9, 4.0), option = "B") + 
  labs(title = "Births in the Early Aughts",
       subtitle = "Birthrate in 100,000's",
       fill = "Births") +
  guides(fill = guide_colorbar(title.position="top", title.hjust = 0.5)) + 
  theme_ipsum() + 
  theme(plot.background = element_rect(fill = "grey20"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 14),
        text = element_text(color = "grey99"),
        axis.title = element_blank(),
        axis.text = element_text(color = "grey99"),
        plot.title = element_text(face = "bold", size = rel(2)),
        plot.caption = element_text(size = 6),
        plot.margin = margin(c(4,4,4,4)))

ggsave("tidy_2018/tidy_27/heatmap_plot.png", height = 12, width = 18)
