library(tidyverse)
library(hrbrthemes)
library(patchwork)

r_tweets <- read_rds(here::here("data/data_2019/week01_rstats_tweets.rds"))
tt_tweets <- read_rds(here::here("data/data_2019/week01_tidy_tuesday_tweets.rds"))

(top_users <- tt_tweets %>% 
  count(screen_name) %>% 
  top_n(10, n) %>% 
  ggplot(aes(x = fct_reorder(screen_name, n, max), y = n)) + 
  geom_col() + 
  geom_text(aes(label = scales::comma(n)), 
            position = position_nudge(y = 8),
            fontface = "italic", 
            family = font_ps, 
            size = 6) + 
  coord_flip() + 
  scale_y_continuous(labels = scales::comma, 
                     expand = c(0,0), 
                     limits = c(0, 200)) + 
  labs(title = "Top Ten Users\nTweeting About #tidy_tuesday", 
       y = "Number of Tweets", x = "Screen Name") + 
  theme_ipsum_ps() + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(linetype = "dashed"), 
        axis.title.x = element_text(size = rel(2.2)),
        axis.title.y = element_text(size = rel(2.2)),
        plot.title = element_text(size = rel(3)))
)

(density_plot <- tt_tweets %>% 
  ggplot(aes(x = favorite_count)) +
  geom_density(fill = 'grey') +
  scale_x_sqrt(labels = c(0, 10, 100, 200, 300, 400, 500), 
               breaks = c(0, 10, 100, 200, 300, 400, 500)) +
  labs(title = "Density of Favorites\nAmong #tidy_tuesday", 
       x = "Count Favorites", y = "Density") + 
  theme_ipsum_ps() + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(linetype = "dashed"), 
        axis.title.x = element_text(size = rel(2.2)),
        axis.title.y = element_text(size = rel(2.2)),
        plot.title = element_text(size = rel(3)))
)
  
(time_plot <- tt_tweets %>% 
  mutate(round_date = lubridate::floor_date(created_at, "day")) %>%
  count(round_date) %>% 
  ggplot(aes(x = round_date, y = n)) + 
  geom_point(alpha = .5, size = 3) + 
  geom_smooth(se = FALSE, color = "gold", size = 1.5) + 
  scale_y_continuous(expand = c(0,0)) + 
  labs(title = "#tidy_tuesday Tweets per Day since Inception",
       x = "Date", y = "Count") + 
  theme_ipsum_ps() + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(linetype = "dashed"), 
        axis.title.x = element_text(size = rel(2.2)),
        axis.title.y = element_text(size = rel(2.2)),
        plot.title = element_text(size = rel(3)), 
        axis.text.x = element_text(size = rel(2)),
        axis.text.y = element_text(size = rel(2)))
)


top_users + density_plot - time_plot + plot_layout(ncol = 1) 
