library(tidyverse)
library(ggrepel)
library(gganimate)

tv_dat <- read_csv(here::here("data/data_2019/week02_tv_ratings.csv"))

tv_shows <- c("The Sopranos", 
              "Twin Peaks", 
              "Sex and the City", 
              "The Wire", 
              "The X-Files", 
              "Breaking Bad", 
              "Game of Thrones")

tv_show_dat <- tv_dat %>% 
  filter(title %in% tv_shows) %>% 
  mutate(title = case_when(
    title == "Twin Peaks" & date > as.Date("2010-01-01") ~ "Twin Peaks\n(reboot)", 
    title == "The X-Files" & date > as.Date("2010-01-01") ~ "The X-Files\n(reboot)",
    TRUE ~ title
  ))

labs <- tv_show_dat %>% 
  group_by(title) %>% 
  filter(row_number(title) == 1)

tv_dat %>% 
  mutate(year = lubridate::year(date)) %>% 
  ggplot(aes(x = date, y = av_rating, size = share)) + 
  geom_point(color = "#d7ebf2", alpha = .8) + 
  geom_smooth(method = "lm", se = F, linetype = "dashed", 
              color = "skyblue4") + 
  annotate(geom = "text", x = as.Date("2015-01-01"), y = 8.2, 
            label = "TV drama trend", color = "skyblue4", 
           fontface = "bold", size = 4) + 
  geom_point(data = tv_show_dat, aes(x = date, y = av_rating, size = share),
             color = "#42bbd0") + 
  geom_line(data = tv_show_dat, aes(x = date, y = av_rating, group = title), 
            color = "#42bbd0", inherit.aes = FALSE) + 
  geom_text_repel(data = labs, aes(x = date, y = av_rating, label = title), 
                  color = "#42bbd0", inherit.aes = FALSE, nudge_y = -.15,
                  fontface = "bold", size = rel(6)) + 
  coord_cartesian(ylim = c(5.5, 9.5)) + 
  scale_y_continuous(breaks = seq(5.5, 9.5, .5), position = "right")+
  scale_x_date(breaks = as.Date(c("1990-01-01",
                                       "1995-01-01", "2000-01-01",
                                       "2005-01-01", "2010-01-01",
                                       "2015-01-01", "2018-01-01")), 
               date_labels = "%Y") +
  scale_size_continuous(range = c(2,10)) + 
  labs(title = "The end of channel surfing\nTV's golden age is real", 
       subtitle = "But for every Breaking Bad, more shows are just bad",
       caption = "*Seasons with at least 100 ratings on average\n*Size=Share of IMDb ratings for shows that year") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = .5, size = rel(2.5)),
        plot.subtitle = element_text(hjust = .5, face = "italic", size = rel(1.3)),
        plot.caption = element_text(face = "italic", color = "grey60", 
                                    size = rel(1)), 
        axis.title = element_blank(),
        axis.text = element_text(size = rel(1.2)),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(), 
        legend.position = "none")

genre_dat <- tv_dat %>% 
  mutate(genres = str_split(genres, pattern = ",")) %>% 
  unnest()

top_genres <- genre_dat %>% 
  count(genres) %>% 
  top_n(6, n) %>% 
  mutate(genres = fct_reorder(genres, n, max, .desc = T)) %>% 
  pull(genres)

genre_dat %>% 
  filter(genres %in% top_genres) %>%
  mutate(genres = fct_relevel(genres, c("Drama", "Crime", "Mystery",
                                        "Comedy", "Action", "Romance"))) %>% 
  ggplot(aes(x = seasonNumber, y = av_rating, size = share)) + 
  geom_point(color = "skyblue", alpha = .3) + 
  geom_smooth(se = F, color = "skyblue4") + 
  scale_x_continuous(breaks = seq(1, 10, 2), 
                     limits = c(1, 10)) + 
  scale_y_continuous(breaks = seq(2, 9.5, .5), 
                     limits = c(4, 10), 
                     position = "right") + 
  scale_size_continuous(range = c(3, 9)) + 
  facet_wrap(~ genres) + 
  labs(title = "Do Ratings Decline Over Time?",
       subtitle = "Most genres see a peak in ratings shortly after\na series begins and trends downward overtime.",
       x = "Seasons", y = "Avg. Rating") + 
  ggthemes::theme_fivethirtyeight() + 
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(), 
        axis.text = element_text(size = rel(1.3)),
        strip.text = element_text(face = "bold", size = 14),
        plot.title = element_text(hjust = .5, size = rel(3)),
        plot.subtitle = element_text(hjust = .5, size = rel(1.5))
        )


tv_dat %>% 
  mutate(year = lubridate::year(date)) %>% 
  ggplot(aes(x = share, y = av_rating, size = seasonNumber)) + 
  geom_point(alpha = .5) +
  geom_smooth(se = FALSE, color = "red", linetype = "dashed", size = 1.9) +
  scale_y_continuous(limits = c(4, 10)) + 
  scale_x_sqrt() + 
  scale_size_continuous(range = c(2, 10)) + 
  ggthemes::theme_fivethirtyeight() +
  labs(title = 'Year: {round(frame_time,0)}', x = 'Share', y = 'Avg. Rating', 
       size = "Season") +
  transition_time(year) +
  ease_aes('linear')
  
