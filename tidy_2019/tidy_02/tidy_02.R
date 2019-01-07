library(tidyverse)
library(ggrepel)

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
