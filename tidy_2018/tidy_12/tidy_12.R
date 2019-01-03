library(tidyverse)
library(lubridate)
library(janitor)

mc_hurricanes <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week12_mediacloud_hurricanes.csv")
mc_states <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week12_mediacloud_states.csv")


labels_list <- c("Hurricane Harvey","Hurricane Irma",
                 "Hurricane Maria","Texas", "Florida","Puerto Rico")

mc_data <- mc_hurricanes %>% 
  rename_at(vars(Harvey:Jose), funs(paste("Hurricane", .))) %>% 
  left_join(mc_states, by = "Date") %>% 
  gather(key = "type", value = "sentences", 2:8) %>% 
  filter(type != "Hurricane Jose") %>% 
  mutate(Date = mdy(Date),
         type = str_remove_all(type, "\""),
         type = fct_relevel(type, labels_list),
         set = case_when(
           type %in% labels_list[1:3] ~ 0,
           TRUE ~ 1),
         fill = case_when(
           type %in% c("Texas", "Hurricane Harvey") ~ "#eb7244",
           type %in% c("Florida","Hurricane Irma") ~ "#f95586",
           TRUE ~ "#47c0c8")
         )

plot_labels <- mc_data %>% 
  mutate(type = str_replace_all(type, " ", "\n")) %>%
  group_by(type) %>% 
  filter(sentences == max(sentences))

ggplot(mc_data) +
  geom_ribbon(aes(x = Date, 
                  ymin = 0, ymax = sentences, 
                  fill = fill, 
                  group = type),
              color = "white") + 
  geom_hline(yintercept = 0) +
  geom_text(data = plot_labels, aes(x = Date-6, y = sentences, label = type,
                                    hjust = .5, vjust = -.5, fontface = "bold",
                                    size = 8)) + 
  scale_y_continuous(labels = scales::comma(seq(0,4000,1000)),
                     breaks = seq(0,4000,1000),
                     limits = c(0,5072)) + 
  scale_x_date(labels = c("AUG. 20, 2017", "AUG. 27", "SEPT. 3", "SEPT. 10", "SEPT. 17"),
               breaks = seq(ymd('2017-08-20'),ymd('2017-09-17'), by = '1 week'),
               limits = c(ymd('2017-08-20'), ymd('2017-09-22'))) + 
  scale_fill_identity() +  
  facet_wrap(~ set) + 
  labs(title = "Hurricane Maria and Puerto Rico got comparatively little online coverage",
       subtitle = "Number of sentences mentioning each hurricane and the place it made landfall,\namong outlets in Media Cloud's 'US Top Online News' collection.",
       y = "Share of sentences per day",
       caption = "Source: Media Cloud") + 
  theme_minimal() + 
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", margin = margin(r = 25)),
        axis.text.y = element_text(margin = margin(r = 20), size = 10),
        strip.text = element_blank(),
        plot.title = element_text(hjust = .5, face = "bold"),
        plot.subtitle = element_text(hjust = .5),
        panel.spacing = unit(2, "lines"))


tv <- tv_hurricanes %>% 
  select(-Jose) %>% 
  gather(key = "type", value = "pct", 2:4) %>% 
  mutate(Date = mdy(Date))

ggplot(tv) +
  geom_ribbon(aes(x = Date, 
                  ymin = 0, ymax = pct, 
                  fill = type, 
                  group = type),
              color = "white") + 
  geom_hline(yintercept = 0) +
  facet_grid()
  scale_x_date(labels = c("AUG. 20, 2017", "AUG. 27", "SEPT. 3", "SEPT. 10", "SEPT. 17", "SEPT. 24"),
               breaks = seq(ymd('2017-08-20'),ymd('2017-09-24'), by = '1 week'),
               limits = c(ymd('2017-08-20'), ymd('2017-09-26'))) + 
  scale_fill_manual(values = rep(c("#eb7244", "#f95586", "#47c0c8"),2)) + 
  labs(title = "Hurricane Maria and Puerto Rico got comparatively little online coverage",
       subtitle = "Number of sentences mentioning each hurricane and the place it made landfall,\namong outlets in Media Cloud's 'US Top Online News' collection.",
       y = "Share of sentences per day",
       caption = "Source: Media Cloud") + 
  theme_minimal() + 
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", margin = margin(r = 25)),
        axis.text.y = element_text(margin = margin(r = 20), size = 10),
        axis.text = element_text(family = "Calling Code Regular"),
        strip.text = element_blank(),
        plot.title = element_text(hjust = .5, face = "bold"),
        plot.subtitle = element_text(hjust = .5),
        panel.spacing = unit(2, "lines"))

google_trends %>% 
  select(-jose) %>% 
  gather(key = "search", value = "count", 2:4) %>% 
  ggplot() + 
  geom_ribbon(aes(x = date, ymin = 0, ymax = count, fill = search),
              color = "white") + 
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = c(0,"","","","","MAX"),
                     breaks = c(0,20,40,60,80,100)) + 
  scale_x_date(labels = c("AUG. 20, 2017", "AUG. 27", "SEPT. 3", "SEPT. 10", "SEPT. 17"),
               breaks = seq(ymd('2017-08-20'),ymd('2017-09-17'), by = '1 week'),
               limits = c(ymd('2017-08-20'), ymd('2017-09-22'))) + 
  scale_fill_manual(values = c("#eb7244", "#f95586", "#47c0c8")) + 
  labs(title = "Google search trends in the U.S. (excluding Puerto Rico)",
       caption = "Source: Google Trends") + 
  theme_minimal() + 
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Calling Code Regular", color = "grey50",
                                 size = 10),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(family = "Roboto Condensed", color = "grey50"))

tv_channel %>% 
  filter(Query != "Hurricane Jose") %>% 
  gather(key = "channel", value = "pct", 3:6) %>% 
  ggplot() + 
  geom_ribbon(aes(x = Date, ymin = 0, ymax = pct, group = Query, fill = Query)) + 
  facet_grid(channel ~ Query)


# mc_top_online <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week12_mediacloud_top_online_news.csv")
# mc_trump <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week12_mediacloud_trump.csv")
# tv_hurricanes <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week12_tv_hurricanes.csv")
# 
# tv_channel <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/puerto-rico-media/tv_hurricanes_by_network.csv")
# tv_states <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/puerto-rico-media/tv_states.csv")


# google_trends <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week12_google_trends.csv",
#                           col_names = c("date", "harvey","irma","maria","jose"), skip = 3)