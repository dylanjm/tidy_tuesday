library(tidyverse)
library(ggthemes)
library(gghighlight)

url_base <- "https://raw.githubusercontent.com/fivethirtyeight/russian-troll-tweets/master/IRAhandle_tweets_"

troll_dat <- map(1:9, ~data.table::fread(glue::glue(url_base,.x,".csv")) %>% 
                   mutate(date_pub = lubridate::mdy_hm(publish_date))) %>% 
  map_df(bind_rows)

troll_dat <- troll_dat %>%
  mutate(day = lubridate::date(date_pub))

troll_dat %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) + 
  geom_bar(stat = "identity", color = "#eb7142") +
  scale_y_continuous(labels = c(0, 5, 10, "15k"),
                     breaks = seq(0, 15000, 5000)) + 
  scale_x_date(limits = c(as.Date("2015-06-01"),
                          as.Date("2017-12-31"))) + 
  labs(title = "Russian troll tweets by day",
       subtitle = "Nearly 3 million tweets sent by trolls associated with the Internet Research Agency") + 
  theme_fivethirtyeight() + 
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())

troll_dat %>%
  count(day, account_category) %>%
  filter(account_category %in% c("LeftTroll", "RightTroll", 
                                 "NewsFeed", "HashtagGamer")) %>% 
  mutate(account_category = fct_relevel(account_category, c("LeftTroll",
                                                            "RightTroll",
                                                            "NewsFeed","HashtagGamer"))) %>% 
  ggplot(aes(x = day, y = n, fill = account_category)) + 
  geom_col(width = 5) +
  gghighlight(unhighlighted_colour = "grey85") + 
  scale_y_continuous(labels = c(0, 5, 10, "15k"),
                     breaks = seq(0, 15000, 5000)) + 
  scale_x_date(limits = c(as.Date("2015-06-01"),
                          as.Date("2017-12-31"))) +
  scale_fill_manual(values = c("#2391d0", "#f92e2a",
                               "#47c1c8", "#c37dc4")) + 
  facet_wrap(~account_category) + 
  labs(title = "Not all trolls are the same",
       subtitle = "Tweets sent by trolls associated with the Internet Research Agency, as categorized by Clemson Researchers") + 
  theme_fivethirtyeight() + 
  theme(legend.position = "none", 
        strip.text = element_blank())
