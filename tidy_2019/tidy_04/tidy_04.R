# Load Library
library(tidyverse)
library(USAboundaries)

raw_dat <- read_csv(here::here("data/data_2019/week04_incarceration_trends.csv"))
pretrial_dat <- read_csv(here::here("data/data_2019/week04_pretrial_summary.csv"))
prison_pop <- read_csv(here::here("data/data_2019/week04_prison_population.csv"))


plot_1_colors <- ggthemes::ggthemes_data[["fivethirtyeight"]] %>% 
  filter(name %in% c("Red", "Blue", "Green")) %>% 
  pull(value)

label_dat <- pretrial_dat %>% 
  group_by(urbanicity) %>% 
  filter(row_number() == n()) %>% 
  ungroup() %>% 
  mutate(lab_x = c(2017, 2017, 2017, 2016.5),
         lab_y = c(255, 252, 174, 173))

pretrial_dat %>% 
  filter(pop_category == "Total") %>% 
  ggplot(aes(x = year, y = rate_per_100000, 
             color = urbanicity, group = urbanicity)) + 
  geom_line(size = 4/.pt) + 
  ggrepel::geom_text_repel(data = label_dat,
                           aes(label = tools::toTitleCase(urbanicity),
                               x = lab_x, y = lab_y),
                           size = 14/.pt, fontface = "bold") + 
  scale_x_continuous(breaks = c(seq(1970, 2010, 10), 2016),
                     limits = c(1970, 2019)) +
  scale_y_continuous(breaks = seq(0, 300, 100), 
                     limits = c(0, 300)) + 
  scale_color_manual(values = c("Gold3", plot_1_colors)) +
  labs(title = "Pretrial Incarceration by Urban-Rural Counties", 
       y = expression(Rate["(per 100,000 population)"])) +
  theme_classic() + 
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 16,
                                    family = "IBMPlexSans-Light"),
        axis.text = element_text(size = 12,
                                 family = "IBMPlexSans-Light"),
        plot.title = element_text(size = 18,
                                  family = "IBMPlexSans-Light"),
        legend.position = "none", 
        plot.background = element_rect(fill = "grey95"),
        panel.background = element_rect(fill = "grey95"),
        aspect.ratio = .65)

ggsave(here::here("tidy_2019/tidy_04/plot_01.png"), width = 10, height = 6)

test <- raw_dat %>% 
  filter(state == "UT", 
         year == "2016") %>% 
  gather(key = "jail_pop", value = "pct_pop", 
         asian_jail_pop:white_jail_pop) %>% 
  gather(key = "race_pop", value = "race_pct_pop", 
         asian_pop_15to64:white_pop_15to64) %>% 
  select(1:7, jail_pop, pct_pop, race_pop, race_pct_pop, total_prison_pop)

test_clean <- test %>% 
  mutate(race_jail_ratio = pct_pop/total_prison_pop,
         race_pop_ratio = race_pct_pop/total_pop_15to64,
         fips = str_pad(fips, 5, pad = "0")) %>% 
  left_join(us_counties(state = "UT"), by = c("fips" = "geoid")) %>% 
  sf::st_as_sf()

ggplot(test_clean) +
  geom_sf(aes(fill = race_jail_ratio)) +
  theme_map()
  
