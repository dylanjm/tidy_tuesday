library(tidyverse)
library(readxl)
library(janitor)
library(ggrepel)

mortality_raw <- read_xlsx(here::here("data/global_mortality.xlsx"))

mortality <- mortality_raw %>% 
  clean_names() %>% 
  gather(key = "cause", value = "percent", 4:35) %>% 
  mutate(cause = str_remove(cause, "_percent"), 
         percent = percent/100)

usa_deaths <- mortality %>% 
  filter(cause %in% c("cancers", "cardiovascular_diseases"),
         country_code %in% "USA") %>% 
  mutate(color = "firebrick2") %>% 
  spread(cause, percent)

mortality %>% 
  filter(cause %in% c("cancers", "cardiovascular_diseases"),
         !country_code %in% ("OWID_WRL")) %>% 
  spread(cause, percent) %>% 
  ggplot(aes(x = cancers, y = cardiovascular_diseases, group = country)) + 
  geom_path(alpha = .13) + 
  geom_path(data = usa_deaths, aes(color = color)) + 
  geom_label_repel(data = filter(usa_deaths, year == 2016), 
                   aes(label = country)) + 
  scale_color_identity() +
  scale_y_continuous(labels = scales::percent_format()) + 
  scale_x_continuous(labels = scales::percent_format()) + 
  labs(title = "Percent of Deaths due to Cancer vs. Cardiovascular Diseases", 
       x = "Percent of Deaths due to Cancers", y = "Percent of Deaths due to Cardiovascular Diseases",
       subtitle = "Data collected worldwide from 1990-2016", 
       caption = "Data Source: IHME, Global Burden of Disease") +
  theme_minimal() + 
  theme(plot.title = element_text(size = rel(1.3), 
                                  family = "Merriweather",
                                  face = "bold"),
        plot.subtitle = element_text(size = rel(.9),
                                     family = "Merriweather Light"),
        plot.caption = element_text(family = "Merriweather Light",
                                    size = rel(.8)),
        axis.title = element_text(size = rel(.9),
                                  family = "Merriweather"),
        axis.text = element_text(family = "Merriweather Light",
                                 face = "italic"))
