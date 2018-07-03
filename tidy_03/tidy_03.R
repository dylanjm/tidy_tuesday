library(tidyverse)
library(readxl)
library(janitor)
library(ggthemes)

# Read in the data
world_deaths <- read_xlsx(here::here("data/global_mortality.xlsx")) 

# Get rid of % sign in all the column titles
names(world_deaths) <- names(world_deaths) %>% 
  gsub("\\s\\(%\\)", "", .)

world_deaths_long <- world_deaths %>% 
  gather(key = "cause", value = "percent", 4:35) %>% 
  mutate(percent = percent/100)

world_deaths_long %>% 
  filter(year == 2016, 
         country_code == "OWID_WRL") %>% 
  mutate(cause = fct_reorder(cause, percent, min)) %>% 
  ggplot(aes(x = cause, y = percent, fill = cause)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(y = percent, label = glue::glue("{round(percent*100,2)}%")),
            hjust = -0.18, size = 3, color = "grey30") + 
  scale_y_continuous(breaks = seq(0,.3,.05),
                     labels = scales::percent(seq(0,.3,.05)),
                     expand = c(0,0), limits = c(0,.35)) + 
  scale_fill_manual(values = colorRampPalette(solarized_pal()(8))(32)) + 
  coord_flip() + 
  labs(title = "Share of deaths by cause, World, 2016",
       subtitle = glue::glue("Data refers to the specific cause of death, which is ",
                             "distinguished from risk factors for death, such as ",
                             "air pollution, diet and other lifestyle factors.",
                             "\nThis is shown by cause of death as the percentage of total deaths."),
       caption = "Source: IHME, Global Burden of Disease") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.3), family = "Merriweather"),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 12, color = "grey50"),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major = element_line(color = "#d9d9d9", linetype = c("22")),
        plot.subtitle = element_text(color = "grey30", family = "Merriweather Light"),
        plot.caption = element_text(color = "grey30"))


world_deaths_long %>%
  filter(!country_code %in% ("OWID_WRL"),
         cause %in% unique(cause)[1:5]) %>% 
  ggplot(aes(x = year, y = percent, group = cause)) + 
  geom_point(alpha = .2, 
             color = "grey30") + 
  geom_smooth(method = "loess", se = FALSE, color = "gold") + 
  scale_y_continuous(labels = scales::percent) + 
  facet_wrap( ~ cause, ncol = 5) + 
  labs(title = "Top 5 Causes of Death, Worldwide, 1990-2016",
       subtitle = glue::glue("Trendline showing top five causes of death worldwide ",
                             "since 1990. \n Each point represents a country's mortality ",
                             "statistics for the given year."),
       caption = "Source: IHME, Global Burden of Disease") + 
  theme_minimal() + 
  theme(
    axis.title = element_blank(), 
    strip.text = element_text(family = "Helvetica", face = "bold"), 
    plot.caption  = element_text(color = "grey30")
  )

