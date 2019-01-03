library(tidyverse)
library(rworldmap)
library(ggthemes)
library(scico)
library(hrbrthemes)
library(mapproj)
library(animation)
library(tweenr)
library(gganimate)

# Static Globe Plot
life_dat <- read_csv(here::here("data/week14_global_life_expectancy.csv")) %>% 
  filter(year == 2013) %>% 
  mutate(country = case_when(
    country == "United States" ~ "United States of America",
    country == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    country == "Congo" ~ "Republic of the Congo",
    country == "Cote d'Ivoire" ~ "Ivory Coast",
    country == "Serbia (including Kosovo)" ~ "Republic of Serbia",
    country == "Tanzania" ~ "United Republic of Tanzania",
    TRUE ~ country
  ))

map_world <- broom::tidy(spTransform(getMap(), CRS("+proj=robin"))) %>% 
  filter(id != "Antarctica")

ggplot() +
  geom_map(data = map_world, map = map_world,
           aes(x = long, y = lat, map_id = id),
           fill = "grey50") + 
  geom_map(data = life_dat, map = map_world,
           aes(fill = life_expectancy, map_id = country)) + 
  scale_fill_scico(palette = "vik", na.value = "grey50") +
  labs(title = "Life expectancy, 2013",
       subtitle = "Shown in the period of life expectancy at birth. This corresponds to the average estimate a newborn\ninfant would live if prevailing patterns of mortality at the time of birth were to stay the same throughout its life.",
       caption = "Source: Clio-Infra estimates until 1949; UN Population Division from 1950 to 2015",
       fill = "Life\nExpectancy") + 
  theme_ipsum() + 
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(), 
        panel.background = element_rect(fill = "grey95", color = NA), 
        plot.background = element_rect(fill = "grey95", color = NA))

# Global Death vs Life Expectancy Plot
global_death <- readxl::read_xlsx(here::here("data/global_mortality.xlsx")) %>% 
  gather(key = "cause", value = "percent", 4:35)

big_dat <- left_join(life_dat, global_death, by = c("code" = "country_code", "year" = "year"))

test <- big_dat %>% 
  filter(!is.na(code),
         cause %in% c("Cardiovascular diseases (%)", "Cancers (%)", 
                      "Respiratory diseases (%)", "Diabetes (%)", "Dementia (%)")) %>% 
  mutate(cause = fct_relevel(cause, c("Cardiovascular diseases (%)", "Cancers (%)", 
                                      "Respiratory diseases (%)", "Diabetes (%)", "Dementia (%)")))

ggplot(test, aes(x = percent/100, y = life_expectancy)) + 
  geom_point(alpha = .5) + 
  geom_smooth(aes(color = cause)) + 
  scale_x_percent() + 
  scale_color_viridis_d() +
  facet_wrap(~ cause, ncol = 2, scales = "free") + 
  labs(title = "Percent of Deaths to Life Expectancy Worldwide",
       y = "Life Expectancy", x = "Percent") +
  guides(color = FALSE) + 
  theme_ipsum()

lm(life_expectancy ~ I(percent^2/100) + cause, data = test)

# Global Animation Plot
life_dat_ani <- read_csv(here::here("data/week14_global_life_expectancy.csv")) %>% 
  mutate(country = case_when(
    country == "United States" ~ "United States of America",
    country == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    country == "Congo" ~ "Republic of the Congo",
    country == "Cote d'Ivoire" ~ "Ivory Coast",
    country == "Serbia (including Kosovo)" ~ "Republic of Serbia",
    country == "Tanzania" ~ "United Republic of Tanzania",
    TRUE ~ country
  ))

ani_filter_year <- function(y = 1950){
  life_dat_ani %>% 
    filter(year == y)
}

ani_life_list <- purrr::map(seq(1950,2015,1), ani_filter_year)

tweenr_df <-tween_states(ani_life_list, 
                        tweenlength = 3,
                        statelength = 5, 
                        ease = rep('cubic-in-out', 65),
                        nframes=265)

g <- ggplot() +
  geom_map(data = map_world, map = map_world,
           aes(x = long, y = lat, map_id = id),
           fill = "grey50") + 
  geom_map(data = tweenr_df, map = map_world,
           aes(frame = round(year,0), fill = life_expectancy, map_id = country)) + 
  scale_fill_scico(palette = "vik", na.value = "grey50") +
  labs(title = "Life expectancy,", 
       subtitle = "Shown in the period of life expectancy at birth. This corresponds to the average estimate a newborn\ninfant would live if prevailing patterns of mortality at the time of birth were to stay the same throughout its life.",
       caption = "Source: Clio-Infra estimates until 1949; UN Population Division from 1950 to 2015",
       fill = "Life\nExpectancy") + 
  theme_ipsum() + 
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(), 
        panel.background = element_rect(fill = "grey95", color = NA), 
        plot.background = element_rect(fill = "grey95", color = NA))


animation::ani.options(interval = 1/8)
gganimate(g, "life_expectancy.gif", title_frame = T, ani.width = 1200, 
          ani.height = 1000)






























