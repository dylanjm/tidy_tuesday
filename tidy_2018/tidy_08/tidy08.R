library(tidyverse)
library(albersusa)
library(wesanderson)
library(ggalt)

honey_raw <- read_csv(here::here("data/honeyproduction.csv"))

us <- usa_composite()
us_map <- broom::tidy(us, region = "iso_3166_2")

ggplot() + 
  geom_map(data = us_map, map = us_map,
           aes(x = long, y = lat, map_id = id),
           color = "black", fill = NA) + 
  geom_map(data = honey_raw, map = us_map,
           aes(fill = totalprod/1000000, map_id = state)) + 
  scale_fill_gradientn(colors = wes_palette("Rushmore1"),
                       labels = scales::comma) +
  facet_wrap(~ year) +
  coord_proj(us_laea_proj) + 
  labs(title = "Total Production of Bee-Honey has Decreased since the Late Nineties",
       subtitle = "Honey production saw sharp declines for the years 1998-2012, with some states decreasing to only one producer (unpublished data).",
       caption = "Source: USDA & Kaggle.com",
       fill = "Production\n(Millions)") +
  theme_minimal() + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = c(.89,.11),
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(family = "Helvetica Bold Oblique"),
        plot.caption = element_text(family = "Helvetica Light Oblique"),
        plot.subtitle = element_text(family = "Helvetica Light Oblique"),
        plot.background = element_rect(fill = "grey95"),
        panel.grid = element_line(color = "grey85"))

ggplot() + 
  geom_map(data = us_map, map = us_map,
           aes(x = long, y = lat, map_id = id),
           color = "black", fill = NA) + 
  geom_map(data = honey_raw, map = us_map,
           aes(fill = priceperlb, map_id = state)) + 
  scale_fill_gradientn(colors = wes_palette("Zissou1"),
                       labels = scales::dollar) +
  facet_wrap(~ year) +
  coord_proj(us_laea_proj) + 
  labs(title = "Price of Honey has Sky Rocketed due to Supply Shocks",
       subtitle = "The price per pound of honey has increased dramatically since US production has slowed in recent years",
       caption = "Source: USDA & Kaggle.com",
       fill = "Average\nPrice/Lbs") +
  theme_minimal() + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = c(.89,.11),
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(family = "Helvetica Bold Oblique"),
        plot.caption = element_text(family = "Helvetica Light Oblique"),
        plot.subtitle = element_text(family = "Helvetica Light Oblique"),
        plot.background = element_rect(fill = "grey95"),
        panel.grid = element_line(color = "grey85"))
