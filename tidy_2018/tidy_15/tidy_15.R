library(tidyverse)
library(readxl)
library(albersusa)
library(cartography)
library(sf)
library(tidytext)
library(ggrepel)

dat_url <- "https://github.com/rfordatascience/tidytuesday/blob/master/data/week15_beers.xlsx?raw=true"
temp <- tempfile()
downloader::download(dat_url, temp, mode = "wb")
beer_dat <- read_xlsx(temp, sheet = 1)
brewery_dat <- read_xlsx(temp, sheet = 2)

test <- brewery_dat %>% 
  count(state) %>% 
  left_join(usa_sf(), by = c("state" = "iso_3166_2")) %>% 
  sf::st_as_sf() %>% 
  mutate(
    CENTROID = map(geometry, st_centroid),
    COORDS = map(CENTROID, st_coordinates),
    COORDS_X = map_dbl(COORDS, 1),
    COORDS_Y = map_dbl(COORDS, 2)
  ) %>%
  as_tibble() %>%
  st_as_sf()

ggplot(test) + 
  geom_sf(aes(fill = n)) + 
  geom_text_repel(aes(x = COORDS_X, y = COORDS_Y, label = name)) + 
  scale_fill_gradientn(colors = carto.pal("orange.pal"))

