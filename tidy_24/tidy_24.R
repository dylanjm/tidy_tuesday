library(tidyverse)
library(albersusa)
library(scico)

dg_dat <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-09-11/cats_vs_dogs.csv")

dg_usa <- dg_dat %>% 
  left_join(usa_sf(proj = "laea"), by = c("state" = "name")) %>% 
  mutate(dog_cat_ratio = dog_population/(dog_population + cat_population))
  
ggplot(data = dg_usa) + 
  geom_sf(aes(fill = dog_cat_ratio),
          color = "white") + 
  scale_fill_scico(palette = "lajolla",
                   breaks = c(1/2.75, 1/2.25, 0.5, 1.25/2.25),
                   labels = c("1.75x more cats", "1.25x more cats", 
                              "1:1 cats and dogs", "1.25x more dogs")) + 
  labs(title = "Cats and Dogs in the USA",
       subtitle = "More cats in northern states, more dogs in southern states",
       fill = "Dog to Cat Ratio") + 
  hrbrthemes::theme_ipsum() + 
  theme(axis.text = element_blank(),
        legend.title = element_text(face = "bold", size = 14),
        legend.key.size = grid::unit(1.3, "cm"), 
        legend.text = element_text(size = 12))
