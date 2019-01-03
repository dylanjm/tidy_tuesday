library(tidyverse)
library(ggthemes)

movie_titles <- read_lines(here::here("data/star_wars.csv")) %>% 
  str_split(",") %>% 
  {.[[3]][4:9]}

movie_characters <- read_lines(here::here("data/star_wars.csv")) %>% 
  str_split(",") %>% 
  {.[[2]][16:29]}

clean_dat <- read_csv(here::here("data/star_wars.csv"), 
                      col_names = FALSE) %>% 
  mutate_all(str_remove, "\x8c\xe6") %>%
  mutate(seen_films = rowSums(!is.na(.[4:9]))) %>% 
  filter(X2 == "Yes", seen_films == 6) %>% 
  select(-(X4:X9)) %>% 
  rename_at(vars(X10:X15), funs(paste(movie_titles))) %>%
  rename_at(vars(X16:X29), funs(paste(movie_characters))) %>%
  gather("movie", "rank", 4:9) %>% 
  gather("character", "favorite", 4:17)

movie_ranking <- clean_dat %>% 
  filter(rank == 1) %>% 
  group_by(movie) %>% 
  summarise(count = n()) %>% 
  mutate(movie = trimws(str_remove(movie, "Star Wars: Episode\\s.{3}")),
         movie = recode(movie, `he Empire Strikes Back` = "The Empire Strikes Back"),
         movie = fct_relevel(movie, c("Return of the Jedi", 
                                      "The Empire Strikes Back", 
                                      "A New Hope", 
                                      "Revenge of the Sith",
                                      "Attack of the Clones",
                                      "The Phantom Menace"))) %>% 
  mutate(pct = round(count / sum(count) * 100, 2))


  ggplot(movie_ranking, aes(x = movie, y = pct)) + 
  geom_bar(stat = "identity",
           fill = "dodgerblue",
           position = position_dodge(width=.1),
           width = .7) + 
  geom_text(aes(y = pct, 
                label = c(paste0(round(pct[1]),"%"), round(pct[2:6]))), 
            nudge_y = .9,
            size = 3,
            color = "grey30") +
  scale_y_continuous(expand = c(0,.5)) +
  coord_flip() +
  labs(title = "What's the Best `Star Wars` Movie?",
       subtitle = "Of 471 respondents who have seen all six films") +
  theme_fivethirtyeight() + 
  theme(plot.margin = margin(4,1,4,1,"cm"),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", margin = margin(r=-1)),
        plot.title = element_text(margin = margin(2,2,2,2)),
        plot.subtitle = element_text(margin = margin(t = 3)))
  
