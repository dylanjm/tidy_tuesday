library(tidyverse)
library(ggthemes)

superhero_dat <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week9_comic_characters.csv")

hero_labeler <- c("DC" = "DC, New Earth continuity", 
                  "Marvel" = "Marvel, Earth-616 continuity")

# Plot 1
superhero_dat %>% 
  group_by(publisher, year) %>% 
  summarise(new_heros = n()) %>% 
  ggplot(aes(x = year, y = new_heros, fill = publisher, color = publisher)) +
  geom_bar(stat = "identity") + 
  annotate("segment", y = 0, yend = 0, 
           x = -Inf, xend = Inf,
           color = "black", size = .3) + 
  scale_y_continuous(labels = seq(0,500,100),
                     breaks = seq(0,500,100)) +
  scale_x_continuous(labels = c(1940, "'60", "'80", 2000),
                     breaks = seq(1940,2000,20)) + 
  scale_fill_manual(values = c("#4fb6f3", "#ed2b17")) + 
  scale_color_manual(values = c("#0c6ea7", "#d52410")) + 
  facet_wrap(~ publisher, labeller = as_labeller(hero_labeler)) + 
  guides(fill = FALSE, color = FALSE) + 
  labs(title = "New Comic Book Characters Introduced Per Year",
       caption = "Source: FiveThirtyEight") + 
  theme_fivethirtyeight() +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 20), 
        axis.text = element_text(size = 14, 
                                 family = "Cutive Mono", face = "bold"),
        plot.caption = element_text(family = "Cutive Mono",
                                    margin = margin(10)))

# Plot 02
female_pct <- superhero_dat %>% 
  group_by(publisher, year) %>% 
  mutate(hero_year_count = n()) %>% 
  group_by(publisher, year, sex) %>% 
  mutate(gender_count = n(),
         gender_pct = gender_count/hero_year_count) %>%
  ungroup() %>% 
  filter(sex == "Female Characters")

ggplot(data = female_pct,
       aes(x = year, y = gender_pct*100, 
           group = publisher, color = publisher)) + 
  geom_line(size = 1.2) + 
  annotate("segment", y = 0, yend = 0, 
           x = -Inf, xend = Inf,
           color = "black", size = .3) + 
  geom_text(aes(x = 2002, y = 25, label = "Marvel"), 
            color = "#ed2b17", size = 5) + 
  geom_text(aes(x = 2001, y = 44, label = "DC"), 
            color = "#4fb6f3", size = 5) + 
  scale_x_continuous(limits = c(1980, 2013),
                     labels = c(1980, "'90", 2000, "'10")) + 
  scale_y_continuous(labels = c(seq(0,40,10), "50%"),
                     breaks = seq(0,50,10),
                     limits = c(0,50)) + 
  scale_color_manual(values = c("#4fb6f3", "#ed2b17")) +
  guides(color = FALSE) +
  labs(title = "Comics Aren't Gaining Many Female Characters",
       subtitle = "Percentage of new characters who are female") + 
  theme_fivethirtyeight() + 
  theme(plot.title = element_text(size = 20), 
        axis.text = element_text(size = 14, 
                                 family = "Roboto Mono Light", 
                                 face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(family = "Cutive Mono",
                                    margin = margin(10)))

# Plot 03
hero_align <- superhero_dat %>% 
  mutate(align = case_when(
    align == "Good Characters" ~ align,
    align == "Bad Characters" ~ align,
    TRUE ~ "Neutral Characters"
  )) %>% 
  group_by(publisher, sex, align) %>% 
  summarise(align_count = n()) %>% 
  filter(sex %in% c("Female Characters", "Male Characters")) %>% 
  group_by(publisher, sex) %>% 
  mutate(total_count = sum(align_count),
         gender_pct  = align_count/total_count,
         align = fct_relevel(align, c("Bad Characters", 
                                      "Neutral Characters", 
                                      "Good Characters")))

ggplot(hero_align, aes(x = fct_relevel(sex, c("Male Characters", 
                                              "Female Characters")),
                       y = gender_pct, fill = align)) + 
  geom_bar(stat = "identity",
           width = .3,
           position = position_stack()) + 
  geom_text(aes(label = glue::glue("{round(gender_pct*100)}%")),
            color = "white", position = position_stack(vjust = .5)) + 
  scale_fill_manual(values = c("#fc2a1c", "#f5b92b", "#78a949")) + 
  scale_x_discrete(labels = c("Male", "Female")) + 
  coord_flip() + 
  facet_wrap(~ publisher, ncol = 1) + 
  labs(title = "Good Girls Gone Meh",
       subtitle = "Character alignment by gender") + 
  guides(fill = FALSE) + 
  theme_fivethirtyeight() + 
  theme(panel.grid = element_blank(), 
        axis.text.x = element_blank())

# Plot 04
superhero_dat %>% 
  filter(!is.na(gsm)) %>% 
  group_by(year) %>% 
  summarise(year_count = n()) %>% 
  ggplot(aes(x = year, y = year_count)) + 
  geom_bar(stat = "identity", fill = "#52ac3c") + 
  scale_y_continuous(labels = seq(0,15,5), 
                     breaks = seq(0,15,5), 
                     limits = c(0,15)) + 
  scale_x_continuous(labels = c(1940, glue::glue("'{seq(50,90,10)}"),2000,"'10"),
                     breaks = seq(1940,2010,10)) + 
  labs(title = "Comics Are Gaining A Few LGBT Characters",
       subtitle = "LGBT characters introduced into DC and Marvel comics per year,\nincluding retroactive continuity changes") + 
  theme_fivethirtyeight() + 
  theme(axis.text = element_text(family = "Roboto Mono Light", size = 12))

# Plot 05
superhero_dat %>% 
  count(publisher, year, sex) %>% 
  group_by(publisher, year) %>% 
  mutate(total_hero = sum(n)) %>%
  filter(sex %in% "Female Characters") %>% 
  group_by(publisher) %>% 
  mutate(cum_hero = cumsum(total_hero),
         cum_female = cumsum(n),
         gender_ratio = cum_female/cum_hero) %>% 
  ggplot(aes(x = year, y = gender_ratio*100, group = publisher, color = publisher)) + 
  geom_line() + 
  geom_text(aes(x = 1994, y = 17, label = "Marvel"),
            color = "#ed2b17", size = 5) + 
  geom_text(aes(x = 1990, y = 29, label = "DC"), 
            color = "#4fb6f3", size = 5) +
  annotate("segment", y = 0, yend = 0, 
           x = -Inf, xend = Inf,
           color = "black", size = .3) + 
  scale_y_continuous(limits = c(0,50),
                     labels = c(seq(0,40,10), "50%")) + 
  scale_x_continuous(limits = c(1939, 2013),
                     labels = c(1940, glue::glue("'{seq(50,90,10)}"), 2010, "'10"),
                     breaks = seq(1940,2010, 10)) + 
  scale_color_manual(values = c("#4fb6f3", "#ed2b17")) +
  labs(title = "The Gender Ratio In Comic Books Is Improving",
       subtitle = "Percentage of total characters in universe who are female",
       caption = "Source: FiveThirtyEight") + 
  guides(color = FALSE) + 
  theme_fivethirtyeight() + 
  theme(axis.text = element_text(family = "Roboto Mono", size = 12))


# Plot 06
ggplot(superhero_dat, aes(x = hair)) + 
  geom_bar(data = filter(superhero_dat, align == "Good Characters")) +
  geom_bar(data = filter(superhero_dat, align == "Bad Characters"),
                 aes(y = ..count.. * (-1))) + 
  geom_text(aes(x = 27, y = -1500, label = "Evil"), size = 5) + 
  geom_text(aes(x = 27, y = 1300, label = "Good"), size = 5) + 
  scale_y_continuous(breaks = seq(-2000,2000,1000),
                     labels = abs(seq(-2000,2000,1000))) + 
  coord_flip() + 
  labs(title = "",
       subtitle = "Plot shows hair color by character alignment, ") + 
  theme_fivethirtyeight()
