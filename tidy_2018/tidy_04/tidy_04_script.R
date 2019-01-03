library(tidyverse)
library(ggridges)

dat <- read_csv(here::here("data/men_women_salaries_australia.csv"))

top_jobs_women <- dat %>%
  filter(gender %in% "Female") %>% 
  top_n(10, average_taxable_income) %>% 
  pull(occupation)

test <- dat %>%
  filter(occupation %in% top_jobs_women) %>% 
  spread(key = gender, value = average_taxable_income) %>%
  group_by(occupation) %>% 
  summarise_at(vars(Female, Male), sum, na.rm = TRUE) %>% 
  mutate(occupation = case_when(
    occupation == "Plastic and reconstructive surgeon" ~ "Plastic surgeon", 
    occupation == "Gynaecologist; Obstetrician" ~ "Gynaecologist", 
    TRUE ~ occupation
  ),
  occupation = str_replace(occupation, "\x96", "-"),
  occupation = fct_reorder(occupation, Female, desc))


dat %>% 
  ggplot(aes(x = average_taxable_income, 
             y = gender, fill = gender, color = gender)) +
  geom_density_ridges(scale = 2.5, alpha = .95) + 
  scale_x_continuous(labels = scales::dollar(seq(0,600000,100000)),
                     breaks = seq(0,600000,100000)) +
  scale_fill_manual(values = c("#cc5f5e","#4b7e99")) +
  scale_color_manual(values = c("#cc5f5e","#4b7e99")) +
  guides(fill = FALSE, color = FALSE) + 
  labs(title = "Density Plot of Income between Australian Men & Women",
       subtitle = "Plot depicts income distribution between genders (Male & Female), given the top 100 \npaying jobs in Australia for the 2013-2014 income year. Measured in Australian Dollars ($)",
       x = "Average Taxable Income", caption = "Graphic: @dylanjm_ds | Source: data.gov.au") + 
  theme_classic() + 
  theme(axis.title.y = element_blank(), 
        panel.background = element_rect(fill = "#f2f2f2"),
        plot.background = element_rect(fill = "#f2f2f2"))

test %>% 
  ggplot() +
  geom_segment(aes(x=1, xend=2, y=Female, yend=Male)) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  geom_point(aes(x = 1, y = Female), color = "#cc5f5e", size = 2) + 
  geom_point(aes(x = 2, y = Male), color = "#4b7e99", size = 2) + 
  facet_wrap(~ occupation, ncol = 5) + 
  scale_y_continuous(labels = scales::dollar,
                     breaks = c(300000,400000,500000)) + 
  scale_x_continuous(breaks = c(1,2),
                     labels = c("Female", "Male"),
                     limits = c(.7,2.3)) + 
  labs(title = "Comparison of Average Taxable Income between Men & Women in Australia", 
       subtitle = "Graphic depicts difference in average taxable income between men and women \nfor Australia's top 10 highest paying jobs for women 2013-2014",
       y = "Average Taxable Income (AUS $)",
       caption = "Graphic: @dylanjm_ds | Source: data.gov.au") + 
  theme_minimal() + 
  theme(axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = "#f2f2f2"),
        plot.background = element_rect(fill = "#f2f2f2"), 
        panel.grid = element_blank())
