library(tidyverse)
library(readxl)
library(here)
library(glue)
library(ggrepel)

nfl.data <- read_xlsx(here("data/tidy_tuesday_week2.xlsx"))


# Use the Forcats Library to get the Facets in the order we want them
position.relevel <- c("Running Back", "Quarterback",
                      "Offensive Lineman", "Tight End",
                      "Wide Receiver", "Cornerback",
                      "Defensive Lineman", "Linebacker",
                      "Safety", "Special Teamer")

# Inspiration from #rstats users elsherbini to use a tribble here
# Now we can get the abbreviations easily
abbreviations <- tribble(
  ~position, ~abb,
  "Defensive Lineman", "DE",
  "Linebacker", "LB",
  "Cornerback", "CB",
  "Special Teamer", "ST",
  "Safety", "S",
  "Quarterback", "QB",
  "Running Back", "RB",
  "Wide Receiver", "WR",
  "Tight End", "TE",
  "Offensive Lineman", "OL"
)

# Let's get our data down to the Top 16 Athletes Long Format
top.paid <- nfl.data %>%
  gather(key = "position", "pay", Cornerback:`Wide Receiver`) %>%
  mutate(position = fct_relevel(position, position.relevel),
         pay = pay/10^6, 
         year = factor(glue("\'{year %% 100}"))) %>% 
  group_by(year, position) %>% 
  top_n(16, wt = pay) %>%
  na.omit() %>% 
  ungroup()

# Let's continue to clean our data to get the proportions
# spent on athletes
position.percent <- top.paid %>% 
  filter(position != "Special Teamer") %>% 
  group_by(year, position) %>% 
  top_n(16, pay) %>% 
  summarise(total.paid = sum(pay)) %>% 
  group_by(year) %>% 
  mutate(percent.paid = total.paid/sum(total.paid),
         type = case_when(
           position %in% c("Running Back", "Quarterback", 
                           "Offensive Lineman", "Tight End", 
                           "Wide Receiver") ~ "Offense",
           TRUE ~ "Defense"
         ),
         type = fct_relevel(type, c("Offense", "Defense"))) %>% 
  ungroup() %>% 
  left_join(abbreviations, by = "position")

# First Plot
ggplot(top.paid, aes(x = year, y = pay, group = position)) +
  geom_point(alpha = .3) +
  geom_smooth(color = "#ff7133",
              size = .9,
              se = FALSE) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(-1.5, 26.5)) +
  scale_x_discrete(labels = levels(top.paid$year)[c(2,4,6,8)],
                   breaks = levels(top.paid$year)[c(2,4,6,8)]) +
  facet_wrap(~ position, ncol = 5) +
  annotate("segment", y = 0, yend = 0, 
           x = -Inf, xend = Inf,
           color = "#4d4d4d", size = .9) + 
  labs(title = "The average pay for top running backs has stalled", 
       subtitle = "Average cap value of 16 highest-paid players in each position",
       y = "Average Cap Value", x = "Year",
       caption = "Graphic: @__dylanjm__ | Data: spotrac.com") +
  theme(plot.background = element_rect(color = "#f0f0f0"), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = rel(1.5)),
        plot.subtitle = element_text(size = rel(1.2)),
        plot.caption = element_text(face = "italic"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(color = "#b6b6b6"),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

# Color pallete for the lines, I ended up switching a few colors 
# in Adobe Illustrator
col_pal <- c("#222222", "#d47bad", "#8aaef6", 
             "#f9d75a", "#8e76ac", "#aa7f77", 
             "#b6b83c", "#56ad74", "#dd575a", 
             "#6e6e6e")

# Proportions plot
ggplot(position.percent, aes(x = year, 
                             y = percent.paid,
                             color = position,
                             group = position)) +
  geom_point(alpha = .5) +
  geom_smooth(size = .9, se = FALSE) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(-0.01, .22),
                     labels = scales::percent) +
  scale_x_discrete(labels = levels(top.paid$year)[c(2,4,6,8)],
                   breaks = levels(top.paid$year)[c(2,4,6,8)]) +
  scale_color_manual(values = col_pal) +
  geom_label_repel(data = filter(position.percent, year == "'16"), aes(label = abb)) +
  facet_grid(. ~ type) +
  annotate("segment", y = 0, yend = 0, 
           x = -Inf, xend = Inf,
           color = "#4d4d4d", size = .9) + 
  labs(title = "Teams are spending less on RBs",
       subtitle = "Percent of money spent on top 16 players at each position",
       x = "Year", y = "Percent spent on each position") + 
  guides(color = FALSE) +
  theme(plot.background = element_rect(color = "#e6e6e6"), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = rel(1.5)),
        plot.subtitle = element_text(size = rel(1.2)),
        plot.caption = element_text(face = "italic"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(color = "#b6b6b6"),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))