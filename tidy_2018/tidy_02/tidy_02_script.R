library(tidyverse)
library(readxl)
library(here)
library(glue)
library(ggrepel)

nfl.data <- read_xlsx(here("data/tidy_tuesday_week2.xlsx"))

# Inspiration from #rstats users elsherbini to use a tribble here
# Now we can get the abbreviations easily
# This will also double as our fct_relevel for later
shorthand <- tribble(
  ~position, ~abb,
  "Running Back", "RB",
  "Quarterback", "QB",
  "Offensive Lineman", "OL",
  "Tight End", "TE",
  "Wide Receiver", "WR",
  "Cornerback", "CB",
  "Defensive Lineman", "DE",
  "Linebacker", "LB",
  "Safety", "S",
  "Special Teamer", "ST"
)

# Let's get our data down to the Top 16 Athletes Long Format
top.paid <- nfl.data %>%
  gather(key = "position", "pay", Cornerback:`Wide Receiver`) %>%
  left_join(shorthand, by = "position") %>% 
  mutate(position = fct_relevel(position, shorthand$position),
         abb = fct_relevel(abb, shorthand$abb),
         pay = pay/1e6, 
         year = year %% 100,
         type = factor(case_when(
           abb %in% c("RB", "QB", "OL", "TE", "WR") ~ "Offense",
           TRUE ~ "Defense"
         ), level = c("Offense", "Defense"))) %>% 
  group_by(year, position) %>% 
  top_n(16, wt = pay) %>% 
  na.omit %>% 
  ungroup()

# Let's continue to clean our data to get the proportions
# spent on athletes
position.percent <- top.paid %>% 
  filter(!position %in% c("Special Teamer")) %>% 
  group_by(year, position, abb, type) %>% 
  summarise(total.paid = sum(pay)) %>% 
  group_by(year) %>% 
  mutate(percent.paid = total.paid/sum(total.paid)) %>% 
  ungroup()

wannabe_538 <- theme(plot.background = element_rect(color = "#e6e6e6"), 
                       panel.grid.minor = element_blank(),
                       plot.title = element_text(face = "bold", size = rel(1.5)),
                       plot.subtitle = element_text(size = rel(1.2)),
                       plot.caption = element_text(face = "italic"),
                       panel.background = element_blank(),
                       axis.ticks = element_blank(),
                       panel.grid.major = element_line(color = "#b6b6b6"),
                       strip.background = element_blank(),
                       strip.text = element_text(face = "bold", size = rel(1.3)),
                       axis.title = element_text(face = "bold", size = rel(1.2)),
                       axis.title.x = element_blank())

# Color pallete for the lines, I ended up switching a few colors 
# in Adobe Illustrator
col_pal <- c("#222222", "#8e76ac",
             "#f9d75a", "#d47bad",
             "#56ad74", "#8aaef6", "#b6b83c", 
             "#dd575a", "#aa7f77", "#6e6e6e")
names(col_pal) <- levels(position.percent$position)

# First Plot
ggplot(top.paid, aes(x = year, y = pay, group = position)) +
  geom_point(alpha = .3, size = 2) +
  geom_smooth(color = "orangered",
              size = .9,
              se = FALSE) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(-1.5, 26.5), 
                     labels = c("0", "5", "10", "15", "20", "$25m")) +
  scale_x_continuous(labels = scales::dollar_format(prefix = "'")) +
  facet_wrap(~ type + abb, ncol = 5) +
  annotate("segment", y = 0, yend = 0, 
           x = -Inf, xend = Inf,
           color = "#4d4d4d", size = .9) + 
  labs(title = "The average pay for top running backs has stalled", 
       subtitle = "Average cap value of 16 highest-paid players in each position",
       y = "Average Cap Value", x = "Year",
       caption = "Graphic: @dylanjm_ds | Data: spotrac.com") +
  wannabe_538

# Proportions plot
ggplot(position.percent, aes(x = year+2000, 
                             y = percent.paid,
                             color = position,
                             group = position)) +
  geom_point(alpha = .5, size = 2.5) +
  geom_smooth(size = .9, se = FALSE) +
  facet_wrap(~ type) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(-0.01, .25),
                     breaks = c(0, .05, .1, .15, .20),
                     labels = c("0", "5", "10", "15", "20%")) +
  scale_color_manual(values = col_pal) +
  geom_label_repel(data = filter(position.percent, 
                                 year == "18"), 
                   aes(label = abb),
                   label.size = NA) +
  annotate("segment", y = 0, yend = 0, 
           x = -Inf, xend = Inf,
           color = "#4d4d4d", size = .9) + 
  labs(title = "Teams are spending less on RBs",
       subtitle = "Percent of money spent on top 16 players at each position",
       caption = "Graphic: @dylanjm_ds | Data: spotrac.com",
       x = "Year", y = "Percent spent on each position") + 
  guides(color = FALSE) +
  wannabe_538
