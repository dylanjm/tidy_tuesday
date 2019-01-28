library(tidyverse)

agency_dat <- read_csv(here::here("data/data_2019/week03_agencies.csv"))
launch_dat <- read_csv(here::here("data/data_2019/week03_launches.csv"))

colors_four = rev(RColorBrewer::brewer.pal(5, "PuBu")[5:2])

colors_one = RColorBrewer::brewer.pal(5, "PuBu")[5:2]

launch_dat %>% 
  count(type, sort = T) %>% 
  top_n(10, n) %>% 
  mutate(type = fct_reorder(type, n, max)) %>% 
  ggplot(aes(x = type, y = n)) +
  geom_point(size = 8, color = "steelblue4") + 
  geom_text(aes(label = n), position = position_nudge(y = 25),
            size = 6, family = "IBMPlexSans-Light") + 
  scale_y_continuous(breaks = seq(100, 600, 100)) + 
  coord_flip() + 
  labs(title = "Soyuz-U Takes The Top",
       subtitle = "Out of all 366 Flight Options, Soyuz-U (produced by the Soviet Union)\nhas the most flights, with the other top 9 vehicles trailing far behind.",
       y = "Total Launches") + 
  theme_light() + 
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed"), 
        text = element_text(family = "IBMPlexSans-Light"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14, color = "grey20",
                                    face = "italic", hjust = 1),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 20, face = "bold",
                                  color = "grey20", family = "IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 12, family = "IBMPlexSans-Italic", 
                                     face = "italic", color = "grey30"),
        aspect.ratio = .5)

ggsave("tidy_2019/tidy_03/plot_01.png", width = 12, height = 6)

launch_dat %>%
  group_by(state_code, launch_year) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  mutate(state_code = fct_reorder(state_code, count)) %>% 
  complete(state_code, launch_year) %>% 
  ggplot(aes(x = launch_year, y = state_code, fill = count, linetype = "")) + 
  geom_tile(color = "grey99") + 
  scale_fill_gradientn(colors = colors_four, na.value = "#dfe4ec") + 
  scale_color_manual(values = NA) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1960, 2020),
                     breaks = c(seq(1960, 2010, 10), 2018)) + 
  coord_equal() + 
  labs(title = "Agency Launches Since the 1960's",
       fill = "Count") + 
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5),
         linetype = guide_legend("No Data", title.position = "top",
                               override.aes=list(fill="#dfe4ec"))) + 
  theme_minimal() + 
  theme(panel.grid = element_blank(),
        text = element_text(family = "IBMPlexSans-Light"),
        axis.title = element_blank(),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 22, face = "bold",
                                  color = "grey20", family = "IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 14, hjust = .5, color = "firebrick3",
                                     face = "italic"),
        legend.key.size = unit(1.3, "cm"),
        legend.text = element_text(size = 12),
        legend.position = "bottom")

ggsave("tidy_2019/tidy_03/plot_02.png", width = 12, height = 6)

prop <- function(df, ...) {
  out <- df %>% 
    group_by(...) %>% 
    summarise(n = n()) %>% 
    mutate(prop = n / sum(n))
  
  out
}

launch_dat %>% 
  prop(state_code, category) %>% 
  filter(category == "O") %>% 
  ungroup() %>% 
  mutate(state_code = fct_reorder(state_code, prop)) %>% 
  ggplot(aes(x = state_code, y = prop, size = n)) + 
  geom_point(color = "#045A8D") + 
  coord_flip() +
  scale_size_continuous(range = c(4, 12)) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Ground Control to Major Tom",
       subtitle = "Success rates computed among state agencies weighted by total number of launches from 1960-2018",
       size = "Total Launches", y = "Success Rate") + 
  guides(size = guide_legend(title.position = "top", title.hjust = 0.5)) + 
  theme_minimal() + 
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "IBMPlexSans-Light"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(hjust = 1, family = "IBMPlexSans-Light"),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 20, face = "bold",
                                  color = "grey20", family = "IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 12, family = "IBMPlexSans-Italic", 
                                     face = "italic", color = "grey30"),
        legend.key.size = unit(1.3, "cm"),
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        aspect.ratio = .5)

ggsave("tidy_2019/tidy_03/plot_03.png", width = 10, height = 6)

launch_dat %>% 
  prop(launch_year, state_code, category) %>% 
  filter(category == "O", 
         !state_code %in% c("KR", "UK", "KP")) %>% 
  ungroup() %>% 
  mutate(state_code = fct_reorder(state_code, n, .desc = T)) %>% 
  ggplot(aes(x = launch_year, y = prop)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = c(seq(1960, 2000, 20), 2018)) + 
  scale_y_continuous(breaks = seq(0, 1, .2),
                     labels = scales::percent_format(),
                     limits = c(0, 1),
                     position = "right") +
  facet_wrap(~ state_code, nrow = 4) + 
  labs(title = "Success Rate of Agencies Over Time",
       subtitle = "",
       x = "Year") + 
  theme_light() + 
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "IBMPlexSans-Light"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(hjust = 1, family = "IBMPlexSans-Light"),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 20, face = "bold",
                                  color = "grey20", family = "IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 12, family = "IBMPlexSans-Italic", 
                                     face = "italic", color = "grey30"),
        strip.text = element_text(family = "IBMPlexSans-Bold"))



