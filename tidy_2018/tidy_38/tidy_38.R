library(tidyverse)

fish_dat <- read_csv(here::here("data/week38_allcetaceandata.csv"))

clean_fish <- fish_dat %>% 
  filter(acquisition %in% c("Born", "Rescue", "Capture")) %>% 
  mutate(acquisition = fct_relevel(acquisition, c("Rescue", "Born", "Capture")),
         acquisition = recode_factor(acquisition, 
                                     "Rescue" = "RESCUED", 
                                     "Born" = "BORN IN CAPTIVITY", 
                                     "Capture" = "CAPTURED"), 
         year_nu = lubridate::year(originDate))


ggplot(data = clean_fish,
       aes(x = year_nu, fill = acquisition)) + 
  geom_histogram(bins = 68, color = "grey20", alpha = .7) + 
  scale_fill_manual(values = c("#486d8a", "#708f6d", "#f8f17c")) + 
  scale_x_continuous(breaks = seq(1940, 2010, 10)) + 
  scale_y_continuous(breaks = seq(0, 200, 20), 
                     expand = c(0,0)) + 
  labs(y = "NUMBER OF ACQUIRED CETACEANS") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(linetype = 20, color = "grey40"),
        legend.position = "top", 
        panel.background = element_rect(fill = "#1c2a35"), 
        plot.background = element_rect(fill = "#1c2a35"), 
        text = element_text(family = "Arial", face = "bold", color = "#abc1d3"),
        axis.text = element_text(color = "#abc1d3"),
        axis.title.x = element_blank(), 
        legend.key = element_rect(fill = "#1c2a35", color = "#1c2a35"),
        legend.key.size = unit(10, "points"),
        legend.title = element_blank(), 
        legend.justification = "right",
        legend.background = element_rect(fill = "#1c2a35"), 
        plot.margin = unit(c(.5,2.5,.5,2.5), "cm"))

