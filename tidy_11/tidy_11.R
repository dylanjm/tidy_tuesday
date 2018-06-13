library(tidyverse)
library(ggpomological)
library(hrbrthemes)

fifa_data <- read_csv(here::here("data/week11_fifa_audience.csv")) %>% 
  select(-X1, -X1_1)

fed_relevel <- c("OFC", "CAF", "CONMEBOL", "CONCACAF", "AFC", "UEFA")
metric_relevel <- c("n_pct", "population_share", 
                    "tv_audience_share", "gdp_weighted_share")


fed_scales <- c("OFC (Oceania)", "CAF (Africa)", "CONMEBOL (S. America)", 
                "CONCACAF (N. America)", "AFC (Asia)", "UEFA (Europe)")

metric_scales <- c("n_pct" = "FIFA Members", "population_share" = "Global\nPopulation",
                   "tv_audience_share" = "World Cup TV\nAudience", 
                   "gdp_weighted_share" = "GDP-Weighted TV\nAudience")

fed_pct <- fifa_data %>% 
  count(confederation) %>% 
  mutate(n_pct = n/sum(n)) %>% 
  pull(n_pct)

fifa_long <- fifa_data %>% 
  group_by(confederation) %>% 
  summarise_at(3:5, sum) %>% 
  mutate(n_pct = round(fed_pct*100, 1)) %>% 
  gather(key = "pct_type", value = "pct", population_share:n_pct) %>% 
  mutate(confederation = fct_relevel(confederation, fed_relevel),
         pct_type = fct_relevel(pct_type, metric_relevel))

fifa_long <- fifa_long %>% 
  mutate(pct2 = case_when(
    confederation == "UEFA" ~ paste0(pct,"%"),
    TRUE ~ paste0(pct)
  ))

ggplot(data = fifa_long,
       aes(x = pct_type, y = confederation, fill = pct)) + 
  geom_tile(color = "grey85", size = .7) + 
  geom_text(aes(label = pct2, family = "Andale Mono"), nudge_x = .3) + 
  scale_fill_gradient(low = "white", high = "#46a131") +
  scale_x_discrete(position = "top",
                   labels = metric_scales,
                   expand = c(0,0)) + 
  scale_y_discrete(labels = fed_scales) + 
  labs(title = "FIFA Confederation Audience Share 2010") + 
  guides(fill = F) +
  theme_minimal() + 
  theme(axis.title = element_blank(),
        axis.text = element_text(face = "bold"),
        plot.margin = margin(5,3,5,3, "cm"),
        plot.title = element_text(face = "bold"))

ggplot(data = fifa_data,
       aes(x = population_share/100, 
           y = tv_audience_share/100, 
           size = gdp_weighted_share/100, 
           color = confederation)) + 
  geom_jitter(alpha = .8) +
  scale_x_sqrt(labels = scales::percent) + 
  scale_y_sqrt(labels = scales::percent) + 
  scale_color_pomological(labels = fed_scales) +
  scale_size_continuous(labels = scales::percent) + 
  labs(title = "FIFA Audience Share vs Population Share",
       x = "Population Share", y = "TV Audience Share",
       size = "GDP Weighted Share", color = "Confederation") +
  theme_ipsum_ps() 

ggplot(data = fifa_long,
       aes(x = pct/100, y = confederation, color = pct)) + 
  geom_point(size = 2.5) + 
  scale_x_continuous(labels = scales::percent) + 
  facet_wrap(~ pct_type, labeller = as_labeller(metric_scales)) +
  scale_y_discrete(labels = fed_scales) + 
  scale_color_gradientn(colors = wesanderson::wes_palettes$FantasticFox1) + 
  labs(title = "Aggregate Statistics on FIFA Audiences by Confederation",
       x = "Percent", y = "Confederation") + 
  guides(color = FALSE) + 
  theme_ipsum_ps()
