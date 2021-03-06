library(tidyverse)
library(readxl)
library(albersusa)
library(ggthemes)
library(tweenr)
library(animation)


tuition.raw <- readxl::read_xlsx("data/us_avg_tuition.xlsx") %>% 
  rename(state = State) # read in raw data and rename a column

tuition.clean <- tuition.raw %>% 
  gather(year, tuition, `2004-05`:`2015-16`) %>% # put data in tidy long format
  group_by(state) %>% 
  mutate(lag = lag(tuition, 5),
         pct.change = (tuition-lag)/lag) %>% # compute rolling 5 yr. percent change
  na.omit()

us <- usa_composite()
us_map <- broom::tidy(us, region = "name")

ggplot() +
  geom_map(data = us_map, map = us_map,
           aes(x = long, y = lat, map_id = id),
           color="#2b2b2b", size=0.05, fill=NA) +
  geom_map(data = filter(tuition.clean, year %in% "2015-16"), 
           map = us_map,
           aes(fill = pct.change, map_id = state),
           color = "white", size = .1) + 
  scale_fill_viridis_c("", labels = scales::percent, option = "A") +
  labs(title = "5 Year Tuition Growth by State 2011-2016") +
  theme_map() + 
  theme(legend.position = "bottom", 
        legend.justification = "center",
        legend.key.width = unit(1.3, "inches"),
        legend.background = element_blank(),
        plot.title = element_text(hjust=0.5, face="bold"),
        plot.background = element_rect(fill="#f7f7f7", color = "transparent"),
        panel.background = element_rect(fill="#f7f7f7", color = "transparent"))

tuition.clean %>% 
  filter(year %in% c("2010-11", "2015-16")) %>% 
  ggplot(aes(x = tuition, 
             y = fct_reorder(state, tuition, min), 
             color = year, 
             group = state)) + 
  geom_line() +
  geom_point() + 
  scale_x_continuous(labels = scales::dollar) +
  scale_color_manual(values = c("#a3c4dc","#0e668b")) + 
  guides(color = FALSE) +
  labs(title = "Tuition Growth from 2010-2016", x = "Tuition", y = "State") + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background = element_rect(fill="#f7f7f7", color = "transparent"),
        panel.background = element_rect(fill="#f7f7f7", color = "transparent"))

tuition.2 <- tuition.raw %>% 
  gather(year, tuition, `2004-05`:`2015-16`) %>% 
  mutate(year = parse_number(gsub("\\-.*", "", year)),
         state = fct_reorder(factor(state), tuition, min))

my_func <- function(y = 2008){
  tuition.2 %>% 
    filter(year == y) %>% 
    select(year, tuition, state)
}

my_list<-lapply(c(2004:2015, 2004), my_func)

tweenr.df<-tween_states(my_list, 
                       tweenlength = 3,
                       statelength = 2, 
                       ease = rep('cubic-in-out',11),
                       nframes=250) %>% 
  mutate(state = fct_reorder(state, tuition, min))

plotf3<- function(i=1){
g <- tweenr.df %>% 
  filter(.frame==i) %>%
  ggplot(aes(x=tuition, y=state, label=state)) +
  geom_text(nudge_x = 600, color = "black", size = 2.5)  +
  geom_point(color = "royalblue", size = 2) +
  scale_x_continuous(limits = c(3000, 16000), breaks = seq(3000,16000,1000)) +
  theme_minimal()  +
  theme(plot.title=element_text(size=18,color="royalblue",face="bold"),
        plot.subtitle=element_text(size=14,face="italic"),
        plot.caption=element_text(hjust=0,vjust=1),
        axis.text.x=element_text(size=12),
        legend.key.width=unit(1,"cm"),
        legend.position="top",
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major.y =element_blank())
}

animation::saveGIF({for (i in 1:max(tweenr.df$.frame)){
  g<-plotf3(i)
  print(g)
  print(paste(i,"out of",max(tweenr.df$.frame)))
  ani.pause()
}},movie.name="tidytuesday.gif", ani.width=650, ani.height=800)


tweenr.df %>% 
  filter(.frame == 1) %>%
  ggplot(aes(x=tuition, y=state, label=state)) +
  geom_text(nudge_x = 600, color = "black", size = 2.5)  +
  geom_point(color = "royalblue", size = 2) +
  scale_x_continuous(limits = c(3000, 16000), breaks = seq(3000,16000,1000)) +
  theme_minimal()  +
  theme(plot.title=element_text(size=18,color="royalblue",face="bold"),
        plot.subtitle=element_text(size=14,face="italic"),
        plot.caption=element_text(hjust=0,vjust=1),
        axis.text.x=element_text(size=12),
        legend.key.width=unit(1,"cm"),
        legend.position="top",
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major.y =element_blank())