library(tidyverse)
library(ggrepel)
library(readxl)

pet_dat_raw <- read_xlsx(here::here("data/week18_dallas_animals.xlsx"))

pet_dat_simple <- read_xlsx(here::here("data/week18_dallas_animals.xlsx"),
                            sheet = 2)

pet_count <- pet_dat_simple %>% 
  filter(outcome_type == "ADOPTION") %>% 
  mutate(due_month = lubridate::floor_date(intake_date, "month")) %>% 
  count(due_month)

ggplot(pet_count, aes(x = due_month, y = n)) + 
  geom_point(color = "darkgreen", size = 2.2) + 
  geom_line(color = "darkgreen", size = .8) + 
  geom_text_repel(aes(label = n), color = "darkgreen", size = 3) + 
  scale_y_continuous(breaks = seq(600, 1200, 100), 
                     labels = c(seq(600, 900, 100),
                                "1.0K", "1.1K", "1.2K"), 
                     limits = c(600, 1200)) + 
  scale_x_datetime(date_breaks = "1 month", 
                   date_labels = "%b.%Y") + 
  labs(title = "Shelter Animal Adoption by Month", 
       y = "Animal Count", x = "Month") + 
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = -45),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold", family = "Arial", 
                                  color = "firebrick3", size = 16))

pet_dat_simple %>% 
  count(animal_type) %>% 
  mutate(total = 34819,
         pct = n/total,
         animal_type = fct_reorder(animal_type, pct, max)) %>% 
  ggplot(aes(x = "", y = pct, fill = animal_type)) + 
  geom_bar(width = 1, stat = "identity", color = "white") + 
  scale_fill_manual(values = rev(c("dodgerblue4", 
                                   "sienna2", 
                                   "seagreen3", 
                                   "grey40", 
                                   "grey10"))) + 
  coord_polar("y", start = 0) + 
  labs(title = "Animals Rescued by Type of Animal") + 
  theme_minimal() + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold", family = "Arial", 
                                  color = "firebrick3", size = 16))

animal_incidents <- jsonlite::read_json("https://www.dallasopendata.com/resource/w26u-9wvq.json",
                                        simplifyVector = T)

animal_incidents %>% 
  mutate(count_activity_number = parse_number(count_activity_number),
         council_district = fct_relevel(council_district, c(1:14, "<NA>"))) %>% 
  ggplot(aes(x = council_district, y = count_activity_number)) + 
  geom_bar(stat = "identity", fill = "darkred") + 
  geom_text(aes(label = scales::comma(count_activity_number)),
            angle = 90, nudge_y = 500) + 
  scale_y_continuous(breaks = seq(0,10000, 1000),
                     labels = scales::comma,
                     expand = c(0,0),
                     limits = c(0,10000)) + 
  scale_x_discrete(labels = c(1:14, "")) + 
  labs(title = "Animal Related Incidents by District", 
       x = "District", y = "Incidents") + 
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", family = "Arial", 
                                  color = "firebrick3", size = 16), 
        axis.text.x = element_text(angle = -45),
        axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 30)))

dog_incidents <- jsonlite::read_json("https://www.dallasopendata.com/resource/6zmn-7zx9.json", 
                    simplifyVector = T)

dog_incidents %>% 
  mutate(count_activity_number = parse_number(count_activity_number),
         council_district = fct_relevel(council_district, c(1:14, "NA"))) %>% 
  ggplot(aes(x = council_district, y = count_activity_number)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_text(aes(label = scales::comma(count_activity_number)),
            angle = 90, nudge_y = 400) + 
  scale_y_continuous(limits = c(0, 7000),
                     breaks = seq(0,7000,1000),
                     expand = c(0,0)) + 
  scale_x_discrete(labels = c(1:14, ""),
                   expand = c(0,0)) + 
  labs(title = "Dog Related Incidents by District", 
       x = "District", y = "Incidents") + 
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", family = "Arial", 
                                  color = "firebrick3", size = 16), 
        axis.text.x = element_text(angle = -45),
        axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 30)))
  
animal_bites <- jsonlite::read_json("https://www.dallasopendata.com/resource/53fd-xwpd.json",
                                    simplifyVector = T)

animal_bites %>% 
  mutate(count_activity_number = parse_number(count_activity_number),
         council_district = fct_relevel(council_district, c(1:14, "NA"))) %>% 
  ggplot(aes(x = council_district, y = count_activity_number)) +
  geom_bar(stat = "identity", fill = "orchid4") +
  geom_text(aes(label = scales::comma(count_activity_number)),
            angle = 90, nudge_y = -20, color = "grey90") + 
  scale_y_continuous(limits = c(0, 700),
                     breaks = seq(0,700,100),
                     expand = c(0,0)) + 
  scale_x_discrete(labels = c(1:14, ""),
                   expand = c(0,0)) + 
  labs(title = "Animal Bites by District", 
       x = "District", y = "Incidents") + 
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", family = "Arial", 
                                  color = "firebrick3", size = 16,
                                  hjust = .5), 
        axis.text.x = element_text(angle = -45),
        axis.title = element_text(face = "bold"))
