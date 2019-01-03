library(tidyverse)
library(USAboundaries)

dat <- read_csv(here::here("data/week5_acs2015_county_data.csv"))

dat <- dat %>% 
  mutate(county = paste(County, "County")) %>% 
  left_join(state_codes, by = c("State" = "state_name")) %>% 
  left_join(blscrapeR::county_fips, by = c("state_abbr" = "state", "county" = "county"))

us_dat <- us_counties()

ggplot() + 
  geom_sf(data = us_dat)
