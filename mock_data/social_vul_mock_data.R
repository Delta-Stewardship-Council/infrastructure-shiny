# creating social vulnerability mock data

# Load packages ----
library(readr)
library(dplyr)


# read data ----
mock_data <- read_csv(file.path("mock_data", "mock_rand_data.csv"))


# select soc vul indicators
social_vul_index <- mock_data %>% 
  select(island_tract, income, education, race, drink_contam, traffic_impacts, waste_sites, social_vul) %>% 
  mutate(social_vul_perc = social_vul/100)


# save mock data ----

write_csv(social_vul_index, file.path("dashboard-app", "data", "mock_social_vul_index.csv"))
