# load packages ----
library(tidyverse)
library(here)
library(sf)

# read data ----
levee_area <- read_sf(here("mock_data", "levees_shapefiles", "fixedLevees.shp"))

rand_mock <- read_csv(here("mock_data", "mock_rand_data.csv"))


# combine both data ----
levee_area_data <- levee_area %>% 
  mutate(island_tract = paste0("island_", 1:139)) %>% 
  left_join(rand_mock, by = "island_tract")

# save data to dashboard folder
write_sf(levee_area_data, here::here("dashboard-app", "data", "mock-data-levee-area.shp"))
