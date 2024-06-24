# LOAD LIBRARIES ----
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)
library(sf)
library(leaflet)
library(bivariatechoropleths)


# READ DATA ----
prop_fail_area <- read_sf("data/mock_prob_fail_levee_area.shp") %>% 
  mutate(island_tract = paste0("island_", 1:77)) 

soc_vul_index <- read_csv("data/mock_social_vul_index.csv")

levee_area_data <- prop_fail_area %>% 
  left_join(soc_vul_index, by = "island_tract") %>% 
  mutate(fail_percentile = percent_rank(lev_flr),
         soc_vul_percentile = percent_rank(social_vul),
         lma = str_to_title(lma))

fail_soc_wgs84 <- levee_area_data %>% 
  st_transform(crs = 4326) 

