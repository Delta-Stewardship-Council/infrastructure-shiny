# LOAD LIBRARIES ----
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse)
library(sf)
library(leaflet)
library(bivariatechoropleths)
library(DT)


# READ DATA ----
prop_fail_area <- read_sf("data/probFailure/prob_fail_levee_area.shp") %>% 
  mutate(island_tract = paste0("island_", 1:130))

soc_vul_index <- st_read("/vsicurl/")

levee_area_data <- prop_fail_area %>% 
  left_join(soc_vul_index, by = "island_tract") %>% 
  mutate(fail_percentile = percent_rank(lev_flr),
         soc_vul_percentile = percent_rank(social_vul),
         lma = str_to_title(lma))

fail_soc_wgs84 <- levee_area_data %>% 
  st_transform(crs = 4326) 

data_explorer_table <- levee_area_data %>% 
  select(island_name = lma,
         levee_failure = lev_flr,
         social_vul,
         prob_levee_failure_prec = fail_percentile,
         soc_vul_percentile) %>% 
  st_drop_geometry()

