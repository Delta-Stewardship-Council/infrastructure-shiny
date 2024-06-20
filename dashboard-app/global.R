# LOAD LIBRARIES ----
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)



# READ DATA ----

mock_data_levee <- read_sf("data/mock-data-levee-area.shp")


risk_soc_data <- mock_data_levee %>% 
  select(island_name = LMA, 
         island_tract = islnd_t,
         social_vul = socl_vl,
         risk,
         risk_scaled_percent = rsk_sc_)


