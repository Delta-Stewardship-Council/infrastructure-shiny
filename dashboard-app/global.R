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
library(htmltools)
library(janitor)
library(markdown)
library(htmlwidgets)


# READ DATA ----
prob_fail <- st_read("data/probFailure/prob_fail_levee_area.shp") %>% 
  st_transform('+proj=longlat +datum=WGS84') %>% 
  clean_names()

structure_value <- st_read("data/structureValue/nsi_2024_total_value_levee_area.shp")%>%
  st_transform('+proj=longlat +datum=WGS84') %>% 
  rename(structure_value_area = strct__,
         total_value_trill = ttl_vl_) %>% 
  clean_names()

managed_wetlands <- st_read("data/managedWetlands/managedWetlands.shp") %>% 
  st_transform('+proj=longlat +datum=WGS84') %>% 
  clean_names()

croplands <- st_read("data/deltaCropland/croplandsByType.shp") %>% 
  st_transform('+proj=longlat +datum=WGS84') %>% 
  rename(total_crop_area = ttl_cr_) %>% 
  clean_names()

soc_vul <- st_read("data/leveeLSDayPopWeightSVI/leveeLSDayPopWeightSVI.shp") %>% 
  select(LMA, area, leveeAr, RPL_THEMES) %>% 
  st_transform('+proj=longlat +datum=WGS84') %>% 
  clean_names()

levee_areas <- st_read("data/fixedLevees/leveedAreas.shp") %>% 
  st_transform('+proj=longlat +datum=WGS84') %>% 
  clean_names()

## Everything re project into '+proj=longlat +datum=WGS84' to avoid issues with Leaflet.

## Layer map color pallets ----

## Probability of Failure
prob_fail_pal <- colorNumeric(
  palette = "magma", 
  domain = prob_fail$lev_flr)

## Managed Wetlands
rainbow <- c("#ff0000", "#ff5300", "#ffa500", "#ffd200", "#ffff00", "#80c000", "#008000", "#004080", "#0000ff", "#2600c1", "#4b0082")


managed_wetlands_pal <- colorFactor(
  palette = rainbow,
  domain  = managed_wetlands$pm_lnd_t)

## Croplands
unique(croplands$type)
vivid <- c("#E58606", "#5D69B1", "#52BCA3", "#99C945", "#CC61B0", "#24796C", "#DAA51B")


crops_pal <- colorFactor(
  palette = vivid,
  domain  = croplands$type)

## Social Vulnerability
soc_vul_pal <- colorNumeric(
  palette = "viridis", 
  domain = soc_vul$rpl_themes)

## structure value (bins)
range(structure_value$total_value_trill, na.rm = T)

## structure color pallet
bins <- c(0, 1, 10, 100, 1000, 20000, 30000, 40000, 70000)
structure_pal <- colorBin("YlOrRd", 
                          domain = structure_value$total_value_trill,
                          bins = bins)



## Data for bichoropleth map
prob_fail2 <- prob_fail %>% 
  select(lma, lev_flr)

structure_value2 <- structure_value %>% 
  select(lma, total_value_trill) %>% 
  st_drop_geometry() %>% 
  mutate(lma = str_to_lower(lma))

soc_vul2 <- soc_vul %>% 
  select(lma, rpl_themes) %>% 
  st_drop_geometry() %>% 
  mutate(lma = str_to_lower(lma))

## combine data soc vul + other variable need to be in the same data frame
bichoropleth_all_data <- prob_fail2 %>% 
  left_join(structure_value2, by = "lma") %>% 
  left_join(soc_vul2, by = "lma") %>% 
  mutate(fail_percentile = percent_rank(lev_flr),
         soc_vul_percentile = percent_rank(rpl_themes),
         structure_val_percentile = percent_rank(total_value_trill))


## prep data for Data Explorer
data_explorer_table <- levee_areas %>% ## FIX!!
  # select(island_name = lma,
  #        levee_failure = lev_flr,
  #        social_vul,
  #        prob_levee_failure_prec = fail_percentile,
  #        soc_vul_percentile) %>%
  st_drop_geometry()

## NEXT: BUILD DF FOR DATA EXPLORER TAB