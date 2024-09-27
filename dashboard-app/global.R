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
prob_fail <- st_read("data/probFailure/prob_fail_levee_area.shp") %>% 
  st_transform('+proj=longlat +datum=WGS84')

structure_value <- st_read("dashboard-app/data/structureValue/nsi_2024_total_value_levee_area.shp")%>%
  st_transform('+proj=longlat +datum=WGS84') %>% 
  rename(structure_value_area = strct__,
         total_value_trill = ttl_vl_)

managed_wetlands <- st_read("data/managedWetlands/managedWetlands.shp") %>% 
  st_transform('+proj=longlat +datum=WGS84')

croplands <- st_read("data/deltaCropland/croplandsByType.shp") %>% 
  st_transform('+proj=longlat +datum=WGS84') %>% 
  rename(total_crop_area = ttl_cr_)

soc_vul <- st_read("data/leveeLSDayPopWeightSVI/leveeLSDayPopWeightSVI.shp") %>% 
  select(LMA, area, leveeAr, RPL_THEMES) %>% 
  st_transform('+proj=longlat +datum=WGS84')

levee_areas <- st_read("data-clean/shapefiles/fixedLevees/leveedAreas.shp") %>% 
  st_transform('+proj=longlat +datum=WGS84')

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
  domain  = managed_wetlands$PM_LndT)

## Croplands
unique(croplands$type)
antique <- c("#855C75", "#D9AF6B", "#AF6458", "#736F4C", "#526A83", "#625377", "#68855C", "#9C9C5E", "#A06177", "#8C785D", "#467378", "#7C7C7C")

crops_pal <- colorFactor(
  palette = antique,
  domain  = croplands$type)

## Social Vulnerability
soc_vul_pal <- colorNumeric(
  palette = "viridis", 
  domain = soc_vul$RPL_THEMES)

## structure value (bins)
range(structure_value$total_value_trill, na.rm = T)

## structure color pallet
bins <- c(0, 1, 10, 100, 1000, 20000, 30000, 40000, 70000)
structure_pal <- colorBin("YlOrRd", 
                          domain = structure_value$total_value_trill,
                          bins = bins)



## NEXT: NEED TO PREPARE DATE FOR BICHOROPLETHE MAP
## ------- OLD ---------
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

## NEXT: BUILD DF FOR DATA EXPLORER TAB