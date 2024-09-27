## code to build the bichoropleth map

## Read data
prob_fail <- st_read("dashboard-app/data/probFailure/prob_fail_levee_area.shp") %>% 
  st_transform('+proj=longlat +datum=WGS84') %>% 
  clean_names() %>% 
  select(lma, lev_flr)

structure_value <- st_read("dashboard-app/data/structureValue/nsi_2024_total_value_levee_area.shp")%>%
  st_transform('+proj=longlat +datum=WGS84') %>% 
  rename(structure_value_area = strct__,
         total_value_trill = ttl_vl_) %>% 
  clean_names() %>% 
  select(lma, total_value_trill) %>% 
  st_drop_geometry() %>% 
  mutate(lma = str_to_lower(lma))

soc_vul <- st_read("dashboard-app/data/leveeLSDayPopWeightSVI/leveeLSDayPopWeightSVI.shp") %>% 
  select(LMA, area, leveeAr, RPL_THEMES) %>% 
  st_transform('+proj=longlat +datum=WGS84') %>% 
  clean_names() %>% 
  select(lma, rpl_themes) %>% 
  st_drop_geometry() %>% 
  mutate(lma = str_to_lower(lma))

## combine data soc vul + other variable need to be in the same data frame
bichoropleth_all_data <- prob_fail %>% 
  left_join(structure_value, by = "lma") %>% 
  left_join(soc_vul, by = "lma") %>% 
  mutate(fail_percentile = percent_rank(lev_flr),
         soc_vul_percentile = percent_rank(rpl_themes),
         structure_val_percentile = percent_rank(total_value_trill))


# create leaflet bichoropleth

leaflet::leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lat=38.2, lng=-121.7, zoom=9) %>%
  bivariatechoropleths::addBivariateChoropleth(
    map_data = bichoropleth_all_data,
    var1_name = fail_percentile,
    var2_name = soc_vul_percentile,
    ntiles= 3,
    var1_label = "Probability of Failure",
    var2_label = "Social Vulnerability Index",
    region_name = "lma",
    weight = 1,
    fillOpacity = 0.7,
    color = "grey",
    highlightOptions = leaflet::highlightOptions(color = "orange",
                                                 weight = 2,
                                                 opacity = 1))
