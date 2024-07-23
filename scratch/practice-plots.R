# ................SETUP............................

# load packages ----
library(tidyverse)
library(leaflet)
library(sf)
library(here)


# ............ READ DATA....................

prop_fail_area <- read_sf(here("dashboard-app/data/probFailure/prob_fail_levee_area.shp")) %>% 
  mutate(island_tract = paste0("island_", 1:130)) 

soc_vul_index <- read_csv("dashboard-app/data/mock/mock_social_vul_index.csv")


#.............. DATA WRANGLING............................

levee_area_data <- prop_fail_area %>% 
  left_join(soc_vul_index, by = "island_tract") %>% 
  mutate(fail_percentile = percent_rank(lev_flr),
         soc_vul_percentile = percent_rank(social_vul),
         lma = str_to_title(lma))

fail_soc_wgs84 <- levee_area_data %>% 
  st_transform(crs = 4326)



# .............PRACTICE PLOTS.....................

# leaflet bechoropleth map ----
leaflet::leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lat=38.2, lng=-121.7, zoom=9) %>% 
  bivariatechoropleths::addBivariateChoropleth(
    map_data = fail_soc_wgs84,
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

# leaflet social vul ----

# Create a color palette for the soc_vul_map:
soc_vul_pal <- colorNumeric(
  palette = "viridis", 
  domain = fail_soc_wgs84$soc_vul_percentile,
  na.color = "transparent")

# labels for islands in soc_vul_map
soc_vul_labs <- paste0("<b>",fail_soc_wgs84$lma,"</b>", "</br>",
                       "<br><b>Social Vulnerability Index</b>",
                       "<br>",round(fail_soc_wgs84$soc_vul_percentile,3), "</br>",
                       "<br><b>","Probabiliy of Failure</b>",
                       "<br>", round(fail_soc_wgs84$fail_percentile, 3))


soc_vul_labs <- purrr::map(soc_vul_labs, htmltools::HTML)

leaflet::leaflet(fail_soc_wgs84) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = fail_soc_wgs84,
              fillColor = ~soc_vul_pal(soc_vul_percentile),
              label = soc_vul_labs,
              stroke = TRUE,
              fillOpacity = 0.6, 
              color="black", # polygon border color
              weight=0.8, ) %>% # polygon border weight
  addLegend(pal = soc_vul_pal,
            values = ~soc_vul_percentile,
            opacity = 0.6,
            title = "Social Vulnerability Index",
            position = "bottomleft") %>% 
  setView(lat=38.2, lng=-121.7, zoom=9)



# leaflet Prob fail (NEED TO BE UPDATED TO BE IMPACT) ----

# Create a color palette for the soc_vul_map:
prob_fail_pal <- colorNumeric(
  palette = "viridis", 
  domain = fail_soc_wgs84$fail_percentile,
  na.color = "grey")

# labels for islands in soc_vul_map
prob_fail_labs <- paste0("<b>",fail_soc_wgs84$lma,"</b>", "</br>",
                       "<br><b>","Probabiliy of Failure</b>",
                       "<br>", round(fail_soc_wgs84$fail_percentile, 3), "</br>",
                       "<br><b>Social Vulnerability Index</b>",
                       "<br>",round(fail_soc_wgs84$soc_vul_percentile,3))


prob_fail_labs <- purrr::map(prob_fail_labs, htmltools::HTML)

leaflet::leaflet(fail_soc_wgs84) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = fail_soc_wgs84,
              fillColor = ~prob_fail_pal(fail_percentile),
              label = prob_fail_labs,
              stroke = TRUE,
              fillOpacity = 0.6, 
              color="black", # polygon border color
              weight=0.8, ) %>% # polygon border weight
  addLegend(pal = prob_fail_pal,
            values = ~fail_percentile,
            opacity = 0.6,
            title = "Probability of Failure",
            position = "bottomleft") %>% 
  setView(lat=38.2, lng=-121.7, zoom=9)



  # bar plot ----

sample_levee_data <- levee_area_data %>% 
  select(lma, soc_vul_percentile, fail_percentile) %>% 
  top_n(5, fail_percentile)


ggplot(sample_levee_data) +
  geom_col(aes(x = lma,
               y = soc_vul_percentile,
               fill = "Social Vulnerability Index"),
           alpha = 0.6,  width = 0.5) + #fill = "#BE64AC",
  geom_col(aes(x = lma,  
               y = fail_percentile,
               fill = "Probability of Failure"), 
           width = 0.3) + #fill = "#59C8C8",
  coord_flip()+
  labs(x = "Levee Island", 
       y = "Percentile",
       fill = "Indicator") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand=c(0, 0))+
  theme_bw()+
  scale_fill_manual(values = c("#59C8C8", "#BE64AC"))+
  theme(legend.position = "top")
  






