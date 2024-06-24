# ................SETUP............................

# load packages ----
library(tidyverse)
library(leaflet)
library(sf)


# ............ READ DATA....................

prop_fail_area <- read_sf("dashboard-app/data/mock_prob_fail_levee_area.shp") %>% 
  mutate(island_tract = paste0("island_", 1:77)) 

soc_vul_index <- read_csv("dashboard-app/data/mock_social_vul_index.csv")


#.............. DATA WRANGLING............................

levee_area_data <- prop_fail_area %>% 
  left_join(soc_vul_index, by = "island_tract") %>% 
  mutate(fail_percentile = percent_rank(lev_flr),
         soc_vul_percentile = percent_rank(social_vul),
         lma = str_to_title(lma))

fail_soc_wgs84 <- levee_area_data %>% 
  st_transform(crs = 4326)



# .............PRACTICE PLOTS.....................

# leaflet map ----
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
  






