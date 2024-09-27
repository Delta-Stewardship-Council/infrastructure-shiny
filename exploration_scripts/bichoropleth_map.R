## code to build the bichoropleth map
# # create leaflet bichoropleth
# leaflet::leaflet() %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   setView(lat=38.2, lng=-121.7, zoom=9) %>% 
#   bivariatechoropleths::addBivariateChoropleth(
#     map_data = fail_soc_wgs84,
#     var1_name = fail_percentile,
#     var2_name = soc_vul_percentile,
#     ntiles= 3,
#     var1_label = "Probability of Failure",
#     var2_label = "Social Vulnerability Index",
#     region_name = "lma",
#     weight = 1,
#     fillOpacity = 0.7,
#     color = "grey",
#     highlightOptions = leaflet::highlightOptions(color = "orange",
#                                                  weight = 2,
#                                                  opacity = 1)) 