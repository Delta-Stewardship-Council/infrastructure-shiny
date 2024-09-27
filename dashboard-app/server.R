# Server ----

server <- function(input, output){
  
  # render leaflet map ----
  output$map_data_layers <- renderLeaflet({
   
      # # labels for islands in soc_vul_map
      # soc_vul_labs <- paste0("<b>",fail_soc_wgs84$lma,"</b>", "</br>",
      #                        "<br><b>Social Vulnerability Index</b>",
      #                        "<br>",round(fail_soc_wgs84$soc_vul_percentile,3), "</br>",
      #                        "<br><b>","Probabiliy of Failure</b>",
      #                        "<br>", round(fail_soc_wgs84$fail_percentile, 3))
      
      
      # soc_vul_labs <- purrr::map(soc_vul_labs, htmltools::HTML)
      
    leaflet() %>%
      ## add tiles
      addProviderTiles(providers$CartoDB.Positron, 
                       group = "Grey background") %>%
      addProviderTiles("Esri.WorldImagery", 
                       group = "Imagery") %>%
      ## Add all polygon layers
      addPolygons(data = levee_areas,
                  group = "Levee Areas",
                  label = ~htmlEscape(
                    paste("Name:", LMA)),
                  color="black", # polygon border color
                  fill = NA,
                  weight=1.2) %>% 
      addPolygons(data = soc_vul,
                  group = "Social Vulnerability",
                  fillColor = ~soc_vul_pal(RPL_THEMES),
                  label = ~htmlEscape(
                    paste("SOVI Index:", RPL_THEMES)),
                  # label = soc_vul_labs,
                  stroke = TRUE,
                  fillOpacity = 0.6, 
                  color="black", # polygon border color
                  weight=0.8 ) %>% # polygon border weight
      addPolygons(data = prob_fail,
                  group = "Probability of Failure",
                  fillColor = ~prob_fail_pal(lev_flr),
                  # label = soc_vul_labs,
                  stroke = TRUE,
                  fillOpacity = 0.6,
                  color="black", # polygon border color
                  weight=0.8, ) %>% # polygon border weight
      addPolygons(data = managed_wetlands,
                  group = "Habitat Type",
                  fillColor = ~managed_wetlands_pal(PM_LndT),
                  # label = soc_vul_labs,
                  stroke = TRUE,
                  fillOpacity = 0.6,
                  color="black", # polygon border color
                  weight=0.8, ) %>% # polygon border weight
      addPolygons(data = croplands,
                  group = "Croplands",
                  fillColor = ~crops_pal(type),
                  # label = soc_vul_labs,
                  stroke = TRUE,
                  fillOpacity = 0.6,
                  color="black", # polygon border color
                  weight=0.8, ) %>% # polygon border weight
      addPolygons(data = structure_value,
                  group = "Structure",
                  fillColor = ~structure_pal(total_value_trill),
                  # label = soc_vul_labs,
                  stroke = TRUE,
                  fillOpacity = 0.6,
                  color = "black", # remove polygon border
                  weight=0.8, ) %>%
      ## Legend layers
      addLegend(group = "Social Vulnerability",
                pal = soc_vul_pal,
                values = soc_vul$RPL_THEMES,
                opacity = 0.6,
                title = "Index",
                position = "bottomleft") %>%
      addLegend(group = "Probability of Failure",
                pal = prob_fail_pal,
                values = prob_fail$lev_flr,
                opacity = 0.6,
                title = "Probability of Failure",
                position = "bottomleft") %>%
      addLegend(group = "Habitat Type",
                pal = managed_wetlands_pal,
                values = managed_wetlands$PM_LndT,
                opacity = 0.6,
                title = "Habitat Type",
                position = "bottomleft") %>%
      addLegend(group = "Croplands",
                pal = crops_pal,
                values = croplands$type,
                opacity = 0.6,
                title = "Cropland Group",
                position = "bottomleft") %>%
      addLegend(group = "Structure",
                pal = structure_pal,
                values = structure_value$total_value_trill,
                opacity = 0.6,
                title = "Structure Value (Trillions)",
                position = "bottomleft") %>%
      addLayersControl(
        baseGroups = c("Grey background", "Imagery"),
        overlayGroups = c("Levee Areas",
                          "Social Vulnerability",
                          "Probability of Failure",
                          "Habitat Type",
                          "Croplands",
                          "Structure"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      # hide these groups by default
      hideGroup(c("Social Vulnerability",
                  "Probability of Failure",
                  "Habitat Type",
                  "Croplands",
                  "Structure")) %>%
      setView(lat=38.2, lng=-121.7, zoom=9)

    
  }) # END of leaflet layer map
  
  
  # filter for levee island ----
  levee_island_df <- reactive({
    levee_area_data %>% 
    filter(lma %in% input$levee_island_input)
  }) #END reactive levee island data frame
  
  # render bichoroplth map ----
   # END render bichoropleth map
  
  
  # data explorer interactive table ----
  # output$interactive_table_output <- DT::renderDataTable(
  #   
  #   DT::datatable(data_explorer_table,
  #                 options = list(paging=FALSE),
  #                 rownames = FALSE)
  # ) # END data explorer interactive table
  
  
  
} # END Server