# Server ----

server <- function(input, output){
  
  # render leaflet layer map ----
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
                    paste("Name:", lma)),
                  color="black", # polygon border color
                  fill = NA,
                  weight=1.2) %>% 
      addPolygons(data = soc_vul,
                  group = "Social Vulnerability",
                  fillColor = ~soc_vul_pal(rpl_themes),
                  label = ~htmlEscape(
                    paste("SOVI Index:", rpl_themes)),
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
                  fillColor = ~managed_wetlands_pal(pm_lnd_t),
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
                values = soc_vul$rpl_themes,
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
                values = managed_wetlands$pm_lnd_t,
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
  
  # render leaflet bichoropleth map ----
  output$map_bichoropleth <- renderLeaflet({
    
    if(input$bichoropleth_select == "Social Vulnerability - Probability of Failure"){
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
    } else {
      if(input$bichoropleth_select == "Social Vulnerability - Structure Value"){
      
      leaflet::leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lat=38.2, lng=-121.7, zoom=9) %>%
        bivariatechoropleths::addBivariateChoropleth(
          map_data = bichoropleth_all_data,
          var1_name = structure_val_percentile,
          var2_name = soc_vul_percentile,
          ntiles= 3,
          var1_label = "Structure Value",
          var2_label = "Social Vulnerability Index",
          region_name = "lma",
          weight = 1,
          fillOpacity = 0.7,
          color = "grey",
          highlightOptions = leaflet::highlightOptions(color = "orange",
                                                       weight = 2,
                                                       opacity = 1))
      
    } # END nested if
      else {
      leaflet::leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lat=38.2, lng=-121.7, zoom=9) %>%
        bivariatechoropleths::addBivariateChoropleth(
          map_data = bichoropleth_all_data,
          var1_name = structure_val_percentile,
          var2_name = fail_percentile,
          ntiles= 3,
          var1_label = "Structure Value",
          var2_label = "Probability of Failure",
          region_name = "lma",
          weight = 1,
          fillOpacity = 0.7,
          color = "grey",
          highlightOptions = leaflet::highlightOptions(color = "orange",
                                                       weight = 2,
                                                       opacity = 1))
    } # END 2nd else statement
    } # END 1st else statement
  
    }) # END bichoropleth map
  
  
  # filter for levee island ----
  # levee_island_df <- reactive({
  #   levee_area_data %>% 
  #   filter(lma %in% input$levee_island_input)
  # }) #END reactive levee island data frame
  
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