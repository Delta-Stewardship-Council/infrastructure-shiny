# Server ----

server <- function(input, output){
  
  # render leaflet layer map ----
  
  # Create reactive values to track layer loading status
  layerStatus <- reactiveValues(
    baseMapLoaded = FALSE,
    leveeAreasLoaded = FALSE,
    socVulLoaded = FALSE,
    probFailLoaded = FALSE,
    habitatLoaded = FALSE,
    cropsLoaded = FALSE,
    structureLoaded = FALSE
  )
  
  # Base map with initial layer (Levee Areas)
  output$map_data_layers <- renderLeaflet({
    layerStatus$baseMapLoaded <- TRUE
    layerStatus$leveeAreasLoaded <- TRUE
    
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, 
                       group = "Grey background") %>%
      addProviderTiles("Esri.WorldImagery",
                       group = "Imagery") %>%
      addPolygons(data = levee_areas,
                  group = "Levee Areas",
                  label = ~htmlEscape(paste("Name:", lma)),
                  color = "black",
                  fill = NA,
                  weight = 1.2) %>%
      addLayersControl(
        baseGroups = c("Grey background", "Imagery"),
        overlayGroups = c("Levee Areas",
                          "Social Vulnerability",
                          "Probability of Failure",
                          "Habitat Type",
                          "Croplands",
                          "Structure"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("Social Vulnerability",
                  "Probability of Failure",
                  "Habitat Type",
                  "Croplands",
                  "Structure")) %>%
      setView(lat=38.2, lng=-121.7, zoom=9)
    
    # Add event listener for layer control
    map %>% htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        map.on('overlayadd', function(e) {
          Shiny.setInputValue('activeLayer', e.name);
        });
      }
    ")
  })
  
  # Observe layer activation events
  observeEvent(input$activeLayer, {
    layer <- input$activeLayer
    proxy <- leafletProxy("map_data_layers")
    
    # Social Vulnerability Layer
    if (layer == "Social Vulnerability" && !layerStatus$socVulLoaded) {
      layerStatus$socVulLoaded <- TRUE
      proxy %>%
        addPolygons(data = soc_vul,
                    group = "Social Vulnerability",
                    fillColor = ~soc_vul_pal(rpl_themes),
                    label = ~htmlEscape(paste("SOVI Index:", rpl_themes)),
                    stroke = TRUE,
                    fillOpacity = 0.6,
                    color = "black",
                    weight = 0.8) %>%
        addLegend(group = "Social Vulnerability",
                  pal = soc_vul_pal,
                  values = soc_vul$rpl_themes,
                  opacity = 0.6,
                  title = "Index",
                  position = "bottomleft")
    }
    
    # Probability of Failure Layer
    if (layer == "Probability of Failure" && !layerStatus$probFailLoaded) {
      layerStatus$probFailLoaded <- TRUE
      proxy %>%
        addPolygons(data = prob_fail,
                    group = "Probability of Failure",
                    fillColor = ~prob_fail_pal(lev_flr),
                    stroke = TRUE,
                    fillOpacity = 0.6,
                    color = "black",
                    weight = 0.8) %>%
        addLegend(group = "Probability of Failure",
                  pal = prob_fail_pal,
                  values = prob_fail$lev_flr,
                  opacity = 0.6,
                  title = "Probability of Failure",
                  position = "bottomleft")
    }
    
    # Habitat Type Layer
    if (layer == "Habitat Type" && !layerStatus$habitatLoaded) {
      layerStatus$habitatLoaded <- TRUE
      proxy %>%
        addPolygons(data = managed_wetlands,
                    group = "Habitat Type",
                    fillColor = ~managed_wetlands_pal(pm_lnd_t),
                    stroke = TRUE,
                    fillOpacity = 0.6,
                    color = "black",
                    weight = 0.8) %>%
        addLegend(group = "Habitat Type",
                  pal = managed_wetlands_pal,
                  values = managed_wetlands$pm_lnd_t,
                  opacity = 0.6,
                  title = "Habitat Type",
                  position = "bottomleft")
    }
    
    # Croplands Layer
    if (layer == "Croplands" && !layerStatus$cropsLoaded) {
      layerStatus$cropsLoaded <- TRUE
      proxy %>%
        addPolygons(data = croplands,
                    group = "Croplands",
                    fillColor = ~crops_pal(type),
                    stroke = TRUE,
                    fillOpacity = 0.6,
                    color = "black",
                    weight = 0.8) %>%
        addLegend(group = "Croplands",
                  pal = crops_pal,
                  values = croplands$type,
                  opacity = 0.6,
                  title = "Cropland Types",
                  position = "bottomleft")
    }
    
    # Structure Layer
    if (layer == "Structure" && !layerStatus$structureLoaded) {
      layerStatus$structureLoaded <- TRUE
      proxy %>%
        addPolygons(data = structure_value,
                    group = "Structure",
                    fillColor = ~structure_pal(total_value_trill),
                    stroke = TRUE,
                    fillOpacity = 0.6,
                    color = "black",
                    weight = 0.8) %>%
        addLegend(group = "Structure",
                  pal = structure_pal,
                  values = structure_value$total_value_trill,
                  opacity = 0.6,
                  title = "Structure Value",
                  position = "bottomleft")
    }
  }) # END of leaflet layer map
  
  # Lazy load menu items
  output$plotsMenuItem <- renderMenu({
    menuItem(text = "Plots", tabName = "plots", icon = icon("chart-simple"))
  })
  
  output$aboutMenuItem <- renderMenu({
    menuItem(text = "About", tabName = "about", icon = icon("info-circle"))
  })
  
  output$dataMenuItem <- renderMenu({
    menuItem(text = "Data Explorer", tabName = "dataExploration", icon = icon("table"))
  })
  
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
            var1_name = fail_percentile,
            var2_name = structure_val_percentile,
            ntiles= 3,
            var1_label = "Probability of Failure",
            var2_label = "Structure Value",
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
  
  # Lazy load content for other tabs
  output$plotsContent <- renderUI({
    req(input$sidebarmenu == "plots")
    div(
      h3("page under construction"),
      # Add your plots content here
    )
  })
  
  output$aboutContent <- renderUI({
    req(input$sidebarmenu == "about")
    includeMarkdown("DESCRIPTION AND METHODS INFORMATION GOES HERE")
  })
  
  output$dataExplorationContent <- renderUI({
    req(input$sidebarmenu == "dataExploration")
    div(
      h3("Data Explorer"),
      DTOutput("data_table")
    )
  })
  
  # Render data table when needed
  output$data_table <- renderDT({
    req(input$sidebarmenu == "dataExploration")
    datatable(data_explorer_table,
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                scrollY = TRUE
              )
    )
  })
  
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