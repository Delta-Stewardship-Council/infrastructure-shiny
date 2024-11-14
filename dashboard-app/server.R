# Server ----

server <- function(input, output){
  
  # render leaflet layer map ----
  
  # Create reactive values to track layer loading status
  layerStatus <- reactiveValues(
    # baseMapLoaded = FALSE,
    # leveeAreasLoaded = FALSE,
    socVulLoaded = FALSE,
    probFailLoaded = FALSE,
    habitatLoaded = FALSE,
    cropsLoaded = FALSE,
    structureLoaded = FALSE
  )
  
  # Track rendering status
  layersRendered <- reactiveVal(0)
  totalLayers <- 5
  
  # Base map with initial layer (Levee Areas)
  output$map_data_layers <- renderLeaflet({
    layerStatus$baseMapLoaded <- TRUE
    layerStatus$leveeAreasLoaded <- TRUE

    leaflet() %>%
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
        overlayGroups = c("Levee Areas"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(lat = 38.2, lng = -121.7, zoom = 9) %>% 
      # Add event listener for layer control
      htmlwidgets::onRender("
                function(el, x) {
                    var map = this;
                    
                    // Track layer activation
                    map.on('overlayadd', function(e) {
                        Shiny.setInputValue('activeLayer', e.name);
                    });
                    
                    // Track layer rendering
                    map.on('layeradd', function(e) {
                        if (e.layer instanceof L.Polygon) {
                            Shiny.setInputValue('layerRendered', Math.random());
                        }
                    });
                }
            ")
  })
  
  additionalLayers %...>% (function(data) {
    
    notificationId <<- showNotification(
      "Loading additional layers...",
      duration = NULL,
      type = "message"
    )
    
    habitatLabels <- paste0(
      "<strong>", data$managed_wetlands$lma, "</strong>", 
      "<br>Total: ", round(data$managed_wetlands$ovrlp_at, 2),
      "<br>Agriculture: ", round(data$managed_wetlands$agrcltr, 2),
      "<br>Akali Seasonal Wetland: ", round(data$managed_wetlands$alkl_swc, 2),
      "<br>Grassland: ", round(data$managed_wetlands$grsslnd, 2),
      "<br>Managed Wetlands: ", round(data$managed_wetlands$mngd_wtl, 2),
      "<br>Oak Woodland: ", round(data$managed_wetlands$ok_wdlnd, 2),
      "<br>Riparian: ", round(data$managed_wetlands$riparin, 2),
      "<br>Seasonal and Nontidal Wetland: ", round(data$managed_wetlands$ssnla_nw, 2),
      "<br>Tidal Wetland: ", round(data$managed_wetlands$tdl_wtln, 2),
      "<br>Urban and Barren Land: ", round(data$managed_wetlands$urbna_bl, 2),
      "<br>Vernal Pool Complex: ", round(data$managed_wetlands$vrnl_pl_c, 2),
      "<br>Water: ", round(data$managed_wetlands$water, 2)
    ) %>% 
      lapply(htmltools::HTML)
    
    # # For sure cannot display all of these. Will only display the total for now...
    # cropLabels <- paste0(
    #   "<strong>", data$managed_wetlands$lma, "</strong>",
    #   "<br>Total: ", round(data$managed_wetlands$ovrlp_at, 2),
    #   "<br>Agriculture: ", round(data$managed_wetlands$agrcltr, 2),
    #   "<br>Akali Seasonal Wetland: ", round(data$managed_wetlands$alkl_swc, 2),
    #   "<br>Grassland: ", round(data$managed_wetlands$grsslnd, 2),
    #   "<br>Managed Wetlands: ", round(data$managed_wetlands$mngd_wtl, 2),
    #   "<br>Oak Woodland: ", round(data$managed_wetlands$ok_wdlnd, 2),
    #   "<br>Riparian: ", round(data$managed_wetlands$riparin, 2),
    #   "<br>Seasonal and Nontidal Wetland: ", round(data$managed_wetlands$ssnla_nw, 2),
    #   "<br>Tidal Wetland: ", round(data$managed_wetlands$tdl_wtln, 2),
    #   "<br>Urban and Barren Land: ", round(data$managed_wetlands$urbna_bl, 2),
    #   "<br>Vernal Pool Complex: ", round(data$managed_wetlands$vrnl_pl_c, 2),
    #   "<br>Water: ", round(data$managed_wetlands$water, 2)
    # ) %>%
    #   lapply(htmltools::HTML)

    leafletProxy("map_data_layers") %>%
      addPolygons(data = st_simplify(data$soc_vul, preserveTopology = T, dTolerance = 1),
                  # layerId = ~data$soc_vul$lma,
                  group = "Social Vulnerability",
                  fillColor = ~data$soc_vul_pal(rpl_themes),
                  # label = ~htmlEscape(paste("SOVI Index:", round(rpl_themes, 2))),
                  label = ~paste0("<strong>", lma, ":</strong> ", round(rpl_themes, 2)) %>% lapply(HTML),
                  stroke = F,
                  fillOpacity = 0,
                  opacity = 0,
                  color = "black",
                  weight = 0.8) %>%
      addPolygons(data = st_simplify(data$prob_fail, preserveTopology = T, dTolerance = 1),
                  # layerId = ~data$prob_fail$lma,
                  group = "Probability of Failure",
                  fillColor = ~data$prob_fail_pal(lev_flr),
                  label = ~paste0("<strong>", lma, ":</strong> ", round(lev_flr, 2)) %>% lapply(HTML),
                  stroke = F,
                  fillOpacity = 0,
                  opacity = 0,
                  color = "black",
                  weight = 0.8) %>%
      addPolygons(data = st_simplify(data$managed_wetlands, preserveTopology = T, dTolerance = 1),
                  # layerId = ~id,
                  group = "Habitat Type",
                  fillColor = ~data$managed_wetlands_pal(ovrlp_at),
                  label = ~habitatLabels,
                  stroke = F,
                  fillOpacity = 0,
                  opacity = 0,
                  color = "black",
                  weight = 0.8) %>%
      addPolygons(data = st_simplify(data$croplands, preserveTopology = T, dTolerance = 1),
                  # layerId = ~data$croplands$lma,
                  group = "Croplands",
                  fillColor = ~data$crops_pal(ttl_cr_vm),
                  label = ~paste0("<strong>", lma, ":</strong> ", round(ttl_cr_vm, 2)) %>% lapply(HTML),
                  stroke = F,
                  fillOpacity = 0,
                  opacity = 0,
                  color = "black",
                  weight = 0.8) %>%
      addPolygons(data = st_simplify(data$structure_value, preserveTopology = T, dTolerance = 1),
                  # layerId = ~data$structure_value$lma,
                  group = "Structure",
                  fillColor = ~data$structure_pal(total_value_trill),
                  label = ~paste0("<strong>", lma, ":</strong> ", ifelse(is.na(total_value_trill), "NA", round(total_value_trill, 2))) %>% 
                    lapply(HTML),
                  stroke = F,
                  fillOpacity = 0,
                  opacity = 0,
                  color = "black",
                  weight = 0.8) %>%
      addLayersControl(
        baseGroups = c("Grey background", "Imagery"),
        overlayGroups = c("Levee Areas",
                          "Social Vulnerability",
                          "Probability of Failure",
                          "Habitat Type",
                          "Croplands",
                          "Structure"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% hideGroup(c("Social Vulnerability",
                  "Probability of Failure",
                  "Habitat Type",
                  "Croplands",
                  "Structure"))
  })
  
  # Handle layer rendering completion
  observeEvent(input$layerRendered, {
    layersRendered(layersRendered() + 1)
    
    if (layersRendered() >= totalLayers) {
      removeNotification(notificationId)
    }
  })

  # Observe layer activation events
  observeEvent(input$activeLayer, {
    req(input$activeLayer)
    
    if ("Social Vulnerability" %in% input$activeLayer && !layerStatus$socVulLoaded) {
      
      additionalLayers %...>% (function(data) {
        leafletProxy("map_data_layers", data = data$soc_vul) %>%
          setGroupStyle(groupName = "Social Vulnerability", fillOpacity = 0.6) %>% 
          addLegend(group = "Social Vulnerability",
                    pal = data$soc_vul_pal,
                    values = data$soc_vul$rpl_themes,
                    opacity = 0.6,
                    title = "Social Vulnerability Index (SOVI)",
                    position = "bottomleft")
      })
      layerStatus$socVulLoaded <- TRUE
    }
    
    if ("Probability of Failure" %in% input$activeLayer && !layerStatus$probFailLoaded) {
      
      additionalLayers %...>% (function(data) {
        leafletProxy("map_data_layers", data = data$prob_fail) %>%
          setGroupStyle(groupName = "Probability of Failure", fillOpacity = 0.6) %>% 
          addLegend(group = "Probability of Failure",
                    pal = data$prob_fail_pal,
                    values = data$prob_fail$lev_flr,
                    opacity = 0.6,
                    title = "Probability of Failure",
                    position = "bottomleft")
      })
      layerStatus$probFailLoaded <- TRUE
    }
    
    if ("Habitat Type" %in% input$activeLayer && !layerStatus$habitatLoaded) {
      
      additionalLayers %...>% (function(data) {
        leafletProxy("map_data_layers", data = data$managed_wetlands) %>%
          setGroupStyle(groupName = "Habitat Type", fillOpacity = 0.6, stroke = T, opacity = 0.6) %>%
          addLegend(group = "Habitat Type",
                    pal = data$managed_wetlands_pal,
                    values = data$managed_wetlands$ovrlp_at,
                    opacity = 0.6,
                    title = "Habitat Acreage",
                    position = "bottomleft")
      })
      layerStatus$habitatLoaded <- TRUE
    }
    
    if ("Croplands" %in% input$activeLayer && !layerStatus$cropsLoaded) {
      
      additionalLayers %...>% (function(data) {
        leafletProxy("map_data_layers", data = data$croplands) %>%
          setGroupStyle(groupName = "Croplands", fillOpacity = 0.6) %>% 
          addLegend(group = "Croplands",
                    pal = data$crops_pal,
                    values = data$croplands$ttl_cr_vm,
                    opacity = 0.6,
                    title = "Crop Value (Millions)",
                    position = "bottomleft")
      })
      layerStatus$cropsLoaded <- TRUE
    }
    
    if ("Structure" %in% input$activeLayer && !layerStatus$structureLoaded) {

      additionalLayers %...>% (function(data) {
        leafletProxy("map_data_layers", data = data$structure_value) %>%
          setGroupStyle(groupName = "Structure", fillOpacity = 0.6) %>% 
          addLegend(group = "Structure",
                    pal = data$structure_pal,
                    values = data$structure_value$total_value_trill,
                    opacity = 0.6,
                    title = "Structure Value (Trillions)",
                    position = "bottomleft")
      })
      layerStatus$structureLoaded <- TRUE
    }
  })
  
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
  additionalLayers %...>% (function(data) {
    output$map_bichoropleth <- renderLeaflet({
      
      if(input$bichoropleth_select == "Social Vulnerability - Probability of Failure") {
          leaflet::leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            setView(lat = 38.2, lng = -121.7, zoom = 9) %>%
            bivariatechoropleths::addBivariateChoropleth(
              map_data = data$bichoropleth_all_data,
              var1_name = fail_percentile,
              var2_name = soc_vul_percentile,
              ntiles = 3,
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
        if(input$bichoropleth_select == "Social Vulnerability - Structure Value") {
            leaflet::leaflet() %>%
              addProviderTiles("CartoDB.Positron") %>%
              setView(lat = 38.2, lng = -121.7, zoom = 9) %>%
              bivariatechoropleths::addBivariateChoropleth(
                map_data = data$bichoropleth_all_data,
                var1_name = structure_val_percentile,
                var2_name = soc_vul_percentile,
                ntiles = 3,
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
              setView(lat = 38.2, lng = -121.7, zoom = 9) %>%
              bivariatechoropleths::addBivariateChoropleth(
                map_data = data$bichoropleth_all_data,
                var1_name = fail_percentile,
                var2_name = structure_val_percentile,
                ntiles = 3,
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
  })
  
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
} # END Server