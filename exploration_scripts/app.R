library(shiny)
library(leaflet)
library(sf)
library(dplyr)


prob_fail_area <- st_read("../dashboard-app/data/probFailure/prob_fail_levee_area.shp") %>%
  st_transform('+proj=longlat +datum=WGS84') %>% 
  mutate(island_tract = paste0("island_", 1:130))

prob_fail_pal <- colorNumeric(
  palette = "viridis", 
  domain = prob_fail_area$lev_flr)

shinyApp(
  ui <- fluidRow(
    leafletOutput("map")),
  
  server <- function(input, output, session) {
    
    #create empty vector to hold all click ids
    clickedIds <- reactiveValues(ids = vector())
    
    # initial map output
    output$map <- renderLeaflet({
      leaflet() %>% 
        addProviderTiles("CartoDB.Positron") %>% 
        setView(lat=38.2, lng=-121.7, zoom=9) %>%
        addPolygons(data = prob_fail_area,
                fillColor = "gray", #~prob_fail_pal(lev_flr)
                layerId = prob_fail_area$lma, 
                group = "selected",
                label = prob_fail_area$lma,
                stroke = TRUE,
                fillOpacity = 0.6, 
                color="black", # polygon border color
                weight=0.8 )
    }) # END INITIAL MAP LEAFLET
    
    observeEvent(input$map_shape_click, {
      
      #create object for clicked polygon
      click <- input$map_shape_click
      
      #define leaflet proxy for second regional level map
      proxy <- leafletProxy("map")
      
      #append all click ids in empty vector 
      clickedIds$ids <- c(clickedIds$ids, click$id)
      
      #shapefile with all clicked polygons - original shapefile subsetted by all admin names from the click list
      clickedPolys <- prob_fail_area[prob_fail_area$lma %in% clickedIds$ids, ]
      
      #if the current click ID [from island_tract] exists in the clicked polygon (if it has been clicked twice)
      if(click$id %in% clickedPolys$island_tract){
        
        #define vector that subsets NAME that matches island_tract click ID
        nameMatch <- clickedPolys$lma[clickedPolys$island_tract == click$id]
        
        #remove the current click$id AND its name match from the clickedPolys shapefile
        clickedIds$ids <- clickedIds$ids[!clickedIds$ids %in% click$id] 
        clickedIds$ids <- clickedIds$ids[!clickedIds$ids %in% nameMatch]
        
        #remove that highlighted polygon from the map
        proxy %>% removeShape(layerId = click$id)
        
      } else {
        
        #map highlighted polygons
        proxy %>% addPolygons(data = clickedPolys,
                              fillColor = ~prob_fail_pal(lev_flr),
                              fillOpacity = 1,
                              weight = 1,
                              color = "black",
                              stroke = T,
                              label = clickedPolys$island_tract, 
                              layerId = clickedPolys$island_tract)
      } #END CONDITIONAL
    }) #END OBSERVE EVENT
  }) #END SHINYAPP