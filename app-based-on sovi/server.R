# Shiny Server

## Install Libraries

### Set the default CRAN repository
default_repo <- "http://cran.us.r-project.org"

### List of required packages
required_packages <- c(
  "shiny",
  "leaflet",
  "tidyverse",
  "RColorBrewer",
  "scales",
  "lattice",
  "DT",
  "rgeos",
  "rsconnect",
  "sf"
)

### Check and install missing packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = default_repo)
  }
}

## Load libraries
library(rsconnect)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
# library(rgdal) ## Might need to update to `terra`
library(DT)
library(tidyverse)
library(rgeos) ## see if its needed
library(sf)
# library(here)

### Note: do we need a raster or is the shapefile enough??

## Read data
levee_area <- read_sf("mock_data/levees_shapefiles/fixedLevees.shp")

rand_mock <- read_csv("mock_data/mock_rand_data.csv")

## Do We need any other maps?? (County boundaries, whatersherd)

### Joining spatial data with model mock data
levee_area_data <- levee_area %>% 
  mutate(island_tract = paste0("island_", 1:139)) %>% 
  left_join(rand_mock, by = "island_tract")

### Do we need to set up a class for each variable? (see lines 35-41 in SOVI app)

## Text for popup 
popup_text <- paste(
  "<span style='font-size: 120%'><strong>Risk: ", levee_area_data$risk_scaled_percent,"</strong></span><br/>", 
  "<strong>", levee_area_data$LMA,", ", levee_area_data$island_tract, " Island", "</strong><br/>") %>% 
  # "Census Block Group ID: ", sovi@data$'Census Block Group ID', "<br/>",
  # "Est. median household income: ", dollar(sovi@data$MHHI), "<br/>", 
  # "Est. population: ", comma(sovi@data$est_pop), "<br/>",
  # sep="") %>%
  lapply(htmltools::HTML)

## 

function(input, output, session) {
  
  # Create point data for interactive location pins
  points <- eventReactive(input$recalc, {
    cbind(input$long, input$lat)
  }, ignoreNULL = FALSE)
  
  
  # Create basemap
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles(
        urlTemplate = "https://api.mapbox.com/styles/v1/moowill/cki3zbj5o4k4b19qlq98amqia/tiles/{z}/{x}/{y}?access_token=pk.eyJ1IjoibW9vd2lsbCIsImEiOiJja2kzejloOHkxdzNtMnhxcTAwY3Zqa25zIn0.VCsBGYnJr6Z7A7XnD157cg",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      )  %>% 
      setView(lat=38.1, lng=-121.8, zoom=10) %>% 
      addMarkers(data = points(), group="Location Pin")
    
  })
  
  
## Responsive description for each variable shown in side bar

  observe({
    textBy <- input$color
    if (textBy == "risk_scaled_percent") {
      output$selected_var <- renderUI({
        tags$div(
          "Description of Risk",
          tags$br(),
          tags$br(),
          "Longer description"
        )
      })
    }
    
    else if (textBy == "Select Indicator") {
      output$selected_var <- renderUI({
        tags$div(
          " ")
      })
    } 
    
    else if (textBy == "prob_flood") {
      output$selected_var <- renderUI({
        tags$div(
          "Description of prob of flooding",
          # tags$a("(US Global Change Research Program, 2016)", href="https://health2016.globalchange.gov/extreme-events", target="_blank"
          #        ),
          tags$br(),
          tags$br(),
          ## Need to update
          "Data Source: ", tags$a("US Census Bureau", href="https://data.census.gov/cedsci/table?q=B01001&tid=ACSDT5Y2017.B01001&hidePreview=false", target="_blank")
        )
      })
    } 
    
    else if (textBy == "levee_cond") {
      output$selected_var <- renderUI({
        tags$div(
          "Description of levee condition",
          # tags$a("(CA Office of Planning and Research, 2017).", href="https://www.opr.ca.gov/planning/icarp/resilient-ca.html", target="_blank"),
          # "Further, individual households' ability to access resources to recover from natural disasters varies based on race",
          # tags  $a("(Elliott et al., 2020).", href="https://journals.sagepub.com/doi/full/10.1177/2378023120905439", target="_blank"),
          tags$br(),
          tags$br(),
          ## Needs to be updated
          "Data Source: ", tags$a("US Census Bureau", href="https://data.census.gov/cedsci/table?q=B03002&tid=ACSDT5Y2017.B03002&hidePreview=false", target="_blank")
        )
      })
    }
    
    else if (textBy == "income") {
      output$selected_var <- renderUI({
        tags$div(
          "Income description", 
          # tags$a("(Knowlton et al. 2009)", href="https://pubmed.ncbi.nlm.nih.gov/19165388/", target="_blank"),
          # "and wildfire smoke",
          # tags$a("(Stone et al. 2019).", href="https://ww3.arb.ca.gov/smp/progdev/pubeduc/wfgv8.pdf", target="_blank"),
          # "Further, older adults living alone are less likely to be able to evacuate on their own.",
          # tags$a("(US Global Change Research Program, 2016)", href="https://health2016.globalchange.gov/extreme-events", target="_blank"),
          tags$br(),
          tags$br(),
          ## Needs to be updated
          "Data Source: ", tags$a("US Census Bureau", href="https://data.census.gov/cedsci/table?q=B11007&tid=ACSDT5Y2017.B11007&hidePreview=false", target="_blank")
        )
      })
    }
    
    else if (textBy == "water_quality") {
      output$selected_var <- renderUI({
        tags$div(
          "Water quality description",
          # tags$a("(Raval et al. 2019).", href="https://apen4ej.org/wp-content/uploads/2019/10/APEN-Mapping_Resilience-Report.pdf", target="_blank"),
          tags$br(),
          tags$br(),
          ## Needs to be updated
          "Data Source: ", tags$a("US Census Bureau", href="https://data.census.gov/cedsci/table?q=B15003&tid=ACSDT5Y2017.B15003&hidePreview=false", target="_blank"))
      })
    }
  })
  }

    
  
## Interactive and responsive polygons in map

# observe({
#   colorBy <- input$color
#   
#   if (colorBy == "risk_scaled_percent") {
#     colorData <- levee_area_data$colorBy
#     pal <- colorNumeric("viridis", colorData)
#   }
#   
#   else if (colorBy == "Select Parameter") {
#     ifelse(levee_area_data$risk_scaled_percent < 1, " ", " ")
#     pal <- colorNumeric("transparent", colorData)
#   }
#   
#   else {
#     colorData <- levee_area_data$colorBy
#     pal <- colorNumeric("viridis", colorData)
# 
#   }
#   
#   
#   
#   
#   # add indicator data and legend    
#   leafletProxy("map", 
#                data = levee_area_data) %>%
#     clearShapes() %>% # removes appearance of previously selected indicator polygons
#     addPolygons(data=levee_area_data, 
#                 fillColor=pal(colorData), 
#                 group = "continuous",
#                 stroke=TRUE,
#                 fillOpacity = 0.6, 
#                 color="black", # polygon border color
#                 weight=0.8, # polygon border weight
#                 popup = popup_text) %>% 
#                 # group = "Indicator Data") %>% 
#     addLegend("bottomright", 
#               pal=pal, 
#               values=colorData, 
#               title=colorBy,
#               layerId="colorLegend", 
#               labFormat = labelFormat(prefix = "", 
#                                       suffix = "", 
#                                       between = " - ", 
#                                       digits = 0)
#     ) %>%
#     
#     # add location markers
#     addMarkers(data = points(), group="Location Pin") 
#     
#   })
#     
    # # add feature shapefiles
    # ## Delta boundary
    # addPolygons(data=delta_sm, 
    #             fill=F, 
    #             stroke=T, 
    #             color="black", # polygon border color
    #             weight=3, # polygon border weight
    #             group = "Delta + Suisun Marsh Boundary") %>% 
    # ## County boundaries 
    # addPolygons(data=delta_counties, 
    #             fillColor = "transparent", 
    #             stroke=T, 
    #             color="yellow", # polygon border color
    #             weight=3, # polygon border weight
    #             label=paste(delta_counties@data$NAME_UCASE),
    #             group = "County Boundaries") %>% 
    # 
    # ## Watersheds 
    # addPolygons(data=watersheds, 
    #             fillColor = "transparent", 
    #             stroke=T, 
    #             color="#08306B", # polygon border color
    #             weight=3, # polygon border weight
    #             label=paste(watersheds@data$Name),
    #             group = "Watersheds") %>% 
    ## Vegetation
    #    addPolygons(data=delta_veg, 
    #                fillColor = v_pal, 
    #                stroke=T, 
    #                color="black", # polygon border color
    #                weight=3, # polygon border weight
    #                label=paste(delta_veg@data$`Vegetation Type`),
    #                group = "Vegetation Cover") %>% 
    
#     # add layer control panel 
#     addLayersControl(
#       #    baseGroups = c("Basemap"),
#       overlayGroups = c("Indicator Data","Delta + Suisun Marsh Boundary", "Location Pin", "Watersheds", "County Boundaries"), #, "Vegetation Cover"),
#       options = layersControlOptions(collapsed = FALSE)
#     ) %>% 
#     hideGroup("Watersheds") %>% 
#     hideGroup("County Boundaries")
#   #     hideGroup("Vegetation Cover")
#   
#   
# })





