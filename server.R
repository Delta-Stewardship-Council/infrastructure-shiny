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
  "<span style='font-size: 120%'><strong>Risk: ", levee_area_data$risk,"</strong></span><br/>", 
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
  
  
  # Responsive description text and data citations
  observe({
    textBy <- input$color
    if (textBy == "risk") {
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

    
    # else if (textBy == "% of households that include someone with a disability") {
    #   output$selected_var <- renderUI({
    #     tags$div(
    #       "People with disabilities are less likely to be able to evacuate on their own, and are therefore more vulnerable
    #     to impacts from climate change",
    #       tags$a("(US Global Change Research Program, 2016).", href="https://health2016.globalchange.gov/extreme-events", target="_blank"),
    #       tags$br(),
    #       tags$br(),
    #       "Data Source: ", tags$a("US Census Bureau", href="https://data.census.gov/cedsci/table?q=B22010&tid=ACSDT5Y2017.B22010&hidePreview=false", target="_blank"))
    #   })
    # }
    # 
    # else if (textBy == "% of rented housing units") {
    #   output$selected_var <- renderUI({
    #     tags$div(
    #       "Renters have fewer resources to repair damage or procure temporary and replacement housing
    #     in the event of a natural disaster",
    #       tags$a("(Cutter et al. 2003),", href="https://onlinelibrary.wiley.com/doi/abs/10.1111/1540-6237.8402002", target="_blank"),
    #       "and face increased exposure and reduced adaptive capacity to hazards like wildfire smoke",
    #       tags$a("(Stone et al. 2019).", href="https://ww3.arb.ca.gov/smp/progdev/pubeduc/wfgv8.pdf", target="_blank"),
    #       "In the case of extreme heat events, renters without air conditioning may not have the option of installing it,
    #     or may not be able to afford higher energy costs associated with using AC during peak demand",
    #       tags$a("(CA Office of Planning and Research, 2017).", href="https://www.opr.ca.gov/planning/icarp/resilient-ca.html", target="_blank"),
    #       tags$br(),
    #       tags$br(),
    #       "Data Source: ", tags$a("US Census Bureau", href="https://data.census.gov/cedsci/table?q=B25003&tid=ACSDT5Y2017.B25003&hidePreview=false", target="_blank"))
    #   })
    # }
    # 
    # else if (textBy == "% of households with no vehicle access") {
    #   output$selected_var <- renderUI({
    #     tags$div(
    #       "Households without access to a vehicle are less able to evacuate and are therefore more vulnerable.",
    #       tags$a("(CA Office of Planning and Research, 2017).", href="https://www.opr.ca.gov/planning/icarp/resilient-ca.html", target="_blank"),
    #       tags$br(),
    #       tags$br(),
    #       "Data Source: ", tags$a("US Census Bureau", href="https://data.census.gov/cedsci/table?q=B25044&tid=ACSDT1Y2018.B25044&hidePreview=false", target="_blank"))
    #   })
    # }
    # 
    # 
    # else if (textBy == "% of households with limited English proficiency") {
    #   output$selected_var <- renderUI({
    #     tags$div(
    #       "Linguistically isolated households are more vulnerable to climate change impacts, such as flooding and extreme 
    #     heat events, because they have more limited access to or understanding 
    #     of emergency alerts, health warnings, and safety information than the general population",
    #       tags$a("(CA Office of Planning and Research, 2017).", href="https://www.opr.ca.gov/planning/icarp/resilient-ca.html", target="_blank"),
    #       tags$br(),
    #       tags$br(),
    #       "Data Source: ", tags$a("US Census Bureau", href="https://data.census.gov/cedsci/table?q=C16002&tid=ACSDT5Y2017.C16002&hidePreview=false", target="_blank")
    #     )
    #   })
    # }
    # 
    # else if (textBy == "% of households with income <200% of the federal poverty line") {
    #   output$selected_var <- renderUI({
    #     tags$div(
    #       "Low-income communities face a host of disadvantages that are compounded by climate change impacts,
    #     such as higher baseline rates of chronic medical conditions that increase their sensitivity to environmental hazards
    #     and fewer resources with which to recover from natural disasers",
    #       tags$a("(CA Office of Planning and Research, 2017).", href="https://www.opr.ca.gov/planning/icarp/resilient-ca.html", target="_blank"),
    #       tags$br(),
    #       tags$br(),
    #       "Data Source: ", tags$a("US Census Bureau", href="https://data.census.gov/cedsci/table?q=C17002&tid=ACSDT5Y2017.C17002&hidePreview=false", target="_blank")
    #     )
    #   })
    # }
    # 
    # else if (textBy == "% of babies born at low birth weight") {
    #   output$selected_var <- renderUI({
    #     tags$div(
    #       "Low birth weights are indicative of increased stressed levels in pregnant people, and are often an impact 
    #     of hazards such as flooding, extreme heat, and wildfire smoke. It is also a useful proxy for overall
    #     community health and as a predictor of future health conditions",
    #       tags$a("(US Global Change Research Program, 2016).", href="https://health2016.globalchange.gov/extreme-events", target="_blank"),
    #       tags$br(),
    #       tags$br(),
    #       "Data Source: ", tags$a("CalEnviroScreen 3.0", href="https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-30", target="_blank")
    #     )
    #   })
    # }
    # 
    # else if (textBy == "Asthma ER visits per 10k") {
    #   output$selected_var <- renderUI({
    #     tags$div(
    #       "Individuals suffering from asthma are more sensitive to air pollution caused by climate change 
    #     hazards such as extreme heat and wildfire smoke",
    #       tags$a("(Stone et al. 2019),", href="https://ww3.arb.ca.gov/smp/progdev/pubeduc/wfgv8.pdf", target="_blank"),
    #       tags$a("(CA Office of Planning and Research, 2017).", href="https://www.opr.ca.gov/planning/icarp/resilient-ca.html", target="_blank"),
    #       tags$br(),
    #       tags$br(),
    #       "Data Source: ", tags$a("CalEnviroScreen 3.0", href="https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-30", target="_blank")
    #     )
    #   })
    # }
    # 
    # else if (textBy == "Heart-attack ER visits per 10k") {
    #   output$selected_var <- renderUI({
    #     tags$div(
    #       "Individuals suffering from cardiovascular disease are more sensitive to air pollution caused by climate change 
    #     hazards such as extreme heat and wildfire smoke",
    #       tags$a("(Stone et al. 2019),", href="https://ww3.arb.ca.gov/smp/progdev/pubeduc/wfgv8.pdf", target="_blank"),
    #       tags$a("(CA Office of Planning and Research, 2017).", href="https://www.opr.ca.gov/planning/icarp/resilient-ca.html", target="_blank"),
    #       tags$br(),
    #       tags$br(),
    #       "Data Source: ", tags$a("CalEnviroScreen 3.0", href="https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-30", target="_blank")
    #     )
    #   })
    # }
    # 
    # else if (textBy == "% uninsured households") {
    #   output$selected_var <- renderUI({
    #     tags$div(
    #       "Individuals that lack health insurance may face more difficulty 
    #     accessing care for conditions caused or exacerbated by climate change impacts,
    #     such as extreme heat, exposure to floodwaters, or wildfire smoke",
    #       tags$a("(CA Office of Planning and Research, 2017).", href="https://www.opr.ca.gov/planning/icarp/resilient-ca.html", target="_blank"),
    #       tags$br(),
    #       tags$br(),
    #       "Data Source: ", tags$a("US Census Bureau", href="https://data.census.gov/cedsci/table?q=B27001&tid=ACSDT1Y2018.B27001&hidePreview=false", target="_blank")
    #     )
    #   })
    # }
    # 
    # else { #(textBy == "Region of low food access") {
    #   output$selected_var <- renderUI({
    #     tags$div(
    #       "A 'food desert' is defined as a region where at least 100 households are more than a half mile
    #     from the nearest supermarket and have no access to a vehicle, or where at least 500 people or 33% 
    #     of the population live more than 20 miles from the nearest supermarket. Food deserts are just one 
    #     measure of food insecurity, which has been identified as a significant contributor to
    #     health outcomes.",
    #       tags$a("(CA Healthy Places Index)", href="https://healthyplacesindex.org/data-reports/", target="_blank"),
    #       tags$br(),
    #       tags$br(),
    #       "Data Source: ", 
    #       tags$a("USDA Food Access Research Atlas", href="https://www.ers.usda.gov/data-products/food-access-research-atlas/download-the-data.aspx", target="_blank")
    #     )
    #   })
  
  




