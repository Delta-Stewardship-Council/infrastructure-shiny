## Shiny UI

## Load libraries
library(shiny)
library(leaflet)

##  Side bar dropdown menu choices

### Setting the dropdown menu options to be several component of the model for now. This list can be extended if necessary.

vars <- c(
  "Select Parameter",
  "Total Risk" = "risk",
  "Probability of flooding" = "prob_flood",
  "Levee condition" = "levee_cond",
  "Income" = "income",
  "Water Quality" = "water_quality"
)


## Styling line creating an HTML div tag (defines a division or a section in an HTML document)
inline=function(x) {
  tags$div(style="display:inline-block;", x) # allows the content to flow inline with other elements, rather than taking a new line like typical block elements.
}

## Navigation Bar

navbarPage("Delta Infrastructure: RAND Model", id="nav",
           
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          # Include custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map", width="100%", height="100%"),
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = 20, right = "auto", bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h3("Explore RAND Model Components"),
                                      h4("Instructions:"),
                                      tags$div(
                                        "1. Step 1",
                                        tags$br(),
                                        tags$br(),
                                        "2. Step 2",
                                        tags$br(),
                                        tags$br(),
                                        "3. Step 3",
                                        tags$br(),
                                        tags$br(),
                                        "4. Step 4.",
                                        tags$a("Get a lat/long from an address here.", href="https://www.latlong.net/convert-address-to-lat-long.html", target="_blank"),
                                        tags$br(),
                                        tags$br(),
                                        "5. Step 5"
                                      ),
                                      
                                      selectInput("color", "", vars, selectize=FALSE),
                                      
                                      uiOutput("selected_var"),
                                      
                                      h3("Pin Location on Map"),
                                      inline(numericInput("long", label = h5("Longitude:"), value = -121.50001)),
                                      inline(numericInput("lat", label = h5("Latitude:"), value = 38.00001)),
                                      
                                      actionButton("recalc", "Show point", width="40%"),
                                      
                                      tags$br(),
                                      tags$br(),
                                      
                                      a(shiny::icon("reply"), "Delta Science shinyapps homepage", 
                                        href="https://deltascience.shinyapps.io/Home/")
                                      
                                      
                        )
                        
                    ),
                    
                    tags$div(id="cite",
                             tags$em('This map was created in a part of a collaboration with the National Center for Ecological Analysis and Synthesys (NCEAS) and The Delta Stwearship Council, 2023')
                    )
           ), # EO tabPanel Interactive map
           
           tabPanel("Data explorer",
                    DT::dataTableOutput("table")
           ),
           tabPanel('Methodology and Resources',
                    h3("Background"),
                    tags$div(
                      "This tool was developed as part of",
                      tags$a("Delta Adapts,", href="https://deltacouncil.ca.gov/delta-plan/climate-change", target="_blank"),
                      "an initiative of the Delta Stewardship Council to improve resilience to climate change within the Delta through
                    equitable adaptation strategies.The first phase of the project focuses on conducting a vulnerability assessment for the region, 
                    in order to protect the vital resources provided by the Delta and the people who live here. The assessment phase characterizes
                    the potential impacts from seven climate stressors on five specific areas of focus for protection. One of these areas
                    of focus is 'Society and Equity', which seeks to preserve the unique cultural values of the Delta and prioritize actions
                    that protect its most vulnerable populations. Delta Adapts is incorporating equity by identifying communities and 
                    populations that are most susceptible to climate hazards, and develop adaptation strategies that recognize and remedy these inequities.",
                      
                      tags$br(),
                      tags$br(),
                      
                      "The most vulnerable populations are defined as 'Those which experience heightened risk and increased sensitivity 
                    to climate change and have less capacity and fewer resources to cope with, adapt to, or recover from climate impacts'",
                      tags$a("(Governor's office of Planning and Research, 2015).", href="https://www.opr.ca.gov/planning/icarp/tac/", target="_blank"), 
                      "These disproportionate effects are caused by physical (built and environmental), social, political, and/or economic 
                    factor(s), which are exacerbated by climate impacts. These factors include, but are 
                    not limited to, race, class, sexual orientation and identification, national origin, and income inequality.'",
                      
                      tags$br(),
                      tags$br(),
                      
                      "The ",tags$a("Vulnerability Assessment", href="https://deltacouncil.ca.gov/pdf/delta-plan/2021-06-25-delta-adapts-vulnerability-assessment.pdf", 
                                    target = "_blank")," was released in 2021.",
                      
                      
                      #         img(src="vulnerability_model.jpg", height="100%", width = "100%"),
                      
                      tags$br(),
                      
                      h3("Methodology"),
                      
                      h4("Indicator Selection"),
                      tags$div(
                        "We conducted a literature review and interviews with Delta community leaders to identify fourteen indicators
                        that contribute to increased sensitivity and decreased adaptive capacity to flooding, extreme heat, and wildfires.
                        These include nine socio-economic factors (young children, disability status, educational attainment, linguistic 
                        isolation, older adults (especially those living alone), poverty, race/ethnicity, tenancy, and 
                        household vehicle access) and five health factors (health insurance, cardiovascular disease, asthma, 
                        low birth weight, and food insecurity). Many of the indicators are based on five-year estimates from the 
                        2017 American Community Survey (ACS), an annual survey conducted by the Census Bureau to supplement the Decennial Census"
                      ),
                      tags$br(),
                      
                      h4("Weighting Scheme"),
                      tags$div("An individual block group or tract was assigned a score based on the number of indicators for which it 
                        is in the 70th percentile or higher. Separate scores were calculated based on the socio-economic indicators with data available at the block group 
                        level and the health indicators available at the tract level. A combined score was calculated by assigning tract-level 
                        scores to the block groups they contained (using a spatial join in ArcGIS). Because the inputs are at different scales, 
                        it is important to recognize that the precision of the combined score is overstated. However, there are twice as many
                        indicators at the block group level, so the combined score is heavily weighted towards the more fine-scale information."
                      ),
                      
                      tags$br(),
                      
                      h3("Data Information"),
                      h4("Data sources"),
                      tags$div(
                        tags$a("US Census Bureau 2017 American Community Survey (ACS)", href="https://data.census.gov/cedsci/", target="_blank"),
                        tags$br(),
                        tags$a("CalEnviroScreen 3.0", href="https://oehha.ca.gov/calenviroscreen", target="_blank"),
                        tags$br(),
                        tags$a("USDA Food Access Research Atlas", href="https://www.ers.usda.gov/data-products/food-access-research-atlas/download-the-data.aspx", target="_blank"),
                      ),
                      tags$br(),
                      tableOutput('indicators'),
                      
                      
                      h3("Resources"),
                      h4("Other vulnerability indices:"),
                      tags$div(
                        tags$a("CalEnviroScreen 3.0", href="https://oehha.ca.gov/calenviroscreen", target="_blank"),
                        tags$br(),
                        tags$a("California Heat Assessment Tool", href="https://www.cal-heat.org/", target="_blank"),
                        tags$br(),
                        tags$a("Climate Change and Health Vulnerability Indicators (CCHVIz)", href="https://discovery.cdph.ca.gov/ohe/CCHVIz", target="_blank"),
                        tags$br(),
                        tags$a("Disadvantaged Communities Mapping Tool", href="https://gis.water.ca.gov/app/dacs", target="_blank"),
                        tags$br(),
                        tags$a("Distressed Communities Index", href="https://eig.org/dci", target="_blank"),
                        tags$br(),
                        tags$a("Healthy Places Index (HPI)", href="https://healthyplacesindex.org/", target="_blank"),
                        tags$br(),
                        tags$a("Regional Opportunity Index", href="https://interact.regionalchange.ucdavis.edu/roi/webmap/webmap.html", target="_blank"),
                        tags$br(),
                        tags$a("Social Vulnerability Index (SoVI)", href="https://coast.noaa.gov/digitalcoast/data/sovi.html", target="_blank")
                      ),
                      tags$br(),
                      
                      h3("Contact"),
                      tags$div(
                        "Please contact the ",
                        tags$a(" Delta Stewardship Council", href="mailto:climatechange@deltacouncil.ca.gov"),
                        "  with any questions."
                      ),
                      
                      tags$br(),
                      tags$br()
                      
                    ),
                    
                    
                    conditionalPanel("false", icon("crosshair"))
           ) # EO tabPanel Methodology and Resources
) # EO navbarPage








