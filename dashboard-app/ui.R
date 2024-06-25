#................Input Variables.....................

# impact_layers <- setdiff(names(risk_soc_data), c("island_name", "island_tract", "geometry"))
impact_layers <- c("Probability of Failure", "Economic Value of Habitat", "Overall Risk")
soc_vul_layers <- c("Social Vulnerability Index", "Income", "Education", "Race", "Tribal Boundaries", "Drinking Water Contaminants", "Traffic Impacts", "Housing Burden")

levee_island <- sort(unique(levee_area_data$lma))


# ................Dashboard Header.....................

header <- dashboardHeader(
  # title ----
  title = "Scaramento-San Joaquin Delta: Risk of Levee Failure",
  titleWidth = 600
  
  
) # END DashboardHeader


# ................Dashboard Sidebar.....................

sidebar <- dashboardSidebar(
  
  # sidebar menu ----
  sidebarMenu(
    menuItem(text = " Introduction",
             tabName = "introduction",
             icon = icon("star")),
    
    menuItem(text = " Maps and Plots",
             tabName = "mapsPlots",
             icon = icon("map")),
    
    menuItem(text = " Methods",
             tabName = "methods",
             icon = icon("list")),
    
    menuItem(text = " Data Exploration",
             tabName = "dataExploration",
             icon = icon("table"))
    
    
  ) # END sidebarMenu
  
) # END dashboardSidebar


# ................Dashboard Body..................... 

body <- dashboardBody(
  
  # tabItems ----
  tabItems(
    
    # Introduction tabItem ----
    tabItem(tabName = "introduction",
            
            "INTRO TEXT GOES HERE"
            
            ), # END introduction tabItem
    
    # maps and plots tabItem ----
    tabItem(tabName = "mapsPlots",
            
            # Map fluidRow ----
            fluidRow(
              # map input box ----
              box(width = 3,
                  title = tags$strong("Select impact and social vulnerability inputs:"),
                  
                  # Impact selectInput ----
                  selectInput(
                    inputId = "map_impact_layer_input",
                    label = "Select an Impact Layer:",
                    choices = impact_layers
                    
                  ), # END Impact selectInput
                  
                  # Soc VUl select input
                  selectInput(
                    inputId = "map_soc_vul_layer_input",
                    label = "Select an indicator:",
                    choices = soc_vul_layers
                  
                  ), # END Soc Vul Select input
                  
                  ), # END selectBox input box
              
              # leaflet map box ----
              box(width = 9,
                  
                  title = tags$strong("Risk of Levee Failure and Social Vulnerability"),
                  
                  # leaflet output ----
                  leafletOutput(outputId = "bichoropleth_map") %>% 
                    withSpinner(type = 1, color = "#4287f5")
                  
                  
                  ) # END leaflet box
              
            ), # END Map fluid row
            
            # Plot FluidRow
            fluidRow(
              # plot island pickerinput box
              box(width = 3,
                  
                  pickerInput(inputId = "levee_island_input", label = "Select an island(s):",
                              choices = levee_island,
                              selected = c("Atlas Tract", "Bacon Island", "Bethel Island"),
                              options = pickerOptions(actionsBox = TRUE),
                              multiple = TRUE), # END island pickerInput
                 
                   ), # END checkbox input
              
              # plot box ----
              box(width = 9,
                  
                  # overlay barplot output ----
                  plotOutput(outputId = "risk_soc_vul_overlay_barplot")
                  
                  ) # END plotBox
              
            ) # END plot fluidRow
            
    ), # END Maps and Plots tabItem
    
    
    # methods tabItem
    tabItem(tabName = "methods",
            
            "METHODS INFORMATION GOES HERE"
      
    ), #END methods tabItem
    
    # data exploration tabItem ----
    tabItem(tabName = "dataExploration",
            
                DT::dataTableOutput(outputId = "interactive_table_output")

      
    ) #END data exploration tabItem
    
  ) # END tabItems
  
) # END dashboardBody

# ................combining all in one page................

dashboardPage(header, sidebar, body)
