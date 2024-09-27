#................Input Variables.....................

# defining vectors with names of map layers for user to choose from
##***CHECK IF WE NEED THIS***
impact_layers <- c("Probability of Failure", "Economic Value of Habitat", "Overall Risk")


# ................Dashboard Header.....................

header <- dashboardHeader(
  # title ----
  title = "Delta Levee Risk Assesment Tool",
  titleWidth = 400
  
  
) # END DashboardHeader


# ................Dashboard Sidebar.....................

sidebar <- dashboardSidebar(
  
  # sidebar menu ----
  sidebarMenu(
    menuItem(text = " Maps",
             tabName = "maps",
             icon = icon("map")),
    
    menuItem(text = " Plots",
             tabName = "plots",
             icon = icon("chart-simple")),
    
    menuItem(text = "About",
             tabName = "about",
             icon = icon("list")),
    
    menuItem(text = " Data Exploration",
             tabName = "dataExploration",
             icon = icon("table"))
    
    
  ) # END sidebarMenu
  
) # END dashboardSidebar


# ................Dashboard Body..................... 

body <- dashboardBody(
  
  # tags$head(
  #   tags$style(HTML("
  #     .leaflet-left .leaflet-control{
  #       zoomControl: false;
  #     }
  #   "))
  # ),
  

  # tabItems ----
  tabItems(
    
    # maps tabItem ----
    tabItem(tabName = "maps",
            
            # Map layers description ----
            fluidRow(
              # use columns to create white space on sides
              column(9, includeMarkdown("text/map-layers-description.md")),
              column(3)
            ), # END map layers description
            
            # Map fluidRow ----
            fluidRow(
              
              # leaflet layer map box ----
              box(width = 12,
                  # title = tags$strong("Data layers by levee areas"),
                  
                  # leaflet output ----
                  leafletOutput(outputId = "map_data_layers") %>% 
                    withSpinner(type = 1, color = "#4287f5")
                  
                  
                  ) # END layers map box
              
            ), # END layers map fluid row
            
            # Map bichoropleth description ----
            fluidRow(
              # use columns to create white space on sides
              column(9, includeMarkdown("text/map-bichoropleth-description.md")),
              column(3)
            ), # END map bichoropleth description
            
            # Bichoropleth FluidRow ----
            fluidRow(
              
              # bichoropleth pickerInput box
              box(width = 12,
                  # bichoropleth map output ----
                  leafletOutput(outputId = "map_bichoropleth")%>% 
                    withSpinner(type = 1, color = "#4287f5"),
                  
                  absolutePanel(top = 10, 
                                right = 10,
                                selectInput(inputId = "bichoropleth_select", 
                                            label = "Select a map",
                                            choices = c("Social Vulnerability - Probability of Failure",
                                                        "Social Vulnerability - Structure Value",
                                                        "Probabiliy of Failure - Structure Value"))
                                # checkboxInput("legend", 
                                #               "Show legend",
                                #               TRUE)
                  ) # END absolute panel
              ) # END bichoropleth box
            
            ) # END bichoropleth map fluidRow
            
    ), # END Maps tabItem
    
    # Plots tabItem ----
    tabItem(tabName = "plots",
            
            "page under construction"
            
    ), # END Plots tabItem
    
    # about tabItem
    tabItem(tabName = "about",
            
            "DESCRIPTION AND METHODS INFORMATION GOES HERE"
      
    ), #END about tabItem
    
    # data exploration tabItem ----
    tabItem(tabName = "dataExploration",
            
                DT::dataTableOutput(outputId = "interactive_table_output")

      
    ) #END data exploration tabItem
    
  ) # END tabItems
  
) # END dashboardBody

# ................combining all in one page................

dashboardPage(header, sidebar, body)
