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
  
  # tabItems ----
  tabItems(
    
    # maps tabItem ----
    tabItem(tabName = "maps",
            
            # Map fluidRow ----
            fluidRow(
              
              # leaflet layer map box ----
              box(width = 12,
                  title = tags$strong("Data layers by levee areas"),
                  
                  # leaflet output ----
                  leafletOutput(outputId = "map_data_layers") %>% 
                    withSpinner(type = 1, color = "#4287f5")
                  
                  
                  ) # END layers map box
              
            ), # END layers map fluid row
            
            # Bichoropleth FluidRow
            fluidRow(
              # plot island pickerInput box
              box(width = 3,
                  
                  # pickerInput(inputId = "levee_island_input", label = "Select an island(s):",
                  #             choices = levee_island,
                  #             selected = c("Atlas Tract", "Bacon Island", "Bethel Island"),
                  #             options = pickerOptions(actionsBox = TRUE),
                  #             multiple = TRUE), # END island pickerInput
                 
                   ), # END checkbox input
              
              # plot box ----
              box(width = 9,
                  
                  # bichoropleth map output ----
                  plotOutput(outputId = "map_bichoropleth")
                  
                  ) # END bichoropleth map Box
              
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
