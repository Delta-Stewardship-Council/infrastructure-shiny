# ................Dashboard Header.....................

header <- dashboardHeader(
  # title ----
  title = "Bay-Delta Levee Risk of Failur and Socio Economic Variables",
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
              
              # input box ----
              box(width = 4,
                  "dropdownMenu Input goes here"
                  
                  )
              
            ) # END Map fluid row
            
    ), # END Maps and Plots tabItem
    
    
    # methods tabItem
    tabItem(tabName = "methods",
            
            "METHODS INFORMATION GOES HERE"
      
    ), #END methods tabItem
    
    # data exploration tabItem ----
    tabItem(tabName = "dataExploration",
            
            "Interactive table with data goes here"
      
    ) #END data exploration tabItem
    
  ) # END tabItems
  
) # END dashboardBody

# ................combining all in one page................

dashboardPage(header, sidebar, body)
