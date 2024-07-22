# Server ----

server <- function(input, output){
  
  # render leaflet map ----
  output$leaflet_map <- renderLeaflet({
   
    if(input$map_choice == "soc_vul_map"){
      
      
    } 
    
    # create leaflet bichoropleth
    leaflet::leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lat=38.2, lng=-121.7, zoom=9) %>% 
      bivariatechoropleths::addBivariateChoropleth(
        map_data = fail_soc_wgs84,
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
    
  }) # END of leaflet map
  
  
  # filter for levee island ----
  levee_island_df <- reactive({
    levee_area_data %>% 
    filter(lma %in% input$levee_island_input)
  }) #END reactive levee island data frame
  
  # render overlay barplot ----
  output$risk_soc_vul_overlay_barplot <- renderPlot({
    
    ggplot(levee_island_df()) +
      geom_col(aes(x = lma,
                   y = soc_vul_percentile,
                   fill = "Social Vulnerability Index"),
               alpha = 0.6,  width = 0.5) + #fill = "#BE64AC",
      geom_col(aes(x = lma,  
                   y = fail_percentile,
                   fill = "Probability of Failure"), 
               width = 0.3) + #fill = "#59C8C8",
      coord_flip()+
      labs(x = "Levee Island", 
           y = "Percentile",
           fill = "Indicator") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                         expand=c(0, 0))+
      theme_bw()+
      scale_fill_manual(values = c("#59C8C8", "#BE64AC"))+
      theme(legend.position = "top",
            text = element_text(size=22))
  }) # END render overleyPlot
  
  
  # data explorer interactive table ----
  output$interactive_table_output <- DT::renderDataTable(
    
    DT::datatable(data_explorer_table,
                  options = list(paging=FALSE),
                  rownames = FALSE)
  ) # END data explorer interactive table
  
  
  
} # END Server