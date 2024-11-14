# LOAD LIBRARIES ----
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse)
library(sf)
library(leaflet)
library(bivariatechoropleths)
library(DT)
library(htmltools)
library(janitor)
library(markdown)
library(htmlwidgets)
library(future)
library(promises)

plan(multisession)

# READ DATA ----
levee_areas <- st_read("data/fixedLevees/leveedAreas.shp") %>% 
  st_transform(4326) %>% 
  clean_names()

additionalLayers <- future_promise({
  prob_fail <- st_read("data/probFailure/prob_fail_levee_area.shp") %>%
    st_transform(4326) %>%
    clean_names()

  structure_value <- st_read("data/structureValue/nsi_2024_total_value_levee_area.shp")%>%
    st_transform(4326) %>%
    rename(structure_value_area = strct__,
           total_value_trill = ttl_vl_) %>%
    clean_names()

  managed_wetlands <- st_read("data/managedWetlands/managedWetlands_perLevee.shp") %>%
    st_transform(4326) %>%
    clean_names()

  croplands <- st_read("data/deltaCropland/deltaCroplandValued_perLevee.shp") %>%
    st_transform(4326) %>%
    clean_names()

  soc_vul <- st_read("data/leveeLSDayPopWeightSVI/leveeLSDayPopWeightSVI.shp") %>%
    select(LMA, area, leveeAr, RPL_THEMES) %>%
    st_transform(4326) %>%
    clean_names()

  ## Layer map color pallets ----

  ## Probability of Failure
  prob_fail_pal <- colorNumeric(
    palette = "Reds",
    domain = prob_fail$lev_flr)

  # ## Managed Wetlands
  # rainbow <- c("#ff0000", "#ff5300", "#ffa500", "#ffd200", "#ffff00", "#80c000", 
  #              "#008000", "#004080", "#0000ff", "#2600c1", "#4b0082")
  # 
  # 
  # managed_wetlands_pal <- colorFactor(
  #   palette = rainbow,
  #   domain  = managed_wetlands$pm_lnd_t)
  # 
  # ## Croplands
  # # unique(croplands$type)
  # vivid <- c("#E58606", "#5D69B1", "#52BCA3", "#99C945", "#CC61B0", "#24796C", "#DAA51B")
  # 
  # 
  # crops_pal <- colorFactor(
  #   palette = vivid,
  #   domain  = croplands$type)
  
  ## Managed Wetlands
  managed_wetlands_pal <- colorBin(
    palette = "YlGn",
    domain = managed_wetlands$ovrlp_at
  )
  
  ## Cropland Valuation
  crops_pal <- colorBin(
    palette = "YlOrRd",
    domain = croplands$ttl_cr_vm
  )

  ## Social Vulnerability
  soc_vul_pal <- colorNumeric(
    palette = "viridis",
    domain = soc_vul$rpl_themes)

  ## structure value (bins)
  # range(structure_value$total_value_trill, na.rm = T)

  ## structure color pallet
  # bins <- c(0, 1, 10, 100, 1000, 20000, 30000, 40000, 70000)
  structure_pal <- colorBin("YlOrRd",
                            domain = structure_value$total_value_trill)
  
  ## Data for bichoropleth map
  prob_fail2 <- prob_fail %>%
    select(lma, lev_flr)
  
  structure_value2 <- structure_value %>%
    select(lma, total_value_trill) %>%
    st_drop_geometry() %>%
    mutate(lma = str_to_lower(lma))
  
  soc_vul2 <- soc_vul %>%
    select(lma, rpl_themes) %>%
    st_drop_geometry() %>%
    mutate(lma = str_to_lower(lma))
  
  ## combine data soc vul + other variable need to be in the same data frame
  bichoropleth_all_data <- prob_fail2 %>%
    left_join(structure_value2, by = "lma") %>%
    left_join(soc_vul2, by = "lma") %>%
    mutate(fail_percentile = percent_rank(lev_flr),
           soc_vul_percentile = percent_rank(rpl_themes),
           structure_val_percentile = percent_rank(total_value_trill))

  list(
    prob_fail = prob_fail,
    structure_value = structure_value,
    managed_wetlands = managed_wetlands,
    croplands = croplands,
    soc_vul = soc_vul,
    prob_fail_pal = prob_fail_pal,
    managed_wetlands_pal = managed_wetlands_pal,
    crops_pal = crops_pal,
    soc_vul_pal = soc_vul_pal,
    structure_pal = structure_pal,
    bichoropleth_all_data = bichoropleth_all_data
  )
})

## prep data for Data Explorer
data_explorer_table <- levee_areas %>% ## FIX!!
  # select(island_name = lma,
  #        levee_failure = lev_flr,
  #        social_vul,
  #        prob_levee_failure_prec = fail_percentile,
  #        soc_vul_percentile) %>%
  st_drop_geometry()

setShapeStyle <- function( map, data = getMapData(map), layerId,
                           stroke = NULL, color = NULL,
                           weight = NULL, opacity = NULL,
                           fill = NULL, fillColor = NULL,
                           fillOpacity = NULL, dashArray = NULL,
                           smoothFactor = NULL, noClip = NULL,
                           options = NULL
){
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray,
                               smoothFactor = smoothFactor, noClip = noClip
               )))
  # evaluate all options
  options <- evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  
  layerId <- options[[1]]
  style <- options[-1] # drop layer column
  
  #print(list(style=style))
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style);
}

setGroupStyle <- function(map, 
                          data = getMapData(map), 
                          groupName,
                          stroke = NULL, 
                          color = NULL,
                          weight = NULL, 
                          opacity = NULL,
                          fill = NULL, 
                          fillColor = NULL,
                          fillOpacity = NULL, 
                          dashArray = NULL,
                          smoothFactor = NULL, 
                          noClip = NULL,
                          options = NULL) {
  
  # Combine all style options
  styleOptions <- filterNULL(list(
    stroke = stroke,
    color = color,
    weight = weight,
    opacity = opacity,
    fill = fill,
    fillColor = fillColor,
    fillOpacity = fillOpacity,
    dashArray = dashArray,
    smoothFactor = smoothFactor,
    noClip = noClip
  ))
  
  # Combine with additional options if provided
  if (!is.null(options)) {
    styleOptions <- c(styleOptions, options)
  }
  
  # Evaluate formulas if present
  styleOptions <- evalFormula(styleOptions, data = data)
  
  # Convert to data frame
  styleDF <- do.call(data.frame, c(styleOptions, list(stringsAsFactors = FALSE)))
  
  # Invoke the JavaScript method
  leaflet::invokeMethod(map, data, "setStyleByGroup", "shape", groupName, styleDF)
}


### JS methods
leafletjs <-  tags$head(
  # add in methods from https://github.com/rstudio/leaflet/pull/598
  tags$script(HTML(
    '
window.LeafletWidget.methods.setStyle = function(category, layerId, style){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){ // in case a single layerid is given
    layerId = [layerId];
  }

  //convert columnstore to row store
  style = HTMLWidgets.dataframeToD3(style);
  //console.log(style);

  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){ // or should this raise an error?
      layer.setStyle(style[i]);
    }
  });
};

window.LeafletWidget.methods.setStyleByGroup = function(category, groupName, style){
  var map = this;
  if (!groupName) return;
  
  // Convert style data frame to JavaScript object
  style = HTMLWidgets.dataframeToD3(style)[0];  // Take first style object
  
  // Find all layers in the specified group
  var foundLayers = [];
  map.eachLayer(function(layer) {
    if (layer.options && layer.options.group === groupName) {
      layer.setStyle(style);
    }
  });
};


window.LeafletWidget.methods.setRadius = function(layerId, radius){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){ // in case a single layerid is given
    layerId = [layerId];
    radius = [radius];
  }

  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer("marker", d);
    if (layer){ // or should this raise an error?
      layer.setRadius(radius[i]);
    }
  });
};
'
  ))
)

## NEXT: BUILD DF FOR DATA EXPLORER TAB