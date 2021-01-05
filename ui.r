library(leaflet)

#helper function in JS for choropleth animation
leafletjs <-  tags$head(
    tags$script(HTML('
  
window.LeafletWidget.methods.setStyle = function(category, layerId, style){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  style = HTMLWidgets.dataframeToD3(style);
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.setStyle(style[i]);
    }
  });
};
window.LeafletWidget.methods.setLabel = function(category, layerId, label){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.unbindTooltip();
      layer.bindTooltip(label[i])
    }
  });
};
'
    ))
)

##########################################
# UI
##########################################

ui <- fluidPage(
    
    leafletjs,
    
    titlePanel("COVID 19 Case Development"),
    
    sidebarPanel(width = 2,
                 
                  selectInput(inputId = "state_selection_input",
                             label = "Choose State (may take a few seconds to populate)", 
                             choices = NULL),
                 
                 actionButton("generatePlotButton", "Generate Plot"),
                 
                 uiOutput("dateUI")
                 
                 ),
    
    mainPanel(width = 10,
              
              leafletOutput("covid_map", width = "70%", height = "750px")
              
              )
    
    
        
        
)

