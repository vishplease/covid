library(leaflet)
library(shinydashboard)
library(shinybusy)
library(plotly)
library(shinydashboardPlus)

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

ui <- dashboardPagePlus(
  header = dashboardHeaderPlus(disable = TRUE, enable_rightsidebar = FALSE),
  sidebar = dashboardSidebar(width = "0px"),
  footer = dashboardFooter(
    left = HTML("Vishal Chandawarkar | <a href='https://www.vishplease.com/data-visualization'>vishplease.com</a>"),
    right = HTML("Sources: <a href='https://github.com/nytimes/covid-19-data'>NY Times</a>, <a href='https://www.census.gov/programs-surveys/acs/news/data-releases.html'>2019 ACS Estimates</a>")
  ),
  body = dashboardBody(
    tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important; }")),
    tags$style(
      HTML('
         #buttons {
         margin-top:10px; vertical-align: middle; opacity:1; height:50px; z-index:5;
         }

         ')
    ),
    add_busy_bar(color = "#e89f3c"),
    
    #############################################
    # INPUTS
    #############################################
    
    fluidRow( 
             
      boxPlus(width = 12,
          title = "U.S. COVID 19 Case Development",
          footer = h6(HTML("<center>Note: Rolling 7 (R7) is the sum of cases/deaths in a 7-day time window to account for
                           a lag between exposure and initial symptoms<br/> </center>")),
          status = "primary",
          solidHeader = TRUE,
          
          column(width = 4, align = "center",
                 selectInput(inputId = "state_selection_input",
                                label = HTML("Step 1: Choose State </br> <h6>Takes a few seconds while data loads</h6"), 
                                choices = NULL)
                 ),
          
          column(width = 4, align = "center",
                 radioButtons(inputId = "casesDeathsButton",
                                 label = "Step 2: Select Data",
                                 choices = c("New Cases R7", "New Deaths R7"),
                                 selected = "New Cases R7",
                                 inline = F),
                 ),
          
          column(width = 2, align = "center", id = "buttons",
                 actionButton("generatePlotButton", HTML("<b>Step 3: Generate Plots </b> <br/> (please be patient)") )))),
    
    #############################################
    # INTERACTIVE MAP
    #############################################
    
    fluidRow(box(width = 12,
                 title = textOutput("mapTitle"),
                 status = "primary",
                 solidHeader = TRUE,
                 h4(htmlOutput('dateText')),
                 leafletjs,
                 leafletOutput("covid_map", width = "100%", 
                               height = "350"),
                 uiOutput("dateUI"))),
    
    #############################################
    # STATEWIDE PLOTS
    #############################################
    
    fluidRow(box(width = 6,
                 solidHeader = TRUE,
                 status = "warning",
                 plotlyOutput("r7Plot")),
    
    box(width = 6,
                 solidHeader = TRUE,
                 status = "warning",
                 plotlyOutput("cumulativePlot"))),
    
    #############################################
    # COUNTY DRILLDOWN PLOTS
    #############################################
    
    fluidRow(column(width = 4),
             box(width = 4, 
                 title = "County Drilldown",
                 status = "primary",
                 solidHeader = TRUE,
                        selectInput(inputId = "county_selection_input",
                                    label = "Choose County", 
                                    choices = NULL)),
             column(width = 4)),
    
    fluidRow(box(width = 6,
                 solidHeader = TRUE,
                 status = "warning",
                 plotlyOutput("r7Plot_county")),

             box(width = 6,
                 solidHeader = TRUE,
                 status = "warning",
                 plotlyOutput("cumulativePlot_county"))
             )
  
    
    
  )
  )
  

