library(shiny)
library(tidyverse)
library(covid19R)
library(USAboundaries)
library(sf)
library(leaflet)
library(RColorBrewer)
library(tidycensus)
library(shinycssloaders)

#############################################
# Helper function for choropleth animation
#############################################

setShapeStyle <- function( map, data = getMapData(map), layerId,
                           stroke = NULL, color = NULL,
                           weight = NULL, opacity = NULL,
                           fill = NULL, fillColor = NULL,
                           fillOpacity = NULL, dashArray = NULL,
                           smoothFactor = NULL, noClip = NULL, label = NULL,
                           options = NULL){
  
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray,
                               smoothFactor = smoothFactor, noClip = noClip, label = label
               )))
  
  options <- evalFormula(options, data = data)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  
  layerId <- options[[1]]
  style <- options[-1]
  if("label" %in% colnames(style)){
    labelData = style[,"label", FALSE]
    style = style[,-which(colnames(style)=="label"), FALSE]
    leaflet::invokeMethod(map, data, "setLabel", "shape", layerId, label)
  }
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style);
}




factop <- function(x) {
  ifelse(is.na(x), 0, 1)
}

#############################################
# LOAD DATA FROM NY TIMES
#############################################

nytimes_counties <- get_covid19_dataset("covid19nytimes_counties")

nytimes_counties <- nytimes_counties %>% 
  filter(is.na(location_code) == FALSE) %>% 
  mutate(state = str_extract(location, '\\b[^,]+$')) %>% 
  filter(state != "Guam",
         state != "Northern Mariana Islands",
         state != "Puerto Rico",
         state != "Virgin Islands") 

# SEPARATE COUNTY INTO ITS OWN VECTOR
nytimes_counties$county <- sub(",.*$", "", nytimes_counties$location)

state_choices <- nytimes_counties %>% 
  select(state) %>% 
  unique() %>% 
  arrange(state)

print("data extracted")


server <- function(input, output, session) {
  
  #############################################
  # POPULATE DROP DOWN SO USER CAN SELECT STATE
  #############################################
  
  updateSelectInput(session, "state_selection_input", choices = state_choices$state)
  
  #########################################################
  # SET UP DATA BASED ON STATE SELECTION AND BUTTON PRESS
  #########################################################
  
  observeEvent(c(input$generatePlotButton),
               {
                 req(input$state_selection_input)
                 
                 #select_state <- "California"
                 
                 select_state <- input$state_selection_input
                 
                 
                 ###########################################################
                 # SET UP VALUES FOR LOOP THAT GENERATES ROLLING 7 NUMBERS
                 ###########################################################
                 
                 unique_county <- nytimes_counties %>% 
                   filter(state == select_state) %>% 
                   select(location, location_code) %>% 
                   unique()
                 
                 rona_loop <- nytimes_counties[0,] %>% 
                   mutate(new_cases_R7 = as.character())
                 
                 count <- 1
                 
                 
                 
                 for(county_loop in unique_county$location_code) {
                   
                   #county_loop <- "06001"
                   
                   print(paste0(round(count/nrow(unique_county)*100,1),"%"))
                   
                   # ISOLATE COUNTY
                   
                   indiv_county <- nytimes_counties %>% 
                     filter(location_code == county_loop) %>% 
                     filter(data_type == "cases_total") %>% 
                     arrange(date)
                   
                   row_count <- nrow(indiv_county)
                   
                   # REVERSE CUM SUM LOOP
                   new_cases_R7 <- c()
                   
                   #i <- 7
                   
                   for (i in 7:row_count) {
                     
                     value <- as.numeric(indiv_county[i,7] - indiv_county[i-6,7])
                     
                     value <- ifelse(value < 0, 0, value)
                     
                     new_cases_R7 <- c(new_cases_R7, value) 
                   }           
                   
                   indiv_county <- cbind(indiv_county[7:row_count,], new_cases_R7)
                   
                   rona_loop <- rbind(rona_loop, indiv_county)
                   
                   count <- count+1
                   
                 }
                 
                 #############################################
                 # RUN ANOTHER LOOP SO ALL COUNTIES ARE REPRESENTED IN
                 # VECTOR FOR EACH DATE. SET NA VALUES FOR COUNTIES
                 # THAT DON'T HAVE DATA YET
                 #############################################
                 
                 
                 allDates <- rona_loop %>% 
                   select(date) %>% 
                   unique() %>% 
                   arrange(date)
                 
                 #i<- allDates$date[1]
                 
                 date_loop <- rona_loop[0,] %>% 
                   select(location,
                          location_code,
                          date,
                          value,
                          new_cases_R7
                   )
                 
                 for (i in 1:nrow(allDates)) {
                   
                   dateFilter <- allDates$date[i]
                   
                   filtered <- rona_loop %>% 
                     filter(date == dateFilter) %>% 
                     select(-location, -state, -county, -location_type, -location_code_type, -data_type)
                   
                   filtered <- unique_county %>% 
                     left_join(filtered, by = "location_code") %>% 
                     mutate(date = dateFilter)
                   
                   date_loop <- rbind(date_loop, filtered)
                   
                 }
                 
                 # SEPARATE COUNTY INTO ITS OWN VECTOR
                 date_loop$county <- sub(",.*$", "", date_loop$location)
                 
                 print("loops completed")
                 
                 ###############################################
                 # GET COUNTY POPULATION DATA AND JOIN TO TABLE
                 ###############################################
                 
                 county_pop <- get_acs(geography = "county", 
                                       variables = "B01003_001", 
                                       state = select_state,
                                       geometry = FALSE)
                 
                 county_pop <- county_pop %>% select(GEOID, estimate)
                 
                 colnames(county_pop) <- c("location_code", "population")
                 
                 rona_plot_partial <- date_loop %>% 
                   left_join(county_pop, by = "location_code")
                 
                 print("added population data")
                 
                 #############################################
                 # FEATURE ENGINEERING
                 #    - log(new cases R7)
                 #    - cases per 100K
                 #    - log(cases per 100K)
                 #    - map label
                 #############################################
                 
                 rona_plot_partial <- rona_plot_partial %>% 
                   mutate(log_new_cases_R7 = ifelse(is.na(new_cases_R7) == FALSE, ifelse(new_cases_R7 > 0, round(log(new_cases_R7),1),0), NA)  ) %>% 
                   mutate(cases_pcap_R7 =  ifelse(is.na(new_cases_R7) == FALSE,  round(new_cases_R7/population*100000, 0), NA) ) %>% 
                   mutate(log_cases_pcap_R7 =   ifelse(is.na(cases_pcap_R7) == FALSE,  ifelse(cases_pcap_R7 > 0, round(log1p(cases_pcap_R7),1),0),NA) ) %>% 
                   mutate(labelText =
                            ifelse(is.na(new_cases_R7) == FALSE, 
                                   paste0("<b>County: </b>", county, "<br>",
                                          "<b>New Cases R7: </b>", new_cases_R7, "<br>",
                                          "<b>New Cases R7 Per 100K: </b>", cases_pcap_R7),
                                   paste0("<b>County: </b>", county, "<br>",
                                          "<b>New Cases R7: </b>", "N/A", "<br>",
                                          "<b>New Cases R7 Per 100K: </b>", "N/A"))) 
                 
                 print("feature engineering complete")
                 
                 #############################################
                 # ASSIGN COLORS AS HEX VALUE TO EACH ROW
                 #############################################
                 casesIndex <- rona_plot_partial %>%
                   filter(is.na(new_cases_R7) == FALSE ) %>% 
                   select(log_new_cases_R7) %>%
                   unique() %>%
                   arrange(log_new_cases_R7)
                 
                 
                 casesIndex <- casesIndex %>%
                   mutate(rank = 1:nrow(casesIndex))
                 
                 
                 
                 gradientColors <- c("white", 
                                     #"yellow", 
                                     #"orange", 
                                     "red", 
                                     "#1D1238")
                 
                 gradientFunction <- colorRampPalette(gradientColors)
                 
                 colorIndex <- as.data.frame(gradientFunction(nrow(casesIndex))) %>% 
                   mutate(rank = 1:nrow(casesIndex))
                 
                 colnames(colorIndex) <- c("color", "rank")
                 colorIndex$color <- as.character(colorIndex$color)
                 
                 
                 rona_plot_partial <- rona_plot_partial %>% 
                   left_join(casesIndex, by = "log_new_cases_R7") %>% 
                   left_join(colorIndex, by = "rank") 
                 
                 
                 colnames(rona_plot_partial)[colnames(rona_plot_partial) == 'county'] <- 'name'
                 
                 
                 rona_colors <- rona_plot_partial %>% 
                   as.data.frame() %>% 
                   select(log_new_cases_R7, color) %>% 
                   unique() %>% 
                   arrange(log_new_cases_R7)
                 
                 palLegend <- colorNumeric(gradientColors, domain = rona_colors$log_new_cases_R7, na.color = NA)
                 
                 print("color assignment complete")
                 
                 
                 #############################################
                 # SET UP SPACIAL DATA
                 #############################################
                 
                 
                 # GET COUNTY SPACIAL DATA
                 county_map <- us_counties(states = select_state, resolution = "high")
                 
                 
                 # SELECT NEW CASES ON 1 DATE
                 
                 selectDate <- as.Date("2020-07-01")
                 
                 rona_plot_partial <- rona_plot_partial %>% 
                   select(date, 
                          name, 
                          location_code,
                          new_cases_R7, 
                          log_new_cases_R7, 
                          cases_pcap_R7, 
                          log_cases_pcap_R7, 
                          color, 
                          labelText, 
                          rank)
                 
                 rona_plot_filtered <-  rona_plot_partial %>% filter(date == selectDate) 
                
                 
                 # MERGE RONA DATAFRAME FILTERED FOR ONE DATE WITH SPACIAL DATA
                 
                 rona_plot <- county_map %>% merge(rona_plot_filtered)
                 
                 print("map file generated")
                 
                 #############################################
                 # DATE SLIDER HANDLING
                 #############################################
                 
                 observe({
                   

                   
                   # FILTER DATES BEFORE MARCH 
                   
                   allDates <- rona_loop %>% 
                     filter(date >= as.Date("2020-03-01")) %>% 
                     select(date) %>% 
                     unique() %>% 
                     arrange(date)
                   
                   output$dateUI <- renderUI({
                     sliderInput("dateSel", "Date",
                                 min = min(allDates$date),
                                 max = max(allDates$date),
                                 value = min(allDates$date),
                                 step = 1,
                                 timeFormat = "%d %b %y",
                                 animate = animationOptions(interval = 500, loop = FALSE)
                     )
                   })
                   
                   print("slider calculated")
                 })
                 
                 #############################################
                 # FILTER DATA BASED ON SLIDER DATE
                 #############################################
                 
                 filteredData <- reactive({
                   req(input$dateSel)
                   
                   #selectDate <- "2020-04-01"
                   
                   selectDate <- input$dateSel
                   
                   dat <- rona_plot_partial %>% 
                     filter(date == selectDate) 
                   
                   print("date filtered")
                   
                   
                   return(dat)

                   
                 })
                 
                 ################################################
                 # GENERATE BASE MAP
                 ################################################
                 output$covid_map <- renderLeaflet({
                  
                   print("generate base map")
                   
                   leaflet(rona_plot) %>% 
                     addProviderTiles(provider = "CartoDB.VoyagerNoLabels") %>% 
                     addPolygons(layerId = ~name,
                                 fillColor = "lightgray",
                                 stroke = TRUE,
                                 fillOpacity = 1,
                                 color = "grey",
                                 weight = 1) %>% 
                     addLegend(pal = palLegend, 
                               values = rona_plot$log_new_cases_R7,
                               opacity = 0.9,
                               title = "Log(New Cases R7)",
                               position = "bottomleft")
                   
                   
                   
                 })
                 
                 ################################################
                 # UPDATE BASE MAP WITH NEWLY FILTERED DATA
                 ################################################
                 
                 observe({
                   
                   rona_plot$log_new_cases_R7 <- 
                     filteredData()$log_new_cases_R7[match(rona_plot$geoid, 
                                                         filteredData()$location_code)]
                   rona_plot$labelText <- 
                     filteredData()$labelText[match(rona_plot$geoid, 
                                                         filteredData()$location_code)]
              
                   rona_plot$cases_pcap_R7 <- 
                     filteredData()$cases_pcap_R7[match(rona_plot$geoid, 
                                                      filteredData()$location_code)]
                   
                   rona_plot$color <- 
                     filteredData()$color[match(rona_plot$geoid, 
                                                        filteredData()$location_code)]
                   
                   print("values updated")
                   
                   leafletProxy("covid_map", data = rona_plot) %>%
                     clearMarkers() %>%
                     setShapeStyle(layerId = ~name,
                                   fillColor = ~color, 
                                   fillOpacity = ~factop(new_cases_R7),
                                   label = ~lapply(labelText, 
                                                   htmltools::HTML))
                   
                   print("map layers updated")
                   
                   
                 })
                 
                 
                 
                 
               })
  
  
  
  
  
  

}