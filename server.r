library(shiny)
library(tidyverse)
library(covid19R)
library(covid19nytimes)
library(USAboundaries)
library(sf)
library(leaflet)
library(RColorBrewer)
library(tidycensus)
library(plotly)
library(data.table)

#############################################
# Units function for ggplots
#############################################

addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3,1), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}


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


#############################################
# Helper function for leaflet
#############################################

factop <- function(x) {
  ifelse(is.na(x), 0, 1)
}




#############################################
# LOAD DATA FROM NY TIMES
#############################################

state_choices <- read_csv("data/state_choices.csv")

nytimes_counties_loaded <- refresh_covid19nytimes_counties()

#nytimes_counties_loaded <- read_csv("data/nytimes_test_data.csv")

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
                 
                 #select_state <- "Kentucky"
                 
                 select_state <- input$state_selection_input
                 
                 
                 
                 
                 nytimes_counties <- nytimes_counties_loaded %>% 
                   mutate(location_code = ifelse(location == "New York City,New York", "1", location_code))
                 
                 nytimes_counties <- nytimes_counties %>% 
                   filter(is.na(location_code) == FALSE) %>% 
                   mutate(state = str_extract(location, '\\b[^,]+$')) %>% 
                   filter(state != "Guam",
                          state != "Northern Mariana Islands",
                          state != "Puerto Rico",
                          state != "Virgin Islands") 
                 
                 # SEPARATE COUNTY INTO ITS OWN VECTOR
                 nytimes_counties$county <- sub(",.*$", "", nytimes_counties$location)
                 
                 ###########################################################
                 # SET UP VALUES FOR LOOP THAT GENERATES ROLLING 7 NUMBERS
                 ###########################################################
                 
                 unique_county <- nytimes_counties %>% 
                   filter(state == select_state) %>% 
                   select(location, location_code) %>% 
                   unique()
                 
                 unique_county$county <- sub(",.*$", "", unique_county$location)
                 
                 updateSelectInput(session, "county_selection_input", choices = unique_county$county)
                 
                 county_select <- input$county_selection_input
                 
                 rona_loop <- nytimes_counties[0,] %>% 
                   mutate(new_cases_R7 = as.character())
                 
                 count <- 1
                 
                 dataTypeSelection <- ifelse(input$casesDeathsButton == "New Cases R7", "cases_total", "deaths_total")
                 
                 #dataTypeSelection <- "cases_total"
                 
                 for(county_loop in unique_county$location_code) {
                   
                   
                   print(paste0(round(count/nrow(unique_county)*100,1),"%"))
                   
                   # ISOLATE COUNTY
                   
                   indiv_county <- nytimes_counties %>% 
                     filter(location_code == county_loop) %>% 
                     filter(data_type == dataTypeSelection) %>% 
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
                 
                 county_pop <- read_csv(paste0("data/census/", input$state_selection_input,"Population.csv"))
                 
                 county_pop <- county_pop %>% 
                   mutate(GEOID = as.character(GEOID))

                 
                 # SEPARATE COUNTY INTO ITS OWN VECTOR

                 
                 if(input$state_selection_input == "New York") {
                   
                   county_pop$county <- sub(" County,.*$", "", county_pop$NAME)
                   
                   county_pop <- county_pop %>% select(GEOID, county, estimate)
                   
                   county_pop <- county_pop %>% 
                     mutate(county = ifelse(county == "Bronx" |
                                              county == "Kings" |
                                              county == "New York" |
                                              county == "Queens" |
                                              county == "Richmond", "New York City", county)) %>%
                     mutate(GEOID = ifelse(county == "New York City", "1", GEOID)) %>%
                     group_by(GEOID) %>% 
                     select(GEOID, estimate) %>% 
                     summarize(estimate = sum(estimate), .groups = 'drop')
                   
                   
                 }
                 
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
                   mutate(log_new_cases_R7 = ifelse(is.na(new_cases_R7) == FALSE, ifelse(new_cases_R7 > 0, round(log1p(new_cases_R7),1),0), NA)  ) %>% 
                   mutate(cases_pcap_R7 =  ifelse(is.na(new_cases_R7) == FALSE,  round(new_cases_R7/population*100000, 0), NA) ) %>% 
                   mutate(log_cases_pcap_R7 =   ifelse(is.na(cases_pcap_R7) == FALSE,  ifelse(cases_pcap_R7 > 0, round(log1p(cases_pcap_R7),1),0),NA) ) %>% 
                   mutate(cases_phour =   ifelse(is.na(new_cases_R7) == FALSE,  ifelse(new_cases_R7 > 0, round(new_cases_R7/7/24,1),0),NA) ) %>%
                   mutate(labelText =
                            ifelse(is.na(new_cases_R7) == FALSE, 
                                   paste0("<b>County: </b>", county, "<br>",
                                          paste0("<b>", input$casesDeathsButton,": </b>"), new_cases_R7, "<br>",
                                          paste0("<b>", input$casesDeathsButton," Per Hour: </b>"), cases_phour),
                                   paste0("<b>County: </b>", county, "<br>",
                                          paste0("<b>", input$casesDeathsButton,": </b>"), "N/A", "<br>",
                                          paste0("<b>", input$casesDeathsButton," Per Hour: </b>"), "N/A"))) 
                 
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
                                     "white",
                                     "#FBD400", 
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
                 #county_map <- us_counties(states = select_state, resolution = "high")


                 county_map <- st_read(paste0("data/maps/", select_state,".shp")) 
                 
                 if(select_state == "New York") {
                   county_map <- county_map %>% 
                     mutate(name = ifelse(name == "Bronx" |
                                            name == "Kings" |
                                            name == "New York" |
                                            name == "Queens" |
                                            name == "Richmond", "New York City", name)) %>% 
                     mutate(geoid = ifelse(name == "New York City", "1", geoid)) %>% 
                     group_by(geoid, name) %>% 
                     summarize(.groups = 'drop') 
                 }
                 
                 
                 
                 # SELECT NEW CASES ON 1 DATE
                 
                 selectDate <- as.Date("2020-07-01")
                 
                 rona_plot_partial <- rona_plot_partial %>% 
                   select(date, 
                          name, 
                          location_code,
                          value,
                          population,
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
                 
                 
                 ########################################
                 # GENERATE STATE GGPLOT DATA
                 ########################################
                 
                 ggData <- rona_plot_partial %>% 
                   filter(date >= as.Date("2020-03-01")) %>% 
                   mutate(value = ifelse(is.na(value) == TRUE, 0, value),
                          new_cases_R7 = ifelse(is.na(new_cases_R7) == TRUE, 0, new_cases_R7)) %>% 
                   group_by(date) %>% 
                   summarize(cum_new_cases = sum(value),
                             new_cases_R7 = sum(new_cases_R7),
                             population = sum(population), .groups = 'drop') %>% 
                   mutate(cases_pcap = cum_new_cases/population*100) %>% 
                   mutate(tooltip = paste0("<b>Date: </b>", date, 
                                           "\n <b> Cumulative Cases: </b>", cum_new_cases,
                                           "\n <b>% of Pop. Infected: </b>", round(cases_pcap,1),"%")) %>% 
                   mutate(tooltip1 = paste0("<b>Date: </b>", date, 
                                            "\n <b>Cumulative Deaths: </b>", round(cum_new_cases,0)))  %>% 
                   mutate(tooltip2 = paste0("<b>Date: </b>", date, "\n <b>", input$casesDeathsButton,": </b>", round(new_cases_R7))) 
                 
                 
                 print("ggplot data generated")
                 
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
                                 animate = animationOptions(interval = 100, loop = FALSE),
                                 width = '100%'
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
                 
                 dataLabel <- reactive({
                   req(input$dateSel)
                   label = paste0("<center><b>", format(as.Date(input$dateSel), "%d %b %Y"), "</b></center>")
                 })
                 
                 filteredCountyData <- reactive({
                   req(input$county_selection_input)
                   
                   ########################################
                   # GENERATE COUNTY GGPLOT DATA
                   ########################################
                   
                   ggData_county <- rona_plot_partial %>% 
                     filter(date >= as.Date("2020-03-01"),
                            name == input$county_selection_input) %>% 
                     mutate(value = ifelse(is.na(value) == TRUE, 0, value),
                            new_cases_R7 = ifelse(is.na(new_cases_R7) == TRUE, 0, new_cases_R7)) %>% 
                     group_by(date, population) %>% 
                     summarize(cum_new_cases = sum(value),
                               new_cases_R7 = sum(new_cases_R7)) %>% 
                     mutate(cases_pcap = cum_new_cases/population*100) %>% 
                     mutate(cases_phour = new_cases_R7/7/24) %>% 
                     mutate(tooltip = ifelse(input$casesDeathsButton == "New Cases R7", 
                                             paste0("<b>Date: </b>", date, 
                                                    "\n <b> Cumulative Cases: </b>", cum_new_cases,
                                                    "\n <b>% County Pop. Infected: </b>", round(cases_pcap,1), "%"),
                                             paste0("<b>Date: </b>", date, "\n <b>Cumulative Deaths: </b>", round(cum_new_cases,0)))) %>% 
                     mutate(tooltip2 = paste0("<b>Date: </b>", date, "\n <b>", input$casesDeathsButton,": </b>", round(new_cases_R7)))
                   
                   print("county filtered")
                   
                   
                   return(ggData_county)
                   
                   
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
                               values = rona_colors$log_new_cases_R7,
                               opacity = 0.9,
                               title = input$casesDeathsButton,
                               labFormat = labelFormat(digits = 0, transform = function(x) expm1(x)),
                               position = "bottomleft") 
                   
                   # %>% 
                   #   addControl(html = paste0("<b>", "Date: ", "</b>"), 
                   #              position = "topright")

                   
                   
                   
                 })
                 
                 output$mapTitle <- renderText({"Interactive Map: use date slider or animate with play button at bottom right"})
                 
                 output$r7Plot <- renderPlotly({
                   
                   ########################################
                   # STATE - GENERATE R7 POPULATION PLOT
                   ########################################
                   
                   titleText1 <- ifelse(dataTypeSelection == "cases_total", 
                                       paste0(select_state, " New Cases R7"),
                                       paste0(select_state, " New Deaths R7"))
                   
                   yText <- ifelse(dataTypeSelection == "cases_total", 
                                   "New Cases R7",
                                   "New Deaths R7")
                   
                   base_plot <- ggData %>% 
                     ggplot(aes(x = date, y = new_cases_R7, text = tooltip2, group = 1)) +
                     geom_line() +
                     labs(
                       title = titleText1,
                       y = yText
                     ) +
                     theme_minimal() +
                     theme(axis.title.x = element_blank()) +
                     scale_y_continuous(labels = addUnits)
                   
                   ggplotly(base_plot, tooltip = "text") %>% 
                     config(displayModeBar = F) %>% 
                     layout(xaxis=list(fixedrange=TRUE)) %>% 
                     layout(yaxis=list(fixedrange=TRUE))
                   
                   
                   
                   
                 })
                 
                 output$cumulativePlot <- renderPlotly({
                   
                   ########################################
                   # STATE - GENERATE CUMULATIVE POPULATION PLOT
                   ########################################
                   
                   titleText2 <- ifelse(dataTypeSelection == "cases_total", 
                                       paste0(select_state, " Cumulative Cases"),
                                       paste0(select_state, " Cumulative Deaths"))
                   
                   if(dataTypeSelection == "cases_total") {
                     
                     base_plot <- ggData %>% 
                       ggplot(aes(x = date, y = cum_new_cases, text = tooltip, group = 1)) +
                       geom_line() +
                       labs(
                         title = titleText2,
                         y = "Cases"
                       ) +
                       theme_minimal() +
                       theme(axis.title.x = element_blank()) +
                       scale_y_continuous(labels = addUnits)
                     
                     ggplotly(base_plot, tooltip = "text") %>% 
                       config(displayModeBar = F) %>% 
                       layout(xaxis=list(fixedrange=TRUE)) %>% 
                       layout(yaxis=list(fixedrange=TRUE))
                     
                   } else{
                     
                     base_plot <- ggData %>% 
                       ggplot(aes(x = date, y = cum_new_cases, text = tooltip1, group = 1)) +
                       geom_line() +
                       labs(
                         title = titleText2,
                         y = "Deaths"
                       ) +
                       theme_minimal() +
                       theme(axis.title.x = element_blank()) +
                       scale_y_continuous(labels = addUnits)
                     
                     ggplotly(base_plot, tooltip = "text") %>% 
                       config(displayModeBar = F) %>% 
                       layout(xaxis=list(fixedrange=TRUE)) %>% 
                       layout(yaxis=list(fixedrange=TRUE))
                     
                     
                   }
                   
                   
                   
                   
                   
                   
                 })
                 
                 
                 output$r7Plot_county <- renderPlotly({
                   
                   ########################################
                   # COUNTY - GENERATE R7 POPULATION PLOT
                   ########################################
                   

                   
                   titleText1_county <- ifelse(dataTypeSelection == "cases_total", 
                                        paste0(input$county_selection_input, " New Cases R7"),
                                        paste0(input$county_selection_input, " New Deaths R7"))
                   
                   yText_county <- ifelse(dataTypeSelection == "cases_total", 
                                   "New Cases R7",
                                   "New Deaths R7")
                   
                   base_plot <- filteredCountyData() %>% 
                     ggplot(aes(x = date, y = new_cases_R7, text = tooltip2, group = 1)) +
                     geom_line() +
                     labs(
                       title = titleText1_county,
                       y = yText_county
                     ) +
                     theme_minimal() +
                     theme(axis.title.x = element_blank()) +
                     scale_y_continuous(labels = addUnits)
                   
                   ggplotly(base_plot, tooltip = "text") %>% 
                     config(displayModeBar = F) %>% 
                     layout(xaxis=list(fixedrange=TRUE)) %>% 
                     layout(yaxis=list(fixedrange=TRUE))
                   
                   
                   
                   
                 })
                 
                 output$cumulativePlot_county <- renderPlotly({
                   
                   ########################################
                   # COUNTY - GENERATE CUMULATIVE POPULATION PLOT
                   ########################################
                   
                   titleText2 <- ifelse(dataTypeSelection == "cases_total", 
                                        paste0(input$county_selection_input, " County Cumulative Cases"),
                                        paste0(input$county_selection_input, " County Cumulative Deaths"))
                   
                   yText_county2 <- ifelse(dataTypeSelection == "cases_total", 
                                          "Cases",
                                          "Deaths")
                   
                   
                   if(dataTypeSelection == "cases_total") {
                     
                     base_plot <- filteredCountyData() %>% 
                       ggplot(aes(x = date, y = cum_new_cases, text = tooltip, group = 1)) +
                       geom_line() +
                       labs(
                         title = titleText2,
                         y = yText_county2
                       ) +
                       theme_minimal() +
                       theme(axis.title.x = element_blank()) +
                       scale_y_continuous(labels = addUnits)
                     
                     ggplotly(base_plot, tooltip = "text") %>% 
                       config(displayModeBar = F) %>% 
                       layout(xaxis=list(fixedrange=TRUE)) %>% 
                       layout(yaxis=list(fixedrange=TRUE))
                     
                     
                   } else {
                     
                     base_plot <- filteredCountyData() %>% 
                       ggplot(aes(x = date, y = cum_new_cases, text = tooltip, group = 1)) +
                       geom_line() +
                       labs(
                         title = titleText2,
                         y = yText_county2
                       ) +
                       theme_minimal() +
                       theme(axis.title.x = element_blank()) +
                       scale_y_continuous(labels = addUnits)
                     
                     ggplotly(base_plot, tooltip = "text") %>% 
                       config(displayModeBar = F) %>% 
                       layout(xaxis=list(fixedrange=TRUE)) %>% 
                       layout(yaxis=list(fixedrange=TRUE))
                     
                   }
                   
                   
                   
                   
                   
                   
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
                   
                   output$dateText <- renderText({dataLabel()})
                   
                   print("values updated")
                   
                   leafletProxy("covid_map", data = rona_plot) %>%
                     clearMarkers() %>%
                     setShapeStyle(layerId = ~name,
                                   fillColor = ~color, 
                                   fillOpacity = ~factop(new_cases_R7),
                                   label = ~labelText) 
                   
                   # %>%
                   #   clearControls() %>% 
                   #   addControl(html = dataLabel(), position = "topright")
                   
                   print("map layers updated")
                   
                   
                 })
                 
                 
                 
                 
               })
  
  
  
  
  
  

}