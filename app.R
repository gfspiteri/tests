library(leaflet)
library(dplyr)
library(tidyverse)
library(zoo)
library(leaflegend)
library(DBI)
library(odbc)
library(sf)
library(devtools)
library(crosstalk)
library(shiny)
library(reactable)
library(shinyWidgets)
library(shinyjs)

devtools::source_url("https://raw.githubusercontent.com/EU-ECDC/EcdcColors/master/R/EcdcColors.R")
devtools::source_url("https://raw.githubusercontent.com/EU-ECDC/EcdcColors/master/R/SurvColors.R")

number_format <-function(number){
  if(number > 0 & number < 1){
    result <- format(round(number, 2), nsmall = 2)
  }else if(number >= 1 & number < 10){
    result <-  format(round(number, 1), nsmall = 1)
  }else if(number >= 10){
    result <- format(round(number, 0), nsmall = 0)
  } else   if(number < 0 & number > -1){
    result <- format(round(number, 2), nsmall = 2)
  }else if(number <= 1 & number > -10){
    result <-  format(round(number, 1), nsmall = 1)
  }else if(number <= 10){
    result <- format(round(number, 0), nsmall = 0)
  }
  return(result)
}

con <- dbConnect(odbc(), Driver = "ODBC Driver 17 for SQL Server", Server = "nvsql3t.ecdcnet.europa.eu", Database = "ref",
                 uid = "NCOV_Shiny", pwd = "Pandemonium$9000")
con_pop <- dbConnect(odbc(), Driver = "ODBC Driver 17 for SQL Server", Server = "nsql3.ecdcnet.europa.eu", Database = "DM_ref",
                     uid = "NCOV_Shiny", pwd = "Pandemonium$9000")

shapes <- dbGetQuery(con,"select [LocationGeometryPLGId]
      ,[LocationCode]
      ,[LocationType]
      ,[Shape_PLG_WKT_WGS84] 
      from [ref].[dLocationGeometryPLG]
                     where LocationType = 'Country'")

coordinates <- dbGetQuery(con, "SELECT [LocationCode]
	   ,[LocationName]
      ,[CountryISO2Code]
      ,[CentroidLatitude]
      ,[CentroidLongitude]
      ,[Centroid_PNT]
      ,[Centroid_PNT_WKT_LAEA]
      ,[Centroid_PNT_WKT_WGS84]  
      FROM [REF].[ref].[dLocationGeometryPNT]
  where LocationType = 'Country'")

population <- dbGetQuery(con_pop, "SELECT 
      [GeoCode]
      ,[TimeCode]
      ,[PopulationIndicatorCode]
      ,[Value] as populationValue
  FROM [DM_Ref].[ref].[Population]
  where TimeCode = '2022' and PopulationIndicatorCode = 'JAN_Age00_MAX' and GeoLevel = 2")

data_numbers <- vroom::vroom("cholera.csv", guess_max = 1000, na = "") %>%
  mutate(DateRep = as.yearmon(as.Date(DateRep, "%d/%m/%Y")), NewCases = as.integer(NewCases))

date_series <- zooreg(1:((max(data_numbers$DateRep) - min(data_numbers$DateRep))*12), min(data_numbers$DateRep), freq = 12)
date_series <- fortify.zoo(date_series)$Index
default_dates <- date_series[c(length(date_series) - 2, length(date_series))]
default_date_series <- date_series[seq(which(date_series == min(default_dates)), which(date_series == max(default_dates)))]

date_series <- as.character(date_series)
default_dates <- as.character(default_dates)
default_date_series <- as.character(default_date_series)

data_notes <- vroom::vroom("cholera_RT_Report.csv", guess_max = 1000, na = "") %>%
  mutate(DateRep = as.yearmon(paste0(Year, " ", Month), "%Y %B"),
         Update = str_replace_all(Update, "\xa0", " "),
         Update = str_trim(Update))

data_combined <- data_numbers %>% left_join(data_notes, by = c("DateRep" = "DateRep", "GeoId" = "GeoID"))

cases <- data_combined %>% 
  left_join(population, by = c("GeoId" = "GeoCode")) %>%
  mutate(DateRep = as.character(DateRep)) %>%
  filter(NewCases > 0) %>% #to avoid negatives  
  left_join(coordinates, by = c("GeoId" = "CountryISO2Code")) %>%
  left_join(shapes, by = c("GeoId" = "LocationCode")) %>%
  st_as_sf(wkt = "Shape_PLG_WKT_WGS84")

st_crs(cases) <- 4326

prepare_data <- function(cases, date_selection, country_selection){
  if(is.na(country_selection)){
    country_selection <- unique(cases$LocationName)
  }
  cases_filtered <- cases %>% filter(DateRep %in% date_selection, LocationName %in% country_selection)
  
  cases_totals <- as_tibble(cases_filtered) %>% 
    group_by(LocationName) %>%
    summarise(TotalCases = sum(NewCases, na.rm = T), populationValue = max(populationValue, na.rm = T)) %>%
    mutate(notificationRate = 100000 * TotalCases/populationValue) %>%
    select(-populationValue)
  
  cases_labels <- cases_filtered %>% 
    right_join(cases_totals, by = "LocationName") %>%
    mutate(label = paste0(LocationName, ": ", TotalCases, if_else(TotalCases == 1, " case", " cases"),
                          "<br> Notification rate: ",  number_format(notificationRate), " per 100 000 persons"),
           CountryLabel = paste0(LocationName, "<br> <i>Number of cases: ", TotalCases, if_else(TotalCases == 1, " case", " cases"),
                                 "<br> Notification rate: ",  number_format(notificationRate), " per 100 000 persons </i>"))
  
  return(cases_labels)
}

bins <- c(0, 5, 10, 15, Inf)

cases_default <- prepare_data(cases, date_selection = default_date_series, country_selection = NA)


base_map <-function(cases, bins) {
  
  cases <- cases %>% group_by(LocationName) %>% slice(1)
  
  pal <- colorBin(rev(EcdcColors(n = length(bins), col_scale = "red")), domain = cases$notificationRate, bins = bins)
  
  leaflet() %>% 
    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
    addPolygons(data = cases, 
                fillColor = ~pal(notificationRate),
                stroke = F,
                highlightOptions = highlightOptions(
                  weight = 5,
                  color = "#666",
                  fillOpacity = 0.7,
                  bringToFront = T),
                popup = ~label,
                layerId = ~LocationName) %>%
    addLegend(values = cases$notificationRate, pal = pal)
}

base_reactable <- function(cases){
  reactable(as_tibble(cases) %>% select(CountryLabel, DateRep, Update),
            onClick = "select",
            columns = list(
              CountryLabel = colDef("Country", style = JS("function(rowInfo, colInfo, state) {
        const firstSorted = state.sorted[0]
        // Merge cells if unsorted or sorting by country
        if (!firstSorted || firstSorted.id === 'CountryLabel') {
          const prevRow = state.pageRows[rowInfo.viewIndex - 1]
          if (prevRow && rowInfo.row['CountryLabel'] === prevRow['CountryLabel']) {
            return { visibility: 'hidden' }
          }
        }
      }"), html = T),
              #    CountryLabel = colDef("Country", show = T, html = T),
              DateRep = colDef("Date", show = T),
              Update = colDef("Update", show = T, html = T)
            ))
}


ui <- fluidPage(
  useShinyjs(),
  leafletOutput("map"),
  p(),
  uiOutput("controls"),
  p(),
  div(sliderTextInput(
    inputId = "date",
    label = "Select time period:", 
    choices = date_series,
    selected = default_dates,
    width = "80%"
  ), align = "center"),
  p(),
  reactableOutput("table")
)

server <- function(input, output, session) {
  
  # produce default map and table
  react_map <- base_map(cases_default, bins = bins)
  
  output$map <- renderLeaflet({
    react_map
  })
  
  react_table <- base_reactable(cases_default)
  
  output$table <- renderReactable({
    react_table
  })
  
  # react to changes in data and map selection
  date_selection <- reactive({
    date_series[seq(which(date_series == min(input$date)), which(date_series == max(input$date)))]
  })
  
  map_data <- reactiveValues(clickedShape=NULL)
  
  observeEvent(input$map_shape_click,
               {
                 map_data$clickedShape$id <- input$map_shape_click$id
                 print(map_data$clickedShape$id)
               })
  
  
  observeEvent(
    c(input$date, map_data$clickedShape, input$reset), {
      print(map_data$clickedShape$id)
      print(date_selection())
      
      if(!is.null(map_data$clickedShape$id)) {
        
        showElement('controls')
        
        print("This is what we have clicked")
        print(map_data$clickedShape$id)
        
      }
      
      processed_data <- prepare_data(cases, date_selection = date_selection(), country_selection = if_else(is.null(map_data$clickedShape$id), NA_character_, map_data$clickedShape$id))
      
      
      pal <- colorBin(rev(EcdcColors(n = length(bins), col_scale = "red")), domain = cases$notificationRate, bins = bins)
      
      bounds <- processed_data %>% pull(Shape_PLG_WKT_WGS84)
      bounds_x_min <- unname(st_bbox(bounds)$xmin)
      bounds_x_max <- unname(st_bbox(bounds)$xmax)
      bounds_y_min <- unname(st_bbox(bounds)$ymin)
      bounds_y_max <- unname(st_bbox(bounds)$ymax)
      
      leafletProxy("map", data = processed_data) %>%
        clearShapes() %>%
        addPolygons(fillColor = ~pal(notificationRate),
                    stroke = F,
                    highlightOptions = highlightOptions(
                      weight = 5,
                      color = "#666",
                      fillOpacity = 0.7,
                      bringToFront = T),
                    popup = ~label,
                    layerId = ~LocationName) %>%
        fitBounds(lng1 = bounds_x_min, lng2 = bounds_x_max, lat1 = bounds_y_min, lat2 = bounds_y_max)
      
      
      updateReactable("table", data = processed_data)
      
      
      output$controls <- renderUI({
        req(input$map_shape_click)
        absolutePanel(id = "controls", top = 360, left = 20,
                      right = "auto", bottom = "auto", width = "auto", height = "auto",
                      actionButton(inputId = "reset", label = "Clear selection", class = "btn-primary")
        )
      })
      
      
      if(req(input$reset)){
        
        #hiding the control button
        hideElement("controls")
        
        map_data$clickedShape <- NULL
        
        bounds_x_min_cases <- unname(st_bbox(processed_data)$xmin)
        bounds_x_max_cases <- unname(st_bbox(processed_data)$xmax)
        bounds_y_min_cases <- unname(st_bbox(processed_data)$ymin)
        bounds_y_max_cases <- unname(st_bbox(processed_data)$ymax)
        
        print("Getting palette ready")
        pal <- colorBin(rev(EcdcColors(n = length(bins), col_scale = "red")), domain = processed_data$notificationRate, bins = bins)
        print("Palette ready")
        leafletProxy("map", data = processed_data) %>%
          addPolygons(fillColor = ~pal(notificationRate),
                      stroke = F,
                      highlightOptions = highlightOptions(
                        weight = 5,
                        color = "#666",
                        fillOpacity = 0.7,
                        bringToFront = T),
                      popup = ~label,
                      layerId = ~LocationName) %>%
          fitBounds(lng1 = bounds_x_min_cases, lng2 = bounds_x_max_cases, lat1 = bounds_y_min_cases, lat2 = bounds_y_max_cases)
        
        print("Map updated")
        
        updateReactable("table", data = processed_data)
        
      }
      
    }, ignoreNULL = T)
  
  
  
  
  
  
}

shinyApp(ui, server)
