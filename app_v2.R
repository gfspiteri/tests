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
  reactable(as_tibble(cases) %>% select(DateRep, CountryLabel, Update),
            onClick = "select",
            columns = list(
              CountryLabel = colDef("Country", show = T, html = T),
              DateRep = colDef("Date", show = T),
              Update = colDef("Update", show = T, html = T)
            ))
}
