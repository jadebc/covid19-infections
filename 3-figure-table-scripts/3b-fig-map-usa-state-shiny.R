#######################################
# COVID-19 estimated infections 
# correcting for incomplete testing and 
# imperfect test accuracy

# Map of COVID-19 cases vs. estimated
# infections by USA State

# Shiny app with slider for date
#######################################

rm(list=ls())

library(sp)
library(raster) # needed for ccodes, getdata
library(rgdal)
library(leaflet)
library(wesanderson) # for a nice color palette
library(sf) # for working with shape files
library(RColorBrewer)
library(maptools)
library(rgeos)
library(viridis)
library(tidyr)
library(shiny)
library(lubridate)
library(maptools)
library(tigris)

source(paste0(here::here(), "/0-config.R"))

# ------------------------------------------------------
# Read data
# ------------------------------------------------------
covid_usa_state_adjusted = readRDS(paste0(results_path, "covid_usa_state_adjusted.RDS"))
covid_usa_state_adjusted = covid_usa_state_adjusted %>% filter(state!="US")

#------------------------------------------------
# cumulative cases by day
#------------------------------------------------
# modify interval labels
label_interval <- function(breaks) {
  paste0(breaks[1:length(breaks) - 1], " - ", breaks[2:length(breaks)])
}

covid_usa_data = covid_usa_state_adjusted %>% 
  dplyr::select(date, state, statename, estimated_cases, positive, population) %>%
  rename(NAME_1 = statename) %>%
  mutate(obs_label = paste0("<b>", NAME_1, "</b><br>", 
                            sprintf("%0.3f", positive/population*1000), " cases per 1,000", "<br>",
                            format(positive, big.mark=",", digits=0, scientific=F, trim = TRUE), " observed cases"),
         ratio_label = paste0("<b>", NAME_1, "</b><br>", 
                              ifelse(positive==0, "No ratio", sprintf("%0.1f", estimated_cases/positive)),
                              ifelse(positive==0, "", " expected : observed"), "<br>",
                              format(estimated_cases, big.mark=",", digits=0, scientific=F, trim = TRUE), " expected cases", "<br>",
                              format(positive, big.mark=",", digits=0, scientific=F, trim = TRUE), " observed cases"))

#-------------------------------------
# read in map data
#-------------------------------------

# read in US state boundaries
USA_Adm_1 <- tigris:::states(cb = TRUE)
USA_Adm_1 <- st_as_sf(USA_Adm_1)

territories = c('AS', 'VI', 'MP', 'GU')
USA_Adm_1 <- USA_Adm_1 %>% filter(!USA_Adm_1$STUSPS %in% territories)
USA_Adm_1 <- as_Spatial(USA_Adm_1)

# read in lakes data from http://www.naturalearthdata.com/downloads/10m-physical-vectors/
great_lakes <- st_read(paste0(here::here(), "/1-data/ne_10m_lakes"))
great_lakes_subset <- great_lakes %>% filter(name_alt == "Great Lakes") %>%  
                                      filter(scalerank == 0) 

# define color palette
gnbu_colors = brewer.pal(n=6,"GnBu")[2:6]
orrd_colors = brewer.pal(n=6,"OrRd")[2:6]

#-------------------------------------
# define user interface - ratio map
#-------------------------------------
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(bottom = 10, left = 20,
                style="z-index:0;", # legend over my map (map z = 400)
                sliderInput(inputId = "dt",
                            label = "Date",
                            value = as.Date("2020-03-31"),
                            min = as.Date("2020-03-07"),
                            max = as.Date("2020-03-31")
                )
  )
)

#-------------------------------------
# define server - ratio map
#-------------------------------------
server <- function(input, output) {
  reactive_data <- reactive({
    # Filter data
    usa_data_bydate <- covid_usa_data %>%
                        filter(date == as.Date(input$dt)) %>%
                        arrange(NAME_1)

    usa_data_bydate$ratio = ifelse(usa_data_bydate$positive==0, NA, usa_data_bydate$estimated_cases / usa_data_bydate$positive)
    
    ratio_quantiles <- quantile(usa_data_bydate$ratio, c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm=TRUE)
    usa_data_bydate$ratio <- cut(usa_data_bydate$ratio,
                                      ratio_quantiles,
                                      include.lowest=TRUE,
                                      labels = label_interval(round(ratio_quantiles, 1))
                                )
    usa_data_bydate
    
  })
  
  reactive_data_SPDF <- reactive({
    # merge cases with shape file
    USA_shp = merge(USA_Adm_1, reactive_data(), by = "NAME_1")
    USA_shp
  })

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      # set zoom center point and level
      setView(lng = -100, lat = 39.0997, zoom = 4.2)
  })
  
  observe({
    if (nrow(reactive_data()) != 0) {
      colpal <- colorFactor(gnbu_colors, reactive_data()$ratio)
      
      proxy <- leafletProxy("map")
      
      proxy <- proxy %>%
        clearControls() %>%
        clearGroup("data") %>%
        addPolygons(data=reactive_data_SPDF(),
                    color = "#D3D3D3",
                    fillColor = ~colpal(reactive_data()$ratio),
                    weight = 1,
                    opacity = 1,
                    fillOpacity = 1,
                    highlightOptions = highlightOptions(color = "#FFFFFF", weight = 2,
                                                        bringToFront = TRUE),
                    label = ~lapply(reactive_data()$ratio_label, htmltools::HTML)) %>%
        addPolygons(data=great_lakes_subset,
                    color = "#D4DADC",
                    weight = 0.5,
                    opacity = 1,
                    fillOpacity = 1) %>%
        addLegend(data = reactive_data_SPDF(),
                 "bottomright",
                 pal = colpal,
                 values = ~reactive_data()$ratio,
                 title = "Expected:observed",
                 opacity = 1
        )
    }
  })
}

shinyApp(ui, server)


#-------------------------------------
# define server - obs map
#-------------------------------------
server <- function(input, output) {
  reactive_data <- reactive({
    # Filter data
    usa_data_bydate <- covid_usa_data %>%
      filter(date == input$dt) %>%
      arrange(NAME_1)
    
    usa_data_bydate$obs_case_perpop = usa_data_bydate$positive / usa_data_bydate$population * 1000
    
    obs_case_perpop_quantiles <- unique(quantile(usa_data_bydate$obs_case_perpop, c(0, 0.2, 0.4, 0.6, 0.8, 1)))
    
    usa_data_bydate$obs_case_perpop_cat = cut(usa_data_bydate$obs_case_perpop, 
                                      obs_case_perpop_quantiles,
                                      include.lowest=TRUE,
                                      labels = label_interval(round(obs_case_perpop_quantiles, 3))
    )

    usa_data_bydate
    
  })
  
  reactive_data_SPDF <- reactive({
    # merge cases with shape file
    USA_shp = merge(USA_Adm_1, reactive_data(), by = "NAME_1")
    USA_shp
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      # set zoom center point and level
      setView(lng = -100, lat = 39.0997, zoom = 4.2)
  })
  
  observe({
    if (nrow(reactive_data()) != 0) {
      colpal <- colorFactor(orrd_colors, reactive_data()$obs_case_perpop_cat)
      
      proxy <- leafletProxy("map")
      
      proxy <- proxy %>%
        clearControls() %>%
        clearGroup("data") %>%
        addPolygons(data=reactive_data_SPDF(),
                    color = "#D3D3D3",
                    fillColor = ~colpal(reactive_data()$obs_case_perpop_cat),
                    weight = 1,
                    opacity = 1,
                    fillOpacity = 1,
                    highlightOptions = highlightOptions(color = "#FFFFFF", weight = 2,
                                                        bringToFront = TRUE),
                    label = ~lapply(reactive_data()$obs_label, htmltools::HTML)) %>%
        addPolygons(data=great_lakes_subset,
                  color = "#D4DADC",
                  weight = 0.5,
                  opacity = 1,
                  fillOpacity = 1) %>%
        addLegend(data = reactive_data_SPDF(),
                  "bottomright",
                  pal = colpal,
                  values = ~reactive_data()$obs_case_perpop_cat,
                  title = "Cases per 1,000",
                  opacity = 1
        )
    }
  })
}

shinyApp(ui, server)

