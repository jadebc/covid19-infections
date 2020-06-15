#######################################
# COVID-19 expected cases 

# US county data map
#######################################
library(rgdal)
library(rgeos)
library(raster)
library(leaflet)
library(viridis)

rm(list=ls())

## TO DO : I think the county pop is wrong 

source(paste0(here::here(), "/0-config.R"))

# covid_usa_state_adjusted = readRDS(paste0(results_path, "covid_usa_state_adjusted.RDS"))
covid_usa_county <- read.table(county_data_path, sep=",", header = TRUE)

county_geo <- read.csv(county_geocode_path)

# aggregate lat long by county
county_geo = county_geo %>% group_by(state, county) %>%
  summarise(latitude = mean(latitude),
            longitude = mean(longitude),
            pop = sum(estimated_population)) %>%
  ungroup()

# pre-process data 
state_name = data.frame(statename = state.name,
                        state = state.abb)

county_geo = county_geo %>% left_join(state_name, by = "state")

drops = names(table(county_geo$state[is.na(county_geo$statename)]))
drops = drops[-which(drops == "DC")]

county_geo = county_geo %>% filter(!state %in% drops) %>%
  dplyr::select(-state) %>%
  rename(state = statename)

# TEMPORARY cumulative
# change to date slider later
covid_usa_county = covid_usa_county %>%
  group_by(state, county) %>%
  summarise(cases = sum(cases))

# merge case counts with population
covid_county_df = covid_usa_county %>% 
  left_join(county_geo %>% 
              dplyr::select(latitude, longitude, state, county, pop), 
            by = c("state", "county")) %>%
  filter(!is.na(latitude)) %>%
  mutate(inc = cases/pop * 1000) %>%
  mutate(inc = ifelse(pop==0, NA, inc))

county_SPDF <- SpatialPointsDataFrame(coords = covid_county_df[,c("longitude", "latitude")],
                                      data = covid_county_df[,c("cases","inc")],
                                      proj4string = CRS("+init=epsg:4326")) 

# read in county state boundaries
USA_Adm_2 <- raster::getData("GADM", country="USA", level=2) 

# merge incidence with shape file 
covid_usa_county_shp = covid_county_df %>%
  rename(NAME_1 = state,
         NAME_2 = county) %>%
  filter(!is.na(cases)) %>%
  filter(!is.na(inc)) %>%
  # drop if cases > pop
  # TEMP dropping aberrant estimate
  filter(inc<=200)

county_shp = merge(USA_Adm_2, covid_usa_county_shp, 
                   by = c("NAME_1", "NAME_2")) 

county_shp = county_shp[!is.na(county_shp$inc),]

##############################################
# map_exp_usa = leaflet(USA_shp) %>%
#   
#   addProviderTiles(provider = "CartoDB.Positron") %>%
#   
#   # set zoom center point and level
#   setView(lng = -94.5786, lat = 39.0997, zoom = 3.3) %>%
#   
#   # add state case count polygons
#   addPolygons(
#     data=USA_shp,
#     color = "#585858",
#     fillColor=  ~exp_cases_pal(USA_shp$estimated_cases),
#     weight = 1,
#     opacity = 1,
#     fillOpacity = 1,
#     highlightOptions = highlightOptions(color = "#5A5A5A", weight = 2,
#                                         bringToFront = TRUE)) %>%
#   
#   # add legend
#   addLegend("bottomleft",
#             pal = exp_cases_pal,
#             values = ~USA_shp$estimated_cases,
#             title = "Cases",
#             opacity = 1
#   )
# 
# 
# map_exp_usa


##############################################
map_obs_usa = leaflet(county_SPDF) %>%
  
  addProviderTiles(provider = "CartoDB.Positron") %>%
  
  # set zoom center point and level
  setView(lng = -94.5786, lat = 39.0997, zoom = 3.3) %>%
  
  # addPolygons(
  #   data=USA_shp,
  #   color = "#585858",
  #   fillColor=  ~obs_cases_pal(USA_shp$positive),
  #   weight = 1,
  #   opacity = 1,
  #   fillOpacity = 1,
  #   highlightOptions = highlightOptions(color = "#5A5A5A", weight = 2,
  #                                       bringToFront = TRUE)) %>%
  
  # add state case count circles
  addCircleMarkers(data=county_SPDF,
                   color="red", 
                   weight = 0.5,
                   # fillColor = "transparent",
                   # fillOpacity = 0,
                   stroke = TRUE,
                   radius = county_SPDF$cases/800)

  # to do : add legend for circle size


map_obs_usa

##############################################

# define color palette
obs_inc_pal = colorNumeric(palette = "YlOrRd", 
                           domain = county_shp$inc, 
                           na.color = "#B9BBB9")

map_obs_inc_usa = leaflet(county_shp) %>%
  
  addProviderTiles(provider = "CartoDB.Positron") %>%
  
  # set zoom center point and level
  setView(lng = -94.5786, lat = 39.0997, zoom = 3.3) %>%
  
  addPolygons(
    data=USA_Adm_2,
    color = "#585858",
    fillColor=  ~obs_inc_pal(county_shp$inc),
    weight = 0.25,
    opacity = 1,
    fillOpacity = 1,
    highlightOptions = highlightOptions(color = "#5A5A5A", weight = 2,
                                        bringToFront = TRUE)) 

# to do : add legend for color
map_obs_inc_usa


