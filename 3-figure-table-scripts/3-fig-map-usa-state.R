#######################################
# COVID-19 estimated infections 
# correcting for incomplete testing and 
# imperfect test accuracy

# Map of COVID-19 cases vs. estimated
# infections by USA State

# Note: map pngs and htmls should be 
# saved manually using the export feature. 
#######################################
rm(list=ls())

library(rgdal)
library(rgeos)
library(leaflet)
library(maptools)
library(sf)
library(tigris)

source(paste0(here::here(), "/0-config.R"))

covid_usa_state_adjusted = readRDS(paste0(results_path, "covid_usa_state_adjusted.RDS"))
covid_usa_state_adjusted = covid_usa_state_adjusted %>% filter(state!="US")

# show cumulative cases 
covid_usa_data = covid_usa_state_adjusted %>% 
  dplyr::select(state, statename, estimated_cases, positive, population) %>%
  rename(NAME_1 = statename) %>%
  mutate(obs_label = paste0("<b>", NAME_1, "</b><br>", 
                            sprintf("%0.3f", positive/population*1000), " cases per 1,000", "<br>",
                            format(positive, big.mark=",", digits=0, scientific=F, trim = TRUE), " observed cases"),
         ratio_label = paste0("<b>", NAME_1, "</b><br>", 
                              ifelse(positive==0, "No ratio", sprintf("%0.1f", estimated_cases/positive)),
                              ifelse(positive==0, "", " estimated : observed"), "<br>",
                              format(estimated_cases, big.mark=",", digits=0, scientific=F, trim = TRUE), " estimated cases", "<br>",
                              format(positive, big.mark=",", digits=0, scientific=F, trim = TRUE), " observed cases"))
 
# read in US state boundaries
USA_Adm_1 <- tigris:::states(cb = TRUE)
USA_Adm_1 <- st_as_sf(USA_Adm_1)


territories = c('AS', 'VI', 'MP', 'GU')
USA_Adm_1 <- USA_Adm_1 %>% filter(!USA_Adm_1$STUSPS %in% territories)
USA_Adm_1 <- as_Spatial(USA_Adm_1)

# read in lakes data from http://www.naturalearthdata.com/downloads/10m-physical-vectors/
great_lakes <- st_read(paste0(here::here(), "/1-data/lakes"))
great_lakes_subset <- great_lakes %>% filter(name_alt == "Great Lakes") %>%  
  filter(scalerank == 0) 


# merge cases with shape file
USA_shp = merge(USA_Adm_1, covid_usa_data, by.x = 'NAME', by.y = 'NAME_1')

# get cases per pop
USA_shp$ratio = ifelse(USA_shp$positive==0, NA, USA_shp$estimated_cases / USA_shp$positive)
USA_shp$obs_case_perpop = USA_shp$positive / USA_shp$population * 1000

# modify interval labels
label_interval <- function(breaks) {
  paste0(breaks[1:length(breaks) - 1], " - ", breaks[2:length(breaks)])
}

ratio_quantiles <- quantile(USA_shp$ratio, c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm=TRUE)
obs_case_perpop_quantiles <- unique(quantile(USA_shp$obs_case_perpop, 
                                             c(0, 0.2, 0.4, 0.6, 0.8, 1),
                                             na.rm=TRUE))

# create categorical variable
USA_shp$ratio = cut(USA_shp$ratio, 
                    ratio_quantiles,
                    include.lowest=TRUE,
                    labels = label_interval(round(ratio_quantiles, 0))
)

USA_shp$obs_case_perpop_cat = cut(USA_shp$obs_case_perpop, 
                                  obs_case_perpop_quantiles,
                                  include.lowest=TRUE,
                                  labels = label_interval(round(obs_case_perpop_quantiles, 1))
)


# define color palette
gnbu_colors = brewer.pal(n=6,"GnBu")[2:6]
orrd_colors = brewer.pal(n=6,"OrRd")[2:6]

exp_cases_pal = colorFactor(gnbu_colors, USA_shp$ratio)
obs_cases_pal = colorFactor(orrd_colors, USA_shp$obs_case_perpop_cat)

##############################################
# map of estimated : observed cases 
##############################################
map_ratio_usa = leaflet(USA_shp, options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) %>%
  
  addProviderTiles(provider = "CartoDB.Positron") %>%
  
  # set zoom center point and level
  setView(lng = -99, lat = 39, zoom = 4.1) %>%
  
  # add state case count polygons
  addPolygons(
    data=USA_shp,
    color = "#878787",
    fillColor=  ~exp_cases_pal(USA_shp$ratio),
    weight = 0.5,
    opacity = 1,
    fillOpacity = 1,
    highlightOptions = highlightOptions(color = "#FFFFFF", weight = 2,
                                        bringToFront = FALSE),
    label = ~lapply(USA_shp$ratio_label, htmltools::HTML)) %>%
  
  addPolygons(
    data=great_lakes_subset,
    color = "#D4DADC",
    weight = 0.5,
    opacity = 1,
    fillOpacity = 1) %>%
  
  # add legend
  addLegend("bottomleft",
            pal = exp_cases_pal,
            # values = ~USA_shp$est_case_perpop,
            values = ~USA_shp$ratio,
            title = "estimated:confirmed",
            opacity = 1
  )

map_ratio_usa

# use export feature to manually save files
# png: "fig-map-usa-state-exp-obs-ratio.png"
# html: "fig-map-usa-state-exp-obs-ratio.html"

##############################################
# map of observed cases per 1,000
##############################################
map_obs_usa = leaflet(USA_shp, options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) %>%
  
  addProviderTiles(provider = "CartoDB.Positron") %>%
  
  # set zoom center point and level
  setView(lng = -99, lat = 39, zoom = 4.1) %>%  

    # add state case count polygons
  addPolygons(
    data=USA_shp,
    color = "#878787",
    fillColor=  ~obs_cases_pal(USA_shp$obs_case_perpop_cat),
    weight = 0.5,
    opacity = 1,
    fillOpacity = 1,
    highlightOptions = highlightOptions(color = "#FFFFFF", weight = 2,
                                        bringToFront = TRUE),
    label = ~lapply(USA_shp$obs_label, htmltools::HTML)) %>%
  
  addPolygons(
    data=great_lakes_subset,
    color = "#D4DADC",
    weight = 0.5,
    opacity = 1,
    fillOpacity = 1) %>%
  
  # add legend
  addLegend("bottomleft",
            pal = obs_cases_pal,
            # values = ~USA_shp$obs_case_perpop,
            values = ~USA_shp$obs_case_perpop_cat,
            title = "Cases per 1,000",
            opacity = 1
  )


map_obs_usa

# use export feature to manually save files
# png: "fig-map-usa-state-cases-per-1000-obs.png"
# html: "fig-map-usa-state-cases-per-1000-obs.html"
