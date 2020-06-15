#######################################
# COVID-19 estimated infections
# correcting for incomplete testing and
# imperfect test accuracy

# configure data directories
# source base functions
# load libraries
#######################################
library(covid19us)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(parallel)
library(gridExtra)
library(reshape2)
library(plotly)
library(htmlwidgets)
library(assertthat)
library(gsheet)
library(RColorBrewer)
library(stringr)
library(tidyverse)
library(viridis)
library(tictoc)
# library(boxr)

#--------------------------------------------
# load base functions
#--------------------------------------------
source(paste0(here::here(), "/0-base-functions/0-base-functions.R"))

# box_auth(client_id = "r19w4k436sygn8opcx4va4troyhj04qm", client_secret = "znxiMaPT83h52IQdS2N8tiwik3SpA14L")
# box_setwd(109019705670)


#--------------------------------------------
# define raw data paths
#--------------------------------------------
data_path = paste0(here::here(),"/1-data")

# source: https://worldpopulationreview.com/states/
# census 2017
# population_data_path = paste0(data_path,'/state-population/data.csv')

# https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html#par_textimage
population_data_path = paste0(data_path,'/state-population/nst-est2019-alldata.csv')

corrected_samples_path = paste0(data_path,'/corrected_samples.rds')
state_abbrev_path = paste0(data_path,"/state-population/state_abbrev.csv")
state_responses_pre_path = paste0(data_path,"/state-responses/")
state_responses_pre_file_path = paste0(state_responses_pre_path,"state_responses_")
# county_data_path = box_search("us-counties.txt") %>% box_read()
county_data_path = "~/Box Sync/covid19-expected-cases/us-counties.txt"
usa_cum_inc_pre_path = paste0(data_path,"/usa-cum-inc/usa_cum_inc_")
num_tested_pre_path = paste0(data_path,"/usa-num-tested/")
num_tested_pre_file_path = paste0(num_tested_pre_path, "usa_num_tested_")

# https://data.healthcare.gov/dataset/Geocodes-USA-with-Counties/52wv-g36k
# county_geocode_path = box_search("Geocodes_USA_with_Counties.csv") %>% box_read()
county_geocode_path = "~/Box Sync/covid19-expected-cases/Geocodes_USA_with_Counties.csv"

#--------------------------------------------
# define output paths
#--------------------------------------------s
plot_path = paste0(here::here(), "/4-figures/")

full_simulation_path = "~/Box Sync/covid19-expected-cases/"

results_path = paste0(here::here(), "/5-results/")
