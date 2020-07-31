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

#--------------------------------------------
# load base functions
#--------------------------------------------
source(paste0(here::here(), "/0-base-functions/0-base-functions.R"))


#--------------------------------------------
# define raw data paths
#--------------------------------------------
data_path = paste0(here::here(),"/1-data")

# https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html#par_textimage
population_data_path = paste0(data_path,'/state-population/nst-est2019-alldata.csv')

corrected_samples_path = paste0(data_path,'/corrected_samples.rds')
state_abbrev_path = paste0(data_path,"/state-population/state_abbrev.csv")
state_responses_pre_path = paste0(data_path,"/state-responses/")
state_responses_pre_file_path = paste0(state_responses_pre_path,"state_responses_")
usa_cum_inc_pre_path = paste0(data_path,"/usa-cum-inc/usa_cum_inc_")
num_tested_pre_path = paste0(data_path,"/usa-num-tested/")
num_tested_pre_file_path = paste0(num_tested_pre_path, "usa_num_tested_")

#--------------------------------------------
# define output paths
#--------------------------------------------
plot_path = paste0(here::here(), "/4-figures/")
results_path = paste0(here::here(), "/5-results/")
