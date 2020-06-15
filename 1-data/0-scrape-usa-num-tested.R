#######################################
# COVID-19 CDC USA Cumulative Incidence

# Scrape date from CDC
# "Total number of COVID-19 cases in the United States by date reported"
# https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/testing-in-us.html
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(jsonlite)
library(lubridate)
library(rvest)
library(dplyr)
library(stringr)


url <- "https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/testing-in-us.html"

# Fetch data

num_tested <- url %>% 
  read_html() %>% 
  rvest::html_nodes('table') %>% 
  rvest::html_table() 

num_tested <- num_tested[[1]]

# Clean data
num_tested <- num_tested %>% 
  select(date = `Date Collected`, cdc_labs = `CDC Labs`, us_ph_labs = `US Public Health Labs`) %>% 
  mutate(date = mdy(paste0(date, "/2020"))) %>% 
  mutate(cdc_conf = str_detect(cdc_labs, "‡"), incomplete = str_detect(us_ph_labs, "§")) %>% 
  mutate(cdc_labs = str_replace(cdc_labs, "‡", ""), us_ph_labs = str_replace(us_ph_labs, "§", "")) %>% 
  mutate(cdc_labs = ifelse(cdc_labs != "", cdc_labs, NA), 
         us_ph_labs = ifelse(us_ph_labs != "", us_ph_labs, NA))

write_csv(num_tested, paste0(num_tested_pre_file_path, ymd(floor_date(now(), "day")), ".csv"))
  
