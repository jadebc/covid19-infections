#######################################
# COVID-19 CDC USA Cumulative Incidence

# Scrape date from CDC
# "Total number of COVID-19 cases in the United States by date reported"
# https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/cases-in-us.html
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(jsonlite)
library(lubridate)

# Fetch data
cum_cases_json <- fromJSON("https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/total-cases-onset.json")
cum_cases_mat <- cum_cases_json$data$columns
cum_cases_df <- as.data.frame(t(cum_cases_mat))

# Clean headers
cum_cases_df <- cum_cases_df[-1, ]
cum_cases_df <- cum_cases_df %>% 
  select(date = V1, cases = V2) %>% 
  mutate(date = mdy(date))

write_csv(cum_cases_df, paste0(usa_cum_inc_pre_path, ymd(floor_date(now(), "day")), ".csv"))
