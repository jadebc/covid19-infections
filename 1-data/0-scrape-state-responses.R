#######################################
# COVID-19 USA State Responses

# Scrape date from NYTimes:
# https://www.nytimes.com/interactive/2020/us/coronavirus-stay-at-home-order.html
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(rvest)
library(dplyr)
library(stringr)
library(readr)
library(tools)
library(lubridate)

# Fetch data
counties_states <- read_delim(url("https://raw.githubusercontent.com/grammakov/USA-cities-and-states/master/us_cities_states_counties.csv"), "|")
counties_states <- counties_states %>% 
  select(City, `State full`) %>% 
  mutate(City = as.character(City))
counties_states <- as.data.frame(counties_states)

state_interventions_url <- "https://www.nytimes.com/interactive/2020/us/coronavirus-stay-at-home-order.html"
state_interventions <- read_html(state_interventions_url)

###################
#### Statewide ####

statewide_regions <- state_interventions %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//div[contains(@class, 'statewide')]/h3/text()") %>% 
  rvest::html_text() %>% 
  str_trim()

statewide_orders <- state_interventions %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//div[contains(@class, 'statewide')]/div[contains(@class, 'place-wrap')]/p[contains(@class, 'l-order')]/text()") %>% 
  rvest::html_text()

statewide_dates <- state_interventions %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//div[contains(@class, 'statewide')]/div[contains(@class, 'place-wrap')]/p[contains(@class, 'l-order')]/span[contains(@class, 'l-date')]/text()") %>% 
  rvest::html_text()

statewide_dates_clean <- statewide_dates %>% 
  str_match("(, effective )([:alpha:]+ [:digit:]+)(.*)")
statewide_dates_clean <- statewide_dates_clean[,3]

us_interventions <- tibble(state = statewide_regions, county = rep(NA, length(statewide_regions)), order = statewide_orders, date = statewide_dates_clean, level = rep("state", length(statewide_regions))) %>% 
  mutate(date = paste0(date, " 2020")) %>% 
  mutate(date = mdy(date))


###########################
#### County level data ####

county_regions <- state_interventions %>%
  rvest::html_nodes('body') %>%
  xml2::xml_find_all("//p[@class = 'l-place']/text()") %>%
  rvest::html_text()

state_regions <- c()
for (county in county_regions) {
  query <- paste0("//*[text()[.='", county,"']]/ancestor::div[contains(@class, 'state-wrap')]/h3")
  state <- state_interventions %>%
    rvest::html_nodes('body') %>%
    xml2::xml_find_all(query) %>%
    rvest::html_text() %>%
    str_trim()
  state_regions <- c(state_regions, state)
}

county_orders <- state_interventions %>%
  rvest::html_nodes('body') %>%
  xml2::xml_find_all("//p[@class = 'l-place']/following-sibling::p[contains(@class, 'l-order')]/text()") %>%
  rvest::html_text()

county_dates <- state_interventions %>%
  rvest::html_nodes('body') %>%
  xml2::xml_find_all("//p[@class = 'l-place']/following-sibling::p[contains(@class, 'l-order')]/span[contains(@class, 'l-date')]/text()") %>%
  rvest::html_text()

county_dates_clean <- county_dates %>%
  str_match("(, effective )([:alpha:]+ [:digit:]+)(.*)")
county_dates_clean <- county_dates_clean[,3]

county_interventions <- tibble(state = state_regions, county = county_regions, order = county_orders, date = county_dates_clean, level = rep("county", length(state_regions))) %>% 
  mutate(county = str_trim(county)) %>% 
  mutate(date = paste0(date, " 2020")) %>% 
  mutate(date = mdy(date))

################
# Combine data and write to csv

state_response <- rbind(us_interventions, county_interventions)

write_csv(state_response, paste0(state_responses_pre_path, ymd(floor_date(now(), "day")), ".csv"))
