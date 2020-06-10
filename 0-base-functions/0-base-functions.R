##############################################
# Documentation:  find_beta_shape_params
# Usage:          find_beta_shape_params(mu, sd)
# Description:    Calculate a and b shape parameters for a beta 
#                 distribution from mean and standard deviation
#
# Args/Options:   
# mu:             mean of distribution
# sd:             standard deviation of distribution

# Returns: a list with elements a and b that include shape parameters
# Output:  none
##############################################
find_beta_shape_params = function(mu, sd){
  var = sd ^ 2
  a = ((1 - mu) / var - 1 / mu) * mu ^ 2
  b = a * (1 / mu - 1)
  return (list(a = a, b = b))
}


##############################################
# Documentation:  gg_color_hue
# Usage:          gg_color_hue(n)
# Description:    create default ggplot color palette
#                 for a given number of unique values
#
# Args/Options:   
# n:              number of unique values/colors to include in palette

# Returns: color palette
# Output:  none
##############################################
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


##############################################
# Documentation:  load_state_data
# Usage:          load_state_data(min_date, max_date)
# Description:    read in data from COVID Tracking Project,
#                 merge in Census 2019 population projections,
#                 filter on date range
#
# Args/Options:   
# min_date:       first date of records to include, as a character string
# max_date:       last date of records to include, as a character string

# Returns:        data from COVID Tracking Project restricted
#                 to dates of interest with state populations 
# Output:         none
##############################################
load_state_data = function(min_date, max_date){
  
  #covid19us package current has a bug, uncommit this line when it's fixed
  #covid_usa_state <- get_states_daily(state = "all", date = "all")
  
  covid_usa_state = read.csv(paste0(here::here(),"/1-data/covid-tracking-data/daily.csv"))
  
  # load state population data
  state_pop_raw <- read_csv(population_data_path)
  state_abbrev <- read_csv(state_abbrev_path)
  
  # filter to state rows
  state_pop = state_pop_raw %>% 
    filter(NAME %in% state_abbrev$State) %>%
    dplyr::select(NAME, POPESTIMATE2019) %>%
    rename(state = NAME,
           population = POPESTIMATE2019)
  
  state_abbrev = state_abbrev %>% rename(state=State)
  
  state_pop = left_join(state_pop, state_abbrev, by = "state") %>%
    rename(state = Abbreviation,
           statename = state)
  
  # Merge state population with covid data
  covid_usa_state <- covid_usa_state %>% left_join(state_pop, by = "state") %>%
    filter(!is.na(population)) 
  
  covid_usa_state = covid_usa_state %>% mutate(date = as.Date(format(as.Date(as.character(date), format = "%Y%m%d"), "%Y-%m-%d"))) %>% 
    filter(date >= as.Date(min_date) & date <= as.Date(max_date))
  
  covid_usa_state
  
}

##############################################
# Documentation:  load_US_data
# Usage:          load_US_data(min_date, max_date)
# Description:    read in data from COVID Tracking Project,
#                 merge in Census 2019 population projections,
#                 filter on date range
#
# Args/Options:   
# min_date:       first date of records to include, as a character string
# max_date:       last date of records to include, as a character string

# Returns:        data from COVID Tracking Project restricted
#                 to dates of interest with US population
# Output:         none
##############################################
load_US_data = function(min_date, max_date){
  
  #covid19us package current has a bug, uncommit this line when it's fixed
  #covid_usa_state <- get_states_daily(state = "all", date = "all")
  covid_usa_state = read.csv(paste0(here::here(),"/1-data/covid-tracking-data/daily.csv"))

  
  # load state population data
  state_pop_raw <- read_csv(population_data_path)
  state_abbrev <- read_csv(state_abbrev_path)
  
  # filter to state rows
  state_pop = state_pop_raw %>% 
    filter(NAME %in% state_abbrev$State) %>%
    dplyr::select(NAME, POPESTIMATE2019) %>%
    rename(state = NAME,
           population = POPESTIMATE2019)
  
  state_abbrev = state_abbrev %>% rename(state=State)
  
  state_pop = left_join(state_pop, state_abbrev, by = "state") %>%
    rename(state = Abbreviation,
           statename = state)
  
  # Merge state population with covid data
  covid_usa_state <- covid_usa_state %>% left_join(state_pop, by = "state") %>%
    filter(!is.na(population)) 
  
  # aggregate to whole US
  covid_all_usa = covid_usa_state %>%
    group_by(date) %>%
    summarise(
      positive = sum(positive, na.rm = TRUE),
      total = sum(total, na.rm=TRUE),
      population = sum(population, na.rm=TRUE)) %>%
    # drop dates with not all 50+DC states
    mutate(date = format(as.Date(as.character(date), format = "%Y%m%d"), "%Y-%m-%d")) %>% 
    filter(date >= as.Date(min_date) & date <= as.Date(max_date))
  
}


