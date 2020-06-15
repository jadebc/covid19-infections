#######################################
# COVID-19 Cases USA County

# Process COVID-19 cases by USA County
# Obtain estimate of expected cases
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
source(paste0(here::here(), "/2-analysis/2-covid-expected-cases.R"))

set.seed(123)

#---------------------------------------
# Observed case counts
#---------------------------------------
# Read in state data 
covid_usa_county <- read.table(county_data_path, sep=",", header = TRUE)
state_pop <- read_csv(population_data_path)
state_abbrev <- read_csv(state_abbrev_path)

# NOT UPDATED BEL0W HERE
# # Merge state abbrev with state population, with covid data
# state_pop <- state_pop %>% dplyr::select("State", "Pop")
# state_pop <- left_join(state_pop, state_abbrev, by = "State") %>% 
#   dplyr::select(Abbreviation, Pop) %>% 
#   rename(state = Abbreviation, population = Pop)
# covid_usa_state <- covid_usa_state %>% left_join(state_pop, by = "state")   %>%
## drop data before March 7 because incomplete state data
#   filter(date >=as.Date("2020-03-07"))
# 
# # Convert dates to "days since lb cases". Drops states where cases < lb
# covid_usa_state_subset <- convert_date_to_days_since_lb(covid_usa_state, 100)
# 
# #---------------------------------------
# # Expected case counts
# #---------------------------------------
# reps = 1000
# 
# set.seed(123)
# corrected_samples = mcmapply(generate_corrected_sample,
#                              covid_usa_state_subset$population,
#                              covid_usa_state_subset$total, # total tested
#                              MoreArgs = list("distributions" = simdata),
#                              reps)
# 
# box_write(corrected_samples, 
#           paste0("corrected_samples_us_state_", Sys.Date(), "_", "reps", reps, ".RDS"),
#           box_getwd())
# 
# # obtain medians
# sample_medians = unlist(mclapply(1:nrow(covid_usa_state_subset), 
#                             function(x) median(corrected_samples[,x]$exp_cases)))
# 
# sample_lb = unlist(mclapply(1:nrow(covid_usa_state_subset), 
#                             function(x) quantile(corrected_samples[,x]$exp_cases, prob=0.025, 
#                                                  na.rm=TRUE)))
# 
# sample_ub = unlist(mclapply(1:nrow(covid_usa_state_subset), 
#                             function(x) quantile(corrected_samples[,x]$exp_cases, prob=0.975, 
#                                                  na.rm=TRUE)))
# 
# 
# covid_usa_adjusted <- covid_usa_state_subset %>% mutate(
#   estimated_cases = sample_medians,
#   estimated_cases_lb = sample_lb,
#   estimated_cases_ub = sample_ub
#   )
# 
# saveRDS(covid_usa_adjusted, paste0(results_path, "covid_usa_state_adjusted.RDS"))
# saveRDS(covid_usa_state_subset, paste0(results_path, "covid_usa_state_subset.RDS"))






