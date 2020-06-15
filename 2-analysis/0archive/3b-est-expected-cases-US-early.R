#######################################
# COVID-19 Cases USA State

# Process COVID-19 cases by USA State
# Obtain estimate of expected cases

# input data is number specimens tested
# divide by 2 to get approx number of
# people tested assuming usually 2 
# specimens per person 
# https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/testing-in-us.html
# https://covidtracking.com/about-data/faq
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
source(paste0(here::here(), "/2-analysis/2-covid-expected-cases.R"))

set.seed(123)

# load priors
simdata = readRDS(paste0(results_path, "input_distributions_early.RDS"))

#---------------------------------------
# Observed case counts
#---------------------------------------
# Read in state covid data 
covid_usa_state <- get_states_daily(state = "all", date = "all")

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
  summarise(total = sum(total, na.rm=TRUE),
            population = sum(population, na.rm=TRUE)) 


#---------------------------------------
# Observed test counts from before March 
#---------------------------------------
# load most recent file saved in directory
tmpshot <- fileSnapshot(num_tested_pre_path)
latest_early_file = rownames(tmpshot$info[which.max(tmpshot$info$mtime),])
early_raw = read.csv(paste0(num_tested_pre_path, latest_early_file))

early = early_raw %>% mutate(
  tested = cdc_labs + us_ph_labs,
  population = mean(covid_all_usa$population)
  ) %>%
  filter(incomplete == F & cdc_conf == F) %>%
  dplyr::select(-c(cdc_conf, incomplete, cdc_labs, us_ph_labs)) %>%
  # assume 2 specimens per test
  mutate(tested = tested/2)

reps = 1000
library(tictoc)
tic()
corrected_samples = mcmapply(generate_corrected_sample,
                             early$population,
                             early$tested, # total tested
                             MoreArgs = list("distributions" = simdata),
                             reps)
toc()

saveRDS(corrected_samples, paste0(results_path, "NO_PUSH_corrected_samples_us_early_", Sys.Date(),
                                  "_", "reps", reps, ".RDS"))

# obtain medians
sample_medians = unlist(mclapply(1:nrow(early),
                                 function(x) median(corrected_samples[,x]$exp_cases)))

sample_lb = unlist(mclapply(1:nrow(early),
                            function(x) median(corrected_samples[,x]$exp_cases, prob=0.025,
                                                 na.rm=TRUE)))

sample_ub = unlist(mclapply(1:nrow(early),
                            function(x) quantile(corrected_samples[,x]$exp_cases, prob=0.975,
                                                 na.rm=TRUE)))

early <- early %>% mutate(
  estimated_cases = sample_medians,
  estimated_cases_lb = sample_lb,
  estimated_cases_ub = sample_ub
)

saveRDS(early, paste0(results_path, "covid_usa_adjusted_early.RDS"))


