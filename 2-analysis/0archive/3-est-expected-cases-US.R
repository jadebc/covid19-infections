#######################################
# COVID-19 Cases USA State

# Process COVID-19 cases by USA State
# Obtain estimate of expected cases
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
source(paste0(here::here(), "/0-base-functions/0-bias-corr-functions.R"))

set.seed(123)

# load priors
simdata = readRDS(paste0(results_path, "NO_PUSH_US_priors_out_mar31.RDS"))

#---------------------------------------
# Observed case counts
#---------------------------------------
# Read in US covid data 
covid_all_usa <- load_US_data() %>%
  filter(date == as.Date("2020-03-31"))

#---------------------------------------
# Expected case counts
#---------------------------------------
reps = 1000
Sys.time()
tic()
corrected_samples = generate_corrected_sample(
  N = covid_all_usa$population,
  N_tested = covid_all_usa$total,
  N_pos_obs = covid_all_usa$positive,
  distribution_list = simdata,
  num_reps = reps,
  state = F
)
toc()


saveRDS(corrected_samples, paste0(results_path, "NO_PUSH_corrected_samples_us", Sys.Date(),
                                  "_", "reps", reps, ".RDS"))

# obtain median and bounds
sample_medians = median(corrected_samples$exp_cases)
sample_lb = quantile(corrected_samples$exp_cases, prob = 0.025)
sample_ub = quantile(corrected_samples$exp_cases, prob = 0.975)


covid_all_usa_adjusted <- covid_all_usa %>% mutate(
  estimated_cases = sample_medians,
  estimated_cases_lb = sample_lb,
  estimated_cases_ub = sample_ub
  )

saveRDS(covid_all_usa_adjusted, paste0(results_path, "covid_usa_adjusted.RDS"))






