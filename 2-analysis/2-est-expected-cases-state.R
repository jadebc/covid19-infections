#######################################
# COVID-19 estimated infections
# correcting for incomplete testing and
# imperfect test accuracy

# Obtain estimated number of infections
# per state
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
source(paste0(here::here(), "/0-base-functions/0-bias-corr-functions.R"))

set.seed(123)

# load priors
# simdata = box_search("NO_PUSH_state_priors_out.RDS") %>% box_read()
simdata = readRDS(paste0(results_path, "NO_PUSH_state_priors_out.RDS"))

#---------------------------------------
# Observed case counts
#---------------------------------------
# Read in covid US and state data
covid_usa_state <- load_state_data(min_date = "2020-02-28", max_date = "2020-04-18")

maxdate = max(covid_usa_state$date)

covid_state_1day = covid_usa_state %>%
  filter(date==as.Date(maxdate)) %>%
  arrange(state)

#---------------------------------------
# Expected case counts
#---------------------------------------
reps = 1e4
Sys.time()
tic()
corrected_samples_state_1day = mapply(
  generate_corrected_sample,
  covid_state_1day$population,
  covid_state_1day$total,
  covid_state_1day$positive,
  MoreArgs = list("distribution_list" = simdata),
  reps,
  unique(covid_state_1day$state)
)
toc()

colnames(corrected_samples_state_1day) = unique(covid_state_1day$state)

# box_write(corrected_samples_state_1day,
#   paste0("NO_PUSH_corrected_samples_us_state_", Sys.Date(), "_", "reps", reps, ".RDS"),
#    dir_id = 115224581369)

saveRDS(corrected_samples_state_1day, paste0(results_path, "NO_PUSH_corrected_samples_us_state_", Sys.Date(),
                                  "_", "reps", reps, ".RDS"))

# obtain medians
sample_medians = unlist(mclapply(1:nrow(covid_state_1day),
                            function(x) median(corrected_samples_state_1day[,x]$exp_cases)))

sample_lb = unlist(mclapply(1:nrow(covid_state_1day),
                            function(x) quantile(corrected_samples_state_1day[,x]$exp_cases, prob=0.025,
                                                 na.rm=TRUE)))

sample_ub = unlist(mclapply(1:nrow(covid_state_1day),
                            function(x) quantile(corrected_samples_state_1day[,x]$exp_cases, prob=0.975,
                                                 na.rm=TRUE)))


covid_usa_adjusted <- covid_state_1day %>% mutate(
  estimated_cases = sample_medians,
  estimated_cases_lb = sample_lb,
  estimated_cases_ub = sample_ub
  )

saveRDS(covid_usa_adjusted, paste0(results_path, "covid_usa_state_adjusted.RDS"))
