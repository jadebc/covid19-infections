#######################################
# COVID-19 estimated infections 
# correcting for incomplete testing and 
# imperfect test accuracy
# 
# Obtain priors for states
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

source(paste0(here::here(), "/0-base-functions/0-prior-functions.R"))

#---------------------------------------
# process data
#---------------------------------------
covid_state <- load_state_data(min_date = "2020-02-28", max_date = "2020-04-18")
maxdate = max(covid_state$date)


covid_state = covid_state %>% 
  mutate(testrate = total / population * 1000, 
         posrate = positive / total) %>%
  filter(date==as.Date(maxdate)) %>%
  dplyr::select(state, positive, total, population, testrate, posrate) %>%
  arrange(state)



#---------------------------------------
# US - constrain priors - does not vary by date
#---------------------------------------
# run time < 1 min
Sys.time()
tic()
theta_samp_constrained = constrain_priors(priors = theta_samp)
toc()

#---------------------------------------
# calculate P_testpos_AS, check distributions
# list of list of priors for each date in each state
#---------------------------------------
theta_samp_states = lapply(as.list(covid_state$posrate),
   function(x) est_P_testpos_AS(
    priors = theta_samp_constrained, 
    est_testpos = x))


names(theta_samp_states) = unique(covid_state$state)


#---------------------------------------
# US - process priors
#---------------------------------------

#---------------------------------------
# states - process priors
#---------------------------------------
Sys.time()
tic()
state_priors_out_proc = lapply(theta_samp_states,
                               function(x)
                                 process_priors(priors = x,
                                                Se = dist_Se,
                                                Sp = dist_Sp))

toc()

test = process_priors(priors = theta_samp_states[[1]],
               Se = dist_Se,
               Sp = dist_Sp)

names(state_priors_out_proc) = unique(covid_state$state)


saveRDS(state_priors_out_proc, paste0(results_path, "NO_PUSH_state_priors_out.RDS"))

