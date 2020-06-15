#######################################
# COVID-19 expected cases 
# 
# Obtain priors for US
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

source(paste0(here::here(), "/0-base-functions/0-prior-functions.R"))

#---------------------------------------
# process data
#---------------------------------------
covid_all_usa <- load_US_data()
covid_all_usa = covid_all_usa %>% 
  mutate(testrate = total / population * 1000, 
         posrate = positive / total)

# examine distribution of testing
plot(covid_all_usa$date, covid_all_usa$posrate)
plot(covid_all_usa$date, covid_all_usa$testrate)
plot(covid_all_usa$date, covid_all_usa$posrate)

covid_all_usa = covid_all_usa %>% filter(date == as.Date("2020-03-31"))


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
# list of priors for each date
#---------------------------------------
theta_samp_US = est_P_testpos_AS(priors = theta_samp_constrained,
                                 est_testpos = covid_all_usa$posrate)


#---------------------------------------
# US - process priors
#---------------------------------------
US_priors_out_proc = process_priors(priors = theta_samp_US,
               Se = dist_Se,
               Sp = dist_Sp)


saveRDS(US_priors_out_proc, paste0(results_path, "NO_PUSH_US_priors_out_mar31.RDS"))

