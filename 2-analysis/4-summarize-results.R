#######################################
# COVID-19 estimated infections 
# correcting for incomplete testing and 
# imperfect test accuracy

# summarize results for numbers in text 
# of manuscript
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

covid_usa_state_adjusted = readRDS(paste0(results_path, "covid_usa_state_adjusted.RDS"))
covid_all_usa <- load_US_data(min_date = "2020-03-07", max_date = "2020-04-18")

state_res = covid_usa_state_adjusted %>%
  dplyr::select(state, statename, positive, total, estimated_cases, population)  %>%
  mutate(
    test_rate = total / population * 1000,
    testpos_rate = positive / total * 100,
    obs_perpop = positive / population * 1000,
    exp_perpop = estimated_cases / population * 1000,
    ratio = estimated_cases / positive
  )

us_res = covid_usa_state_adjusted %>%
  summarise(
    positive = sum(positive),
    total = sum(total),
    population = sum(population),
    estimated_cases = sum(estimated_cases),
    estimated_cases_lb = sum(estimated_cases_lb),
    estimated_cases_ub = sum(estimated_cases_ub)
    
  ) %>%
  mutate(
    test_rate = total / population * 1000,
    testpos_rate = positive / total * 100,
    obs_perpop = positive / population * 1000,
    exp_perpop = estimated_cases / population * 1000,
    ratio = estimated_cases / positive
  )

#---------------------------------------
# US total confirmed vs. estimated
#---------------------------------------
us_res
us_res$exp_perpop
us_res$ratio

#---------------------------------------
# % of infections in the US in March were undocumented
#---------------------------------------
(us_res$estimated_cases - us_res$positive) / us_res$estimated_cases

#---------------------------------------
# testing rates
#---------------------------------------
covid_all_usa = covid_all_usa %>% 
  filter(date %in% as.Date(c("2020-02-28", "2020-04-18"))) %>%
  mutate(testrate = total / population * 1000)
covid_all_usa$testrate

state_res %>% arrange(test_rate) %>% dplyr::select(test_rate) %>% pull() %>% min()
state_res$state[state_res$test_rate<6.068]
state_res %>% arrange(test_rate) %>% dplyr::select(test_rate) %>% pull() %>% max()
state_res$state[state_res$test_rate>30.8]

#---------------------------------------
# test positive rates
#---------------------------------------
state_res %>% dplyr::select(testpos_rate) %>% pull() %>% min()
state_res %>% dplyr::select(testpos_rate) %>% pull() %>% max()

#---------------------------------------
# total US cases obs and exp
#---------------------------------------
covid_usa$estimated_cases / covid_usa$positive
covid_usa$positive / covid_usa$population * 1000

covid_usa$estimated_cases_lb / covid_usa$positive
covid_usa$estimated_cases_ub / covid_usa$positive

#---------------------------------------
# state range cases obs and exp
#---------------------------------------
state_res %>% dplyr::select(obs_perpop) %>% pull() %>% min()
state_res %>% dplyr::select(obs_perpop) %>% pull() %>% max()

state_res %>% dplyr::select(exp_perpop) %>% pull() %>% min()
state_res %>% dplyr::select(exp_perpop) %>% pull() %>% max()

#---------------------------------------
# state range ratio
#---------------------------------------
state_res %>% dplyr::select(state, ratio) %>% arrange(ratio) %>% head()
state_res %>% dplyr::select(state, ratio) %>% arrange(ratio) %>% tail()

#---------------------------------------
# states with at least 10 times ratio
#---------------------------------------
state_res %>% filter(ratio > 10) %>% nrow()

#---------------------------------------
# testing rate among states with biggest ratio
#---------------------------------------
# largest ratio
state_res %>% arrange(ratio) %>% tail() %>%
  dplyr::select(statename, test_rate, ratio)

# smallest ratio
state_res %>% arrange(ratio) %>% head() %>%
  dplyr::select(statename, test_rate, ratio)


