#######################################
# COVID-19 estimated infections
# correcting for incomplete testing and
# imperfect test accuracy

# Obtain estimated number of infections
# per state, percent under detection
# attributable to incomplete testing vs.
# imperfect test accuracy
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
source(paste0(here::here(), "/0-base-functions/0-bias-corr-functions-undertesting.R"))

set.seed(123)

# load priors
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
reps = 1000
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


saveRDS(corrected_samples_state_1day, paste0(results_path, "NO_PUSH_corrected_samples_us_state_undertesting_", Sys.Date(),
                                  "_", "reps", reps, ".RDS"))

#---------------------------------------------
# obtain medians - exp_cases
#---------------------------------------------
sample_medians = unlist(mclapply(1:nrow(covid_state_1day),
                            function(x) median(corrected_samples_state_1day[,x]$exp_cases)))

sample_lb = unlist(mclapply(1:nrow(covid_state_1day),
                            function(x) quantile(corrected_samples_state_1day[,x]$exp_cases, prob=0.025,
                                                 na.rm=TRUE)))

sample_ub = unlist(mclapply(1:nrow(covid_state_1day),
                            function(x) quantile(corrected_samples_state_1day[,x]$exp_cases, prob=0.975,
                                                 na.rm=TRUE)))

#---------------------------------------------
# obtain medians - exp_cases with perfect Se Sp
#---------------------------------------------
sample_medians_perfSeSp = unlist(mclapply(1:nrow(covid_state_1day),
                                 function(x) median(corrected_samples_state_1day[,x]$exp_cases_perfSeSp)))

sample_lb_perfSeSp = unlist(mclapply(1:nrow(covid_state_1day),
                            function(x) quantile(corrected_samples_state_1day[,x]$exp_cases_perfSeSp, prob=0.025,
                                                 na.rm=TRUE)))

sample_ub_perfSeSp = unlist(mclapply(1:nrow(covid_state_1day),
                            function(x) quantile(corrected_samples_state_1day[,x]$exp_cases_perfSeSp, prob=0.975,
                                                 na.rm=TRUE)))

#---------------------------------------------
# obtain medians - percent change due to accuracy
#---------------------------------------------
sample_medians_acc = unlist(mclapply(1:nrow(covid_state_1day),
                                          function(x) median(corrected_samples_state_1day[,x]$percent_acc)))

sample_lb_acc = unlist(mclapply(1:nrow(covid_state_1day),
                                     function(x) quantile(corrected_samples_state_1day[,x]$percent_acc, prob=0.025,
                                                          na.rm=TRUE)))

sample_ub_acc = unlist(mclapply(1:nrow(covid_state_1day),
                                     function(x) quantile(corrected_samples_state_1day[,x]$percent_acc, prob=0.975,
                                                          na.rm=TRUE)))


#---------------------------------------------
# obtain medians - percent change due to undertesting
#---------------------------------------------

sample_medians_und = unlist(mclapply(1:nrow(covid_state_1day),
                                          function(x) median(corrected_samples_state_1day[,x]$percent_und)))

sample_lb_und = unlist(mclapply(1:nrow(covid_state_1day),
                                     function(x) quantile(corrected_samples_state_1day[,x]$percent_und, prob=0.025,
                                                          na.rm=TRUE)))

sample_ub_und = unlist(mclapply(1:nrow(covid_state_1day),
                                     function(x) quantile(corrected_samples_state_1day[,x]$percent_und, prob=0.975,
                                                          na.rm=TRUE)))

covid_usa_adjusted <- covid_state_1day %>% mutate(
  estimated_cases = sample_medians,
  estimated_cases_lb = sample_lb,
  estimated_cases_ub = sample_ub,

  estimated_cases_perfSeSp = sample_medians_perfSeSp,
  estimated_cases_perfSeSp_lb = sample_lb_perfSeSp,
  estimated_cases_perfSeSp_ub = sample_ub_perfSeSp,

  percent_acc = sample_medians_acc,
  percent_acc_lb = sample_lb_acc,
  percent_acc_ub = sample_ub_acc,

  percent_und = sample_medians_und,
  percent_und_lb = sample_lb_und,
  percent_und_ub = sample_ub_und
  )

saveRDS(covid_usa_adjusted, paste0(results_path, "covid_usa_state_adjusted_undertesting.RDS"))

#---------------------------------------------
# obtain national average - percent change due to undertesting
#---------------------------------------------

covid_state_1day %>%
  select(state,population) %>%
  mutate(pop_prop = population/sum(population)) -> state_pops

samps_percent_und <- rep(0,length.out=ncol(corrected_samples_state_1day) * length(corrected_samples_state_1day[,"CA"]$percent_und))
samps_percent_und <- setNames(samps_percent_und,nm = rep(colnames(corrected_samples_state_1day),each=1e3))

weights <- rep(0,length(samps_percent_und))
weights <- setNames(weights,names(samps_percent_und))

for(i in 1:ncol(corrected_samples_state_1day)){
  state_i <- colnames(corrected_samples_state_1day)[i]
  state_ix <- which(names(samps_percent_und) == state_i)
  samps_percent_und[state_ix] <- corrected_samples_state_1day[,i]$percent_und

  weight_ix <- which(names(weights) == state_i)
  weights[weight_ix] <- state_pops %>% filter(state==state_i) %>% select(pop_prop) %>% as.numeric()
}

natl_mean_incomptest <- Hmisc::wtd.mean(x = samps_percent_und,weights = weights,normwt = TRUE)
natl_quant_incomptest <- Hmisc::wtd.quantile(x = samps_percent_und,weights = weights,normwt = TRUE,probs = c(0.025,0.5,0.975))

natl_mean_imperftest <- Hmisc::wtd.mean(x = 1-samps_percent_und,weights = weights,normwt = TRUE)
natl_quant_imperftest <- Hmisc::wtd.quantile(x = 1-samps_percent_und,weights = weights,normwt = TRUE,probs = c(0.025,0.5,0.975))