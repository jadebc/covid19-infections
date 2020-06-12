# --------------------------------------------------------------------------------
#
#   Run analysis
#
# --------------------------------------------------------------------------------

rm(list=ls());gc()
library(parallel)

# load base functions for the analysis
source(here::here("/0-config.R"))

# load sampling functions
source(here::here("6-sensitivity/sampling.R"))

# load bias correction functions
source(here::here("0-base-functions/0-bias-corr-functions.R"))



set.seed(86932102)

# number of samples from the prior
nsamp <- 1e3

tic()
theta_samp <- sample_prior(n = nsamp)
theta_samp <- as.data.frame(theta_samp)
toc()

# how many times to resample the distribution of expected case counts
reps <- 1e3


# --------------------------------------------------------------------------------
#   1. obtain state-specific prior distributions
# --------------------------------------------------------------------------------

covid_state <- load_state_data(min_date = "2020-02-28", max_date = "2020-04-18")
maxdate = max(covid_state$date)


covid_state <- covid_state %>%
  mutate(testrate = total / population * 1000,
         posrate = positive / total) %>%
  filter(date==as.Date(maxdate)) %>%
  dplyr::select(state, positive, total, population, testrate, posrate) %>%
  arrange(state)


#---------------------------------------
# US - constrain priors - does not vary by date
#---------------------------------------

theta_samp_constrained <- constrain_priors(priors = theta_samp)

#---------------------------------------
# calculate P_testpos_AS, check distributions
# list of list of priors for each date in each state
#---------------------------------------
theta_samp_states <- lapply(
  X = as.list(covid_state$posrate),
  FUN = function(x){
    est_P_testpos_AS(
      priors = theta_samp_constrained,
      est_testpos = x)
  }
)


names(theta_samp_states) <- unique(covid_state$state)


#---------------------------------------
# US - process priors
#---------------------------------------

#---------------------------------------
# states - process priors (uses RNG)
#---------------------------------------
state_priors_out_proc <- lapply(
  X = theta_samp_states,
  FUN = function(x){
    process_priors(priors = x)
  }
)


names(state_priors_out_proc) = unique(covid_state$state)


# --------------------------------------------------------------------------------
#   2. Obtain estimated number of infections per state
# --------------------------------------------------------------------------------

# Read in covid US and state data
covid_usa_state <- load_state_data(min_date = "2020-02-28", max_date = "2020-04-18")
maxdate = max(covid_usa_state$date)

covid_state_1day = covid_usa_state %>%
  filter(date==as.Date(maxdate)) %>%
  arrange(state)


#---------------------------------------
# Expected case counts
#---------------------------------------

corrected_samples_state_1day = mapply(
  generate_corrected_sample,
  covid_state_1day$population,
  covid_state_1day$total,
  covid_state_1day$positive,
  MoreArgs = list("distribution_list" = state_priors_out_proc),
  reps,
  unique(covid_state_1day$state)
)

colnames(corrected_samples_state_1day) = unique(covid_state_1day$state)

# saveRDS(corrected_samples_state_1day, paste0(results_path, "NO_PUSH_corrected_samples_us_state_undertesting_", Sys.Date(),
#                                   "_", "reps", reps, ".RDS"))

# obtain medians
sample_medians = unlist(mclapply(1:nrow(covid_state_1day),
                            function(x) median(corrected_samples_state_1day[,x]$exp_cases)))

sample_lb = unlist(mclapply(1:nrow(covid_state_1day),
                            function(x) quantile(corrected_samples_state_1day[,x]$exp_cases, prob=0.025,
                                                 na.rm=TRUE)))

sample_ub = unlist(mclapply(1:nrow(covid_state_1day),
                            function(x) quantile(corrected_samples_state_1day[,x]$exp_cases, prob=0.975,
                                                 na.rm=TRUE)))


covid_usa_state_adjusted <- covid_state_1day %>% mutate(
  estimated_cases = sample_medians,
  estimated_cases_lb = sample_lb,
  estimated_cases_ub = sample_ub
)


# --------------------------------------------------------------------------------
#   PLOT
# --------------------------------------------------------------------------------



dist <- corrected_samples_state_1day

state_abbrev <- read_csv(state_abbrev_path) %>%
  rename(state = Abbreviation,
         statename = State)


state_case_dist_list = list()
N_list = list()
for(i in 1:ncol(dist)){
  state_case_dist_list[[i]] = dist[, i]$exp_cases
  N_list[[i]] = dist[, i]$N
}

names(state_case_dist_list) = colnames(dist)
names(N_list) = colnames(dist)
state_case_dist = as.data.frame(bind_rows(state_case_dist_list))
N_df = as.data.frame(bind_rows(N_list))

state_case_distl = melt(state_case_dist) %>%
  rename(state = variable,
         exp_cases = value)
N_dfl = melt(N_df) %>%
  rename(state = variable,
         N = value)
N_dfl = N_dfl[!duplicated(N_dfl),]

plotdf = left_join(state_case_distl, N_dfl, by = "state") %>%
  mutate(exp_perpop = exp_cases / N * 1000) %>%
  group_by(state) %>%
  mutate(
    med = quantile(exp_cases, prob = 0.5),
    lb = quantile(exp_cases, prob = 0.025),
    ub = quantile(exp_cases, prob = 0.975))


plotdf = plotdf %>%
  left_join(state_abbrev, by = "state")

plotdf$statename = factor(plotdf$statename)
plotdf$statename_f = fct_reorder(plotdf$statename, plotdf$med)

plotbreaks = c(0, 1000, 10000, 100000, 1000000)

plot = ggplot(plotdf, aes(y = exp_cases, x = statename_f)) +
  geom_boxplot(aes(fill = log10(med)),
               outlier.stroke = 0.01, lwd = 0.2) +
  scale_y_log10(breaks = plotbreaks,
                labels = format(plotbreaks, scientific = F, big.mark = ",")) +
  scale_fill_viridis("log10(median)", begin = 0.3, end = 0.95, direction = -1, option = "A") +
  ylab("Distribution of estimated COVID-19 infections") +
  xlab("") +
  coord_flip() +
  theme_bw() +
  theme(legend.position="none")
plot
