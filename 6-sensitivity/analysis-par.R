# --------------------------------------------------------------------------------
#
#   Run analysis (parallel)
#
# --------------------------------------------------------------------------------

rm(list = ls());gc()

# load sampling functions
source(here::here("6-sensitivity/sampling.R"))

library(parallel)
library(foreach)
library(covid19us)
library(tictoc)
library(plotly)
library(truncdist)
library(gsheet)
library(viridis)
library(here)
library(iterators)
library(doSNOW)


# --------------------------------------------------------------------------------
#   set up scenarios for sensitivity analysis
# --------------------------------------------------------------------------------

defaults <- list(
  correlated = FALSE, sigma = 0, # correlation?
  zs_a = 0.8, zs_mu = 0.9, # alpha
  za_b = 0.4, za_mu = 0.15, # beta
  p1_shape1 = 20, p1_shape2 = 1.4, # P(S1|tested)
  p2_b = 0.15 # P(S1|untested)
)

# 7 scenarios
params <- replicate(n = 7, expr = {defaults}, simplify = FALSE)

# 1. shift down alpha 1
params[[1]]$zs_mu <- 0.85
params[[1]]$zs_a <- 0.5

# 2. shift down alpha 2
params[[2]]$zs_mu <- 0.75
params[[2]]$zs_a <- 0.25

# 3. P(S1|tested) shift lower
a <- defaults$p1_shape1
b <- defaults$p1_shape2
pars_3 <- find_beta_shape_params(mu = 0.8, sd = sqrt((a*b) / (((a+b)^2) * (a+b+1))))

params[[3]]$p1_shape1 <- pars_3$a
params[[3]]$p1_shape2 <- pars_3$b

# 4. P(S1|untested) shift upper bound to 25%
params[[4]]$p2_b <- 0.25

# 5. upward shift beta
params[[5]]$za_mu <- 0.25
params[[5]]$za_b <- 0.6

# 6. mild correlation
params[[6]]$correlated <- TRUE
params[[6]]$sigma <- 0.2

# 7. high correlation
params[[7]]$correlated <- TRUE
params[[7]]$sigma <- 0.8

# means

# get their means
means <- replicate(n = 7, expr = {NULL})
means[[1]] <- means_prior(
  zs_mu = params[[1]]$zs_mu,
  zs_a = params[[1]]$zs_a
)

means[[2]] <- means_prior(
  zs_mu = params[[2]]$zs_mu,
  zs_a = params[[2]]$zs_a
)

means[[3]] <- means_prior(
  p1_shape1 = params[[3]]$p1_shape1,
  p1_shape2 = params[[3]]$p1_shape2
)

means[[4]] <- means_prior(
  p2_b = params[[4]]$p2_b
)

means[[5]] <- means_prior(
  za_mu = params[[5]]$za_mu,
  za_b = params[[5]]$za_b
)

# # correlation analysis
#
# # 6.
# sigma <- matrix(data = params[[6]]$sigma, nrow = 2, ncol = 2)
# diag(sigma) <- 1
#
# # Z_s := adjustment factor for P(test +|S) among untested (alpha)
# invCDF_zs <- make_trunc_beta_invCDF(a = defaults$zs_a, b = 1, mu = defaults$zs_mu, sd = 0.2^2)
#
# # Z_a := adjustment factor for P(test +|A) among untested (beta)
# invCDF_za <- make_trunc_beta_invCDF(a = 0.002, b = defaults$za_b, mu = defaults$za_mu, sd = 0.3^2)
#
# z <- MASS::mvrnorm(n = 1e6, mu = c(0,0), Sigma = sigma, empirical = TRUE)
# u <- pnorm(z)
#
# alpha <- invCDF_zs(u[, 1])
# beta <- invCDF_za(u[, 2])

# --------------------------------------------------------------------------------
#   run the sensitivity analysis
# --------------------------------------------------------------------------------

cl <- parallel::makeCluster(7,type = "SOCK")
doSNOW::registerDoSNOW(cl)

parallel::clusterSetRNGStream(cl=cl,iseed=583932411L)

clusterEvalQ(cl,{
  source(here::here("/0-config.R"))
  source(here::here("6-sensitivity/sampling.R"))
  source(here::here("0-base-functions/0-bias-corr-functions.R"))
})

sensitivty_out <- foreach(ii = iter(params), .combine = "c") %dopar% {

  n <- 1e5
  reps <- 1e4

  # for correlated alpha/beta
  if(ii$correlated){

    theta_samp <- sample_prior_corr(
      n = n,
      zs_a = ii$zs_a,
      zs_mu = ii$zs_mu,
      za_b = ii$za_b,
      za_mu = ii$za_mu,
      p1_shape1 = ii$p1_shape1,
      p1_shape2 = ii$p1_shape2,
      p2_b = ii$p2_b,
      z_corr = ii$sigma
    )

  # not correlated
  } else {

    theta_samp <- sample_prior(
      n = n,
      zs_a = ii$zs_a,
      zs_mu = ii$zs_mu,
      za_b = ii$za_b,
      za_mu = ii$za_mu,
      p1_shape1 = ii$p1_shape1,
      p1_shape2 = ii$p1_shape2,
      p2_b = ii$p2_b
    )

  }

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

  return(list(corrected_samples_state_1day))
}

parallel::stopCluster(cl)

saveRDS(object = sensitivty_out,file = here::here("6-sensitivity/NO_PUSH_sens.RDS"))
