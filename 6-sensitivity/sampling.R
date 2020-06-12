# --------------------------------------------------------------------------------
#
#   Functions to sample from prior (and Bayesian Melding)
#
# --------------------------------------------------------------------------------

##############################################
# Documentation:  find_beta_shape_params
# Usage:          find_beta_shape_params(mu, sd)
# Description:    Calculate a and b shape parameters for a beta
#                 distribution from mean and standard deviation
#
# Args/Options:
# mu:             mean of distribution
# sd:             standard deviation of distribution

# Returns: a list with elements a and b that include shape parameters
# Output:  none
##############################################
find_beta_shape_params = function(mu, sd){
  var = sd ^ 2
  a = ((1 - mu) / var - 1 / mu) * mu ^ 2
  b = a * (1 / mu - 1)
  return (list(a = a, b = b))
}


##############################################
# Documentation:  est_P_testpos_AS
# Usage:          est_P_testpos_AS(priors, est_testpos)
# Description:    calculate P(test+|A), P(test+|S)
#
# Args/Options:
# priors:         matrix or data frame containing columns for priors for Z_S (alpha range),
#                 Z_A (beta range)
# est_testpos:    empirical estimate of P(test+|tested)

# Returns:        data frame containing P(test+|S), P(test+|A),
#                 empirical estimate of P(test+|tested)
# Output:         none
##############################################
est_P_testpos_AS = function(priors, est_testpos){

  priors = as.data.frame(priors)

  P_testpos_S = priors$Z_S  * est_testpos
  P_testpos_A = priors$Z_A  * est_testpos

  priors_out = priors %>% mutate(
    P_testpos_S = P_testpos_S,
    P_testpos_A = P_testpos_A,
    est_testpos = est_testpos
  )

  return(priors_out)
}

#---------------------------------------
# Define prior distributions
#---------------------------------------

# function factory for sampling beta distributions
make_beta_samp <- function(shape1, shape2){

  samp <- function(n){
    rbeta(n = n, shape1 = shape1, shape2 = shape2)
  }

  return(samp)
}

# function factory for PDF of truncated beta densities
make_trunc_beta_dens <- function(a, b, mu, sd){
  stopifnot(all(c(a,b,mu,sd) >= 0))

  pars <- find_beta_shape_params(mu = mu, sd = sd)

  dens <- function(x){
    truncdist::dtrunc(
      x = x, spec = "beta", a = a, b = b,
      shape1 = pars$a, shape2 = pars$b,
      ncp = 0, log = FALSE
    )
  }

  return(dens)
}


# function factory for sampling truncated beta densities
make_trunc_beta_samp <- function(a, b, mu, sd){
  stopifnot(all(c(a,b,mu,sd) >= 0))

  pars <- find_beta_shape_params(mu = mu, sd = sd)

  samp <- function(n){
    truncdist::rtrunc(
      n = n, spec = "beta", a = a, b = b,
      shape1 = pars$a, shape2 = pars$b, ncp = 0
    )
  }

  return(samp)
}

# function factory for inverse CDFs (quantile fn) of truncated beta densities
make_trunc_beta_invCDF <- function(a, b, mu, sd){
  stopifnot(all(c(a,b,mu,sd) >= 0))

  pars <- find_beta_shape_params(mu = mu, sd = sd)

  invCDF <- function(x){
    truncdist::qtrunc(
      p = x, spec = "beta", a = a, b = b,
      shape1 = pars$a, shape2 = pars$b, ncp = 0,
      lower.tail = TRUE, log.p = FALSE
    )
  }

  return(invCDF)
}

# sample the prior
sample_prior <- function(
  n,
  p1_shape1 = 20, p1_shape2 = 1.4,
  p2_a = 0, p2_b = 0.15, p2_mu = 0.025, p2_sd = 0.15^2,
  zs_a = 0.8, zs_b = 1, zs_mu = 0.9, zs_sd = 0.2^2,
  za_a = 0.002, za_b = 0.4, za_mu = 0.15, za_sd = 0.3^2
){

  # p1 := P(S1|tested)
  samp_p1 <- make_beta_samp(shape1 = p1_shape1, shape2 = p1_shape2)

  # p2 := P(S1|untested)
  samp_p2 <- make_trunc_beta_samp(a = p2_a, b = p2_b, mu = p2_mu, sd = p2_sd)

  # Z_s := adjustment factor for P(test +|S) among untested (alpha)
  samp_zs <- make_trunc_beta_samp(a = zs_a, b = zs_b, mu = zs_mu, sd = zs_sd)

  # Z_a := adjustment factor for P(test +|A) among untested (beta)
  samp_za <- make_trunc_beta_samp(a = za_a, b = za_b, mu = za_mu, sd = za_sd)

  out <- matrix(data = NaN,nrow = n,ncol = 4,dimnames = list(NULL,c("p1","p2","zs","za")))
  out[,"p1"] <- samp_p1(n)
  out[,"p2"] <- samp_p2(n)
  out[,"zs"] <- samp_zs(n)
  out[,"za"] <- samp_za(n)
  return(out)
}

# get means of the prior
means_prior <- function(
  p1_shape1 = 20, p1_shape2 = 1.4,
  p2_a = 0, p2_b = 0.15, p2_mu = 0.025, p2_sd = 0.15^2,
  zs_a = 0.8, zs_b = 1, zs_mu = 0.9, zs_sd = 0.2^2,
  za_a = 0.002, za_b = 0.4, za_mu = 0.15, za_sd = 0.3^2,
  Se_a = 0.65, Se_b = 1, Se_mu = 0.8, Se_sd = 0.4^2,
  Sp_a = 0.9998, Sp_b = 1, Sp_mu = 0.99995, Sp_sd = 0.01^2
){

  out <- replicate(n=6,expr={NULL})
  out <- setNames(out,nm = c("P(S1|tested)", "P(S1|untested)", "alpha", "beta", "Se", "Sp"))

  # p1 := P(S1|tested)
  mean_p1 <- p1_shape1 / (p1_shape1+p1_shape2)

  out[["P(S1|tested)"]]$mean <- mean_p1
  out[["P(S1|tested)"]]$shape1 <- p1_shape1
  out[["P(S1|tested)"]]$shape2 <- p1_shape2

  # p2 := P(S1|untested)
  pars <- find_beta_shape_params(mu = p2_mu, sd = p2_sd)

  integrand <- function(x){
    truncdist::dtrunc(
      x = x, spec = "beta", a = p2_a, b = p2_b,
      shape1 = pars$a, shape2 = pars$b, ncp = 0
    ) * x
  }

  mean_p2 <- integrate(integrand,lower=0,upper=1)$value

  out[["P(S1|untested)"]]$mean <- mean_p2
  out[["P(S1|untested)"]]$shape1 <- pars$a
  out[["P(S1|untested)"]]$shape2 <- pars$b

  # Z_s := adjustment factor for P(test +|S) among untested (alpha)
  pars <- find_beta_shape_params(mu = zs_mu, sd = zs_sd)

  integrand <- function(x){
    truncdist::dtrunc(
      x = x, spec = "beta", a = zs_a, b = zs_b,
      shape1 = pars$a, shape2 = pars$b, ncp = 0
    ) * x
  }

  mean_zs <- integrate(integrand,lower=0,upper=1)$value

  out[["alpha"]]$mean <- mean_zs
  out[["alpha"]]$shape1 <- pars$a
  out[["alpha"]]$shape2 <- pars$b

  # Z_a := adjustment factor for P(test +|A) among untested (beta)
  pars <- find_beta_shape_params(mu = za_mu, sd = za_sd)

  integrand <- function(x){
    truncdist::dtrunc(
      x = x, spec = "beta", a = za_a, b = za_b,
      shape1 = pars$a, shape2 = pars$b, ncp = 0
    ) * x
  }

  mean_za <- integrate(integrand,lower=0,upper=1)$value

  out[["beta"]]$mean <- mean_za
  out[["beta"]]$shape1 <- pars$a
  out[["beta"]]$shape2 <- pars$b

  # test sensitivity
  pars <- find_beta_shape_params(mu = Se_mu, sd = Se_sd)

  integrand <- function(x){
    truncdist::dtrunc(
      x = x, spec = "beta", a = Se_a, b = Se_b,
      shape1 = pars$a, shape2 = pars$b, ncp = 0
    ) * x
  }

  mean_Se <- integrate(integrand,lower=0,upper=1)$value

  out[["Se"]]$mean <- mean_Se
  out[["Se"]]$shape1 <- pars$a
  out[["Se"]]$shape2 <- pars$b

  # test specificity
  pars <- find_beta_shape_params(mu = Sp_mu, sd = Sp_sd)

  integrand <- function(x){
    truncdist::dtrunc(
      x = x, spec = "beta", a = Sp_a, b = Sp_b,
      shape1 = pars$a, shape2 = pars$b, ncp = 0
    ) * x
  }

  mean_Sp <- integrate(integrand,lower=0,upper=1)$value

  out[["Sp"]]$mean <- mean_Sp
  out[["Sp"]]$shape1 <- pars$a
  out[["Sp"]]$shape2 <- pars$b

  return(out)
}


# sample the prior where alpha/beta are no longer independent
sample_prior_corr <- function(
  n,
  p1_shape1 = 20, p1_shape2 = 1.4,
  p2_a = 0, p2_b = 0.15, p2_mu = 0.025, p2_sd = 0.15^2,
  zs_a = 0.8, zs_b = 1, zs_mu = 0.9, zs_sd = 0.2^2,
  za_a = 0.002, za_b = 0.4, za_mu = 0.15, za_sd = 0.3^2,
  z_corr = 0.5
){

  stopifnot((z_corr < 1) & (z_corr >= 0))

  # inverse CDF fn's for alpha/beta

  # Z_s := adjustment factor for P(test +|S) among untested (alpha)
  invCDF_zs <- make_trunc_beta_invCDF(a = zs_a, b = zs_b, mu = zs_mu, sd = zs_sd)

  # Z_a := adjustment factor for P(test +|A) among untested (beta)
  invCDF_za <- make_trunc_beta_invCDF(a = za_a, b = za_b, mu = za_mu, sd = za_sd)

  # correlation matrix for Gaussian copula and sample from it
  sigma <- matrix(data = z_corr, nrow = 2, ncol = 2)
  diag(sigma) <- 1

  z <- MASS::mvrnorm(n, mu = c(0,0), Sigma = sigma, empirical = TRUE)
  u <- pnorm(z)

  samp_zs <- invCDF_zs(u[, 1])
  samp_za <- invCDF_za(u[, 2])

  # p1 := P(S|tested)
  samp_p1 <- make_beta_samp(shape1 = p1_shape1, shape2 = p1_shape2)

  # p2 := P(S|untested)
  samp_p2 <- make_trunc_beta_samp(a = p2_a, b = p2_b, mu = p2_mu, sd = p2_sd)

  # return results in matrix
  out <- matrix(data = NaN,nrow = n,ncol = 4,dimnames = list(NULL,c("p1","p2","zs","za")))
  out[,"p1"] <- samp_p1(n)
  out[,"p2"] <- samp_p2(n)
  out[,"zs"] <- samp_zs
  out[,"za"] <- samp_za
  return(out)
}




#---------------------------------------
# Add Se and Sp to each prior
#---------------------------------------
# set.seed(123)
# # distribution of sensitivity of test
# dist_Se = truncdist::rtrunc(n = 100000,spec = "beta",a = 0.65,b = 1,
#                             shape1 = find_beta_shape_params(mu = 0.8, sd = (0.4)^2)$a,
#                             shape2 = find_beta_shape_params(mu = 0.8, sd = (0.4)^2)$b)
#
# # distribution of specificity of test
# dist_Sp = truncdist::rtrunc(n = 100000,spec = "beta",a = 0.9998,b = 1,
#                             shape1 = find_beta_shape_params(mu = 0.99995, sd = (0.01)^2)$a,
#                             shape2 = find_beta_shape_params(mu = 0.99995, sd = (0.01)^2)$b)

process_priors <- function(
  priors,
  Se_a = 0.65, Se_b = 1, Se_mu = 0.8, Se_sd = 0.4^2,
  Sp_a = 0.9998, Sp_b = 1, Sp_mu = 0.99995, Sp_sd = 0.01^2
){

  samp_Se <- make_trunc_beta_samp(a = Se_a, b = Se_b, mu = Se_mu, sd = Se_sd)
  samp_Sp <- make_trunc_beta_samp(a = Sp_a, b = Sp_b, mu = Sp_mu, sd = Sp_sd)

  n <- nrow(priors)

  Se <- samp_Se(n)
  Sp <- samp_Sp(n)

  simdata = as.data.frame(priors) %>%
    rename(
      P_S_tested = "P(S|tested)",
      P_S_untested = "P(S|untested)"
    ) %>%
    mutate(dist_Se = Se,
           dist_Sp = Sp)

  simdata$P_A_testpos = est_P_A_testpos(
    simdata$P_S_untested,
    simdata$Z_A,
    simdata$Z_S
  )

  return(simdata)

}

##############################################
# Documentation:  est_P_A_testpos
# Usage:          est_P_A_testpos(P_S_untested, Z_A, Z_S)
# Description:    calculate P(A|test+)
#
# Args/Options:
# P_S_untested:   prior for P(S|untested), as a scalar
# Z_A:            prior for Z_A, as a scalar
# Z_S:            prior for Z_S, as a scalar

# Returns:        estimate of P(A|test+)
# Output:         none
##############################################
est_P_A_testpos <- function(P_S_untested, Z_A, Z_S){

  Z_A * (1 - P_S_untested) / (( Z_A * (1 - P_S_untested)) + (Z_S * P_S_untested))

}
est_P_A_testpos <- Vectorize(est_P_A_testpos)


##############################################
# Documentation:  p0
# Usage:          p0(x)
# Description:    function to create truncated beta probability density function
#                 for P(A|test+)
#
# Args/Options:   none

# Returns:        pdf of P(A|test+)
# Output:         none
##############################################

# for simplicity, p0 := P(A|test+)
p0 <- make_trunc_beta_dens(a = 0.25, b = 0.7, mu = 0.4, sd = 0.35^2)


##############################################
# Documentation:  constrain_priors
# Usage:          constrain_priors(priors)
# Description:    constrain priors using Bayesian melding
#                 to ensure P(A|test+) is within defined range
#
# Args/Options:
# priors:         data frame with priors for P_S_untested, Z_S, Z_A

# Returns:        data frame with constrained priors for P_S_untested, Z_S, Z_A,
#                 and P(A|test+)
# Output:         none
##############################################
constrain_priors <- function(priors){

  nsamp <- nrow(priors)

  #---------------------------------------
  # Run the SIR algorithm to sample from
  # the induced "posterior" on theta
  #---------------------------------------

  phi <- est_P_A_testpos(
    P_S_untested = priors[,2],
    Z_S = priors[,3],
    Z_A = priors[,4]
  )

  phi_induced <- density(x = phi,n = nsamp,adjust = 2,kernel = "gaussian")
  phi_sampled_density <- unlist(parallel::mclapply(X = phi,FUN = function(p){
    phi_induced$y[which(phi_induced$x > p)[1]]
  }))

  weights <- parallel::mcmapply(FUN = function(p,phi_sampled_density,alpha){
    (phi_sampled_density/ p0(p))^(1-alpha)
  },p=phi,phi_sampled_density=phi_sampled_density,MoreArgs = list(alpha=0.5))

  # resample the posterior
  nsamp_post <- 1e5 # number of samples from the posterior
  post_samp_ind <-sample.int(n=nsamp, size=nsamp_post, prob=1/weights,replace=T)

  pi_samp <- matrix(
    data = NaN,
    nrow = nsamp_post,
    ncol = 5,
    dimnames = list(NULL,c("P(S|tested)","P(S|untested)","Z_S","Z_A","P(A|test+)"))
  )

  pi_samp[1:nsamp_post,1:4] <- as.matrix(theta_samp[post_samp_ind,])
  pi_samp[1:nsamp_post,5] <- phi[post_samp_ind]

  pi_samp = cbind(pi_samp, `P(S|test+)` = 1 - pi_samp[,5])

  return(pi_samp)

}
