

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
# p1 := P(S|tested)
samp_p1 <- function(n){
  rbeta(n = n,shape1 = 20, shape2 = 1.4)
}

# p2 := P(S|untested)
samp_p2 <- function(n){
  truncdist::rtrunc(n = n,spec = "beta",a = 0,b = 0.15,
                    shape1 = find_beta_shape_params(mu = 0.025, sd = (0.15)^2)$a, 
                    shape2 = find_beta_shape_params(mu = 0.025, sd = (0.15)^2)$b)
}

# Z_s := adjustment factor for P(test +|S) among untested (alpha)
samp_zs <- function(n){
  truncdist::rtrunc(n = n,spec = "beta",a = 0.8,b = 1,
                    shape1 = find_beta_shape_params(mu = 0.9, sd = (0.2)^2)$a,
                    shape2 = find_beta_shape_params(mu = 0.9, sd = (0.2)^2)$b)
}

# Z_a := adjustment factor for P(test +|A) among untested (beta)
samp_za <- function(n){
  truncdist::rtrunc(n = n,spec = "beta",a = 0.002,b = 0.4,
                    shape1 = find_beta_shape_params(mu = 0.15, sd = (0.3)^2)$a,
                    shape2 = find_beta_shape_params(mu = 0.15, sd = (0.3)^2)$b)
}


# sample the prior
sample_prior <- function(n){
  stopifnot(n>0)
  out <- matrix(data = NaN,nrow = n,ncol = 4,dimnames = list(NULL,c("p1","p2","zs","za")))
  out[,"p1"] <- samp_p1(n)
  out[,"p2"] <- samp_p2(n)
  out[,"zs"] <- samp_zs(n)
  out[,"za"] <- samp_za(n)
  return(out)
}

set.seed(123)
# number of samples from the prior
nsamp <- 1e5

theta_samp <- sample_prior(n = nsamp)
theta_samp = as.data.frame(theta_samp)





#---------------------------------------
# Add Se and Sp to each prior
#---------------------------------------
set.seed(123)
# distribution of sensitivity of test
dist_Se = truncdist::rtrunc(n = 100000,spec = "beta",a = 0.65,b = 1,
                            shape1 = find_beta_shape_params(mu = 0.8, sd = (0.4)^2)$a,
                            shape2 = find_beta_shape_params(mu = 0.8, sd = (0.4)^2)$b)

# distribution of specificity of test
dist_Sp = truncdist::rtrunc(n = 100000,spec = "beta",a = 0.9998,b = 1,
                            shape1 = find_beta_shape_params(mu = 0.99995, sd = (0.01)^2)$a,
                            shape2 = find_beta_shape_params(mu = 0.99995, sd = (0.01)^2)$b)

process_priors = function(priors, Se, Sp){
  
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
est_P_A_testpos = function(P_S_untested, Z_A, Z_S){
  
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
p0 <- function(x){
  truncdist::dtrunc(x = x,spec = "beta",a = 0.25,b = 0.7,
                    shape1 = find_beta_shape_params(mu = 0.4, sd = (0.35)^2)$a, 
                    shape2 = find_beta_shape_params(mu = 0.4, sd = (0.35)^2)$b)
}


##############################################
# Documentation:  samp_p0
# Usage:          samp_p0(x)
# Description:    function to create truncated beta distribution
#                 for P(A|test+)
#
# Args/Options:   none

# Returns:        randomly sampled distribution of P(A|test+)
# Output:         none
##############################################
samp_p0 <- function(n){
  truncdist::rtrunc(n = n,spec = "beta",a = 0.25,b = 0.7,
                    shape1 = find_beta_shape_params(mu = 0.4, sd = (0.35)^2)$a, 
                    shape2 = find_beta_shape_params(mu = 0.4, sd = (0.35)^2)$b)
}


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
constrain_priors = function(priors){
  
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

