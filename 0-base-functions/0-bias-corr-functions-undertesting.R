##############################################
# Documentation:  calc_A_star_undertesting
# Usage:          calc_A_star_undertesting(N, N_tested, N_pos_obs, P_testpos_est, P_S_tested,  
#                 P_S_untested, P_A_testpos, Z_S, Z_A, Se, Sp)
# Description:    estimate the number infections correcting for
#                 incomplete testing and imperfect test accuracy and
#                 the proportion of cases under-estimated due to
#                 incomplete testing vs. imperfect test accuracy
#
# Args/Options:   
# N:              population size
# N_tested:       number of people tested
# N_pos_obs:      number of confirmed COVID-19 cases
# P_testpos_est:  empirical estimate of P(test+|tested)
# P_S_tested:     prior value for P(S|tested)
# P_S_untested:   prior value for P(S|untested)
# P_A_testpos:    prior value for P(A|test +)
# Z_S:            prior value for alpha range
# Z_A:            prior value for beta range
# Se:             prior value for sensitivity
# Sp:             prior value for specificity

# Returns:        the number of estimated infections correcting for
#                 incomplete testing and imperfect test accuracy (A), 
#                 the number of estimated infections correcting for
#                 incomplete testing assuming perfect test accuracy (A_star), 
#                 the percentage of under-estimation attributable to 
#                 imperfect test accuracy (percent_acc), and the percentage of
#                 under-estimation due to incomplete testing (percent_und), as a list
# Output:         none
##############################################
calc_A_star_undertesting = function(N, N_tested, N_pos_obs, P_testpos_est, P_S_tested, P_S_untested, P_A_testpos, Z_S, Z_A, Se, Sp){
  
  N_untested = N - N_tested
  
  #----- NS, NA among tested ----------------------
  Npos_tested_S = N_pos_obs * (1 - P_A_testpos)
  Npos_tested_A = N_pos_obs - Npos_tested_S
  
  #----- prob testpos among untested ----------------------
  P_testpos_S = P_testpos_est * Z_S
  P_testpos_A = P_testpos_est * Z_A
  
  # estimate number of positives amogn untested
  Npos_untested_S = P_S_untested * N_untested * P_testpos_S 
  Npos_untested_A = (1 - P_S_untested) * N_untested * P_testpos_A
  
  A_star = Npos_tested_S   + Npos_tested_A +
    Npos_untested_S + Npos_untested_A
  
  # correct for imperfect sensitivity and specificity
  A = (A_star - ((1 - Sp) * N)) / (Se + Sp - 1)

  increase_infections = A - N_pos_obs
  percent_acc = (A - A_star) / increase_infections
  percent_und = 1 - percent_acc
  
  return(list(
    A = A,
    A_star = A_star,
    percent_acc = percent_acc,
    percent_und = percent_und))
  
}

##############################################
# Documentation:  correct_bias_undertesting
# Usage:          correct_bias_undertesting(N, N_tested, N_pos_obs, P_testpos_est, distributions)
# Description:    perform probabilistic bias analysis to estimate the 
#                 number infections correcting for incomplete testing and 
#                 imperfect test accuracy    
#
# Args/Options:   
# N:              population size
# N_tested:       number of people tested
# N_pos_obs:      number of confirmed COVID-19 cases
# P_testpos_est:  empirical estimate of P(test+|tested)
# distributions:  data frame with prior distributions for P_S_tested, P_S_untested, 
#                 P_A_testpos, Z_S, Z_A, Se, Sp

# Returns:        data frame with the number of estimated infections correcting for
#                 incomplete testing and imperfect test accuracy (exp_cases), 
#                 the number of estimated infections correcting for
#                 incomplete testing assuming perfect test accuracy (exp_cases_perfSeSp), 
#                 the percentage of under-estimation attributable to 
#                 imperfect test accuracy (percent_acc), and the percentage of
#                 under-estimation due to incomplete testing (percent_und) and prior distributions
# Output:         none
##############################################
correct_bias_undertesting = function(N, N_tested, N_pos_obs, P_testpos_est, distributions){
  
  cat(".")
  
  # sample index to draw from distribution
  sample_ind = sample(1:nrow(distributions), size = 1, replace=TRUE)
  
  # randomly sample from each distribution 
  samples = distributions[sample_ind,]
  
  # corrected case count
  res = calc_A_star_undertesting(N = N,
                      N_tested = N_tested,
                      N_pos_obs = N_pos_obs,
                      P_testpos_est = P_testpos_est,
                      P_S_tested = samples[which(names(samples) == "P_S_tested")],
                      P_S_untested = samples[which(names(samples) == "P_S_untested")],
                      P_A_testpos = samples[which(names(samples) == "P_A_testpos")],
                      Z_S = samples[which(names(samples) == "Z_S")],
                      Z_A = samples[which(names(samples) == "Z_A")],
                      Se = samples[which(names(samples) == "dist_Se")],
                      Sp = samples[which(names(samples) == "dist_Sp")]
  )
  
  out = data.frame(
    exp_cases = res$A,
    exp_cases_perfSeSp = res$A_star,
    percent_acc = res$percent_acc,
    percent_und = res$percent_und,
    N = N,
    N_tested = N_tested,
    N_pos = N_pos_obs,
    P_S_tested = samples[which(names(samples) == "P_S_tested")],
    P_S_untested = samples[which(names(samples) == "P_S_untested")],
    Z_S = samples[which(names(samples) == "Z_S")],
    Z_A = samples[which(names(samples) == "Z_A")],
    P_A_testpos = samples[which(names(samples) == "P_A_testpos")],
    P_testpos_S = samples[which(names(samples) == "P_testpos_S")],
    P_testpos_A = samples[which(names(samples) == "P_testpos_A")],
    Se = samples[which(names(samples) == "dist_Se")],
    Sp = samples[which(names(samples) == "dist_Sp")]
  )
  
  return(out)
}

##############################################
# Documentation:  generate_corrected_sample
# Usage:          generate_corrected_sample(N, N_tested, N_pos_obs, distribution_list, 
#                 num_reps, state, select_state=NULL)
# Description:    wrapper for correct_bias_undertesting that subsets a list of prior distributions
#                 to those for a particular state, performs probabilistic bias 
#                 analysis for that state, and formats output
#
# Args/Options:   
# N:              population size
# N_tested:       number of people tested
# N_pos_obs:      number of confirmed COVID-19 cases
# P_testpos_est:  empirical estimate of P(test+|tested)
# distributions:  data frame with prior distributions for P_S_tested, P_S_untested, 
#                 P_A_testpos, Z_S, Z_A, Se, Sp

# Returns:        data frame with the number of estimated infections correcting for
#                 incomplete testing and imperfect test accuracy (exp_cases), 
#                 the number of estimated infections correcting for
#                 incomplete testing assuming perfect test accuracy (exp_cases_perfSeSp), 
#                 the percentage of under-estimation attributable to 
#                 imperfect test accuracy (percent_acc), and the percentage of
#                 under-estimation due to incomplete testing (percent_und) and prior values
# Output:         none
##############################################
generate_corrected_sample = function(N, N_tested, N_pos_obs, distribution_list, num_reps, 
                                     select_state=NULL){
  
  #----------------------------------------
  # Obtain corrected case estimates
  #----------------------------------------
  reps = num_reps
  
  distributions = distribution_list[which(names(distribution_list) == select_state)][[1]]
  
  # need to set seed here to ensure that the same random draws are
  # used for a given time period / location with same priors
  set.seed(123)

  result = replicate(reps, correct_bias_undertesting(
    N = N, 
    N_tested = N_tested, 
    N_pos_obs = N_pos_obs, 
    P_testpos_est = mean(distributions$est_testpos), 
    distributions = distributions
  ))
  
  result_long = as.data.frame(matrix(result, nrow=reps, byrow=TRUE))
  colnames(result_long) = c(
    "exp_cases", "exp_cases_perfSeSp", 
    "percent_acc", "percent_und",
    "N", "N_tested", "N_pos",
    "P_S_tested", "P_S_untested",
    "Z_S", "Z_A", "P_A_testpos",
    "P_testpos_S", "P_testpos_A", "Se", "Sp"
  )
  
  for(i in 1:ncol(result_long)){
    result_long[,i] = unlist(result_long[,i])
  }
  
  return(result_long)
}
