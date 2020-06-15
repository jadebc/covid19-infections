
source(paste0(here::here(), "/0-config.R"))

set.seed(123)

#---------------------------------------
# Define distribution of P(A|test+)
#---------------------------------------
# for simplicity, p0 := P(A|test+)
p0 <- function(x){
  truncdist::dtrunc(x = x,spec = "beta",a = 0.25,b = 0.7,
                    shape1 = find_beta_shape_params(mu = 0.4, sd = (0.35)^2)$a, 
                    shape2 = find_beta_shape_params(mu = 0.4, sd = (0.35)^2)$b)
}

samp_p0 <- function(n){
  truncdist::rtrunc(n = n,spec = "beta",a = 0.25,b = 0.7,
                    shape1 = find_beta_shape_params(mu = 0.4, sd = (0.35)^2)$a, 
                    shape2 = find_beta_shape_params(mu = 0.4, sd = (0.35)^2)$b)
}

x <- seq(0.25,0.8,by=0.001)
plot(x,p0(x),type="l",lwd=1,col="darkorchid3",main="Prior on P(A|test+)")
abline(v=0.62,col="firebrick3",lty=2)

#---------------------------------------
# Define prior distributions
#---------------------------------------
# p1 := P(S|tested)
samp_p1 <- function(n){
  truncdist::rtrunc(n = 100000,spec = "beta",a = 0.9,b = 1,
                    shape1 = find_beta_shape_params(mu = 0.975, sd = (0.025)^2)$a, 
                    shape2 = find_beta_shape_params(mu = 0.975, sd = (0.025)^2)$b)
}

# p2 := P(S|untested)
samp_p2 <- function(n){
  rbeta(n = n,shape1 = 3, shape2 = 65)
  truncdist::rtrunc(n = 100000,spec = "beta",a = 0,b = 0.05,
                    shape1 = find_beta_shape_params(mu = 0.01, sd = (0.01)^2)$a, 
                    shape2 = find_beta_shape_params(mu = 0.01, sd = (0.01)^2)$b)
}

# p3 := P(test+|S)
samp_p3 <- function(n){
  truncdist::rtrunc(n = n,spec = "beta",a = 0.05,b = 0.3,
                    shape1 = find_beta_shape_params(mu = 0.125, sd = (0.22)^2)$a, 
                    shape2 = find_beta_shape_params(mu = 0.125, sd = (0.22)^2)$b)
}

# p4 := P(test+|A)
samp_p4 <- function(n){
  truncdist::rtrunc(n = n,spec = "beta",a = 0.002,b = 0.15,
                    shape1 = find_beta_shape_params(mu = 0.06, sd = (0.25)^2)$a, 
                    shape2 = find_beta_shape_params(mu = 0.06, sd = (0.25)^2)$b)
}

pre_prior = data.frame(
  P_S_test = samp_p1(n=10000),
  P_S_untest = samp_p2(n=10000),
  P_testpos_S = samp_p3(n=10000),
  P_testpos_A = samp_p4(n=10000),
  P_A_testpos = samp_p0(n=10000)
) %>% melt()

pre_prior = ggplot(pre_prior, aes(x = value)) + 
  geom_histogram(color="black", fill="white", bins=80) + 
  scale_x_continuous(limits=c(0,1)) + 
  facet_wrap(~variable, scales = "free")+ 
  theme_bw() + xlab("") + ylab("") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

ggsave(pre_prior, filename = paste0(plot_path, "fig-distributions-early-pre.png"),
       width=6, height=4)


# set.seed(123)
# dist_P_S_tested = rbeta(n = 100000, shape1 = 20, shape2 = 1.4)
# 
# # probability of being symptomatic among those not tested
# set.seed(123)
# dist_P_S_untested = rbeta(n = 100000, shape1 = 3, shape2 = 11) 
# 
# # probability of testing positive among the symptomatic
# set.seed(123)
# dist_P_testpos_S = rbeta(n = 100000, shape1 = 40, shape2 = 10)
# 
# # probability of testing positive among the asymptomatic
# set.seed(123)
# dist_P_testpos_A = rbeta(n = 100000, shape1 = 5, shape2 = 20)
# 
# sample the prior
sample_prior <- function(n){
  stopifnot(n>0)
  out <- matrix(data = NaN,nrow = n,ncol = 4,dimnames = list(NULL,c("p1","p2","p3","p4")))
  out[,"p1"] <- samp_p1(n)
  out[,"p2"] <- samp_p2(n)
  out[,"p3"] <- samp_p3(n)
  out[,"p4"] <- samp_p4(n)
  return(out)
}

#---------------------------------------
# calculate P(A|test+)
#---------------------------------------
est_P_AS_testpos = function(P_S_tested, P_S_untested, P_testpos_S, P_testpos_A,
                            N_tested, N){
  
  # scale up tested population assuming everyone was tested,
  # stratify by symptom status
  N_S = (P_S_tested * N_tested) + (P_S_untested*(N - N_tested))
  N_A = (N_tested - (P_S_tested * N_tested)) + 
    (N - N_tested - (P_S_untested*(N - N_tested)))
  
  N_A_testpos = P_testpos_A * N_A
  N_S_testpos = P_testpos_S * N_S
  
  P_A_testpos = N_A_testpos / (N_A_testpos + N_S_testpos)
  
  return(P_A_testpos)
}
est_P_AS_testpos <- Vectorize(est_P_AS_testpos)

#---------------------------------------
# Run the SIR algorithm to sample from 
# the induced "posterior" on theta
#---------------------------------------
# number of samples from the prior
nsamp <- 1e5

theta_samp <- sample_prior(n = nsamp)

phi <- est_P_AS_testpos(
  P_S_tested = theta_samp[,1],
  P_S_untested = theta_samp[,2],
  P_testpos_S = theta_samp[,3],
  P_testpos_A = theta_samp[,4],
  N_tested = 103945,
  N = 331002651
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
  dimnames = list(NULL,c("P(S|tested)","P(S|untested)","P(test+|S)","P(test+|A)","P(A|test+)"))
)

pi_samp[1:nsamp_post,1:4] <- theta_samp[post_samp_ind,]
pi_samp[1:nsamp_post,5] <- phi[post_samp_ind]

pi_samp = cbind(pi_samp, `P(S|test+)` = 1 - pi_samp[,5])

BayesianTools::correlationPlot(pi_samp)

#---------------------------------------
# Create data frame of all input distributions
#---------------------------------------
# distribution of sensitivity of test
dist_Se = truncdist::rtrunc(n = 100000,spec = "beta",a = 0.65,b = 1,
                            shape1 = find_beta_shape_params(mu = 0.8, sd = (0.15)^2)$a, 
                            shape2 = find_beta_shape_params(mu = 0.8, sd = (0.15)^2)$b)

# distribution of specificity of test
dist_Sp = rbeta(n = 100000, shape1 = 200, shape2 = 1)

simdata = as.data.frame(pi_samp) %>%
  rename(
    dist_P_S_tested = `P(S|tested)`,
    dist_P_S_untested = `P(S|untested)`,
    dist_P_testpos_S = `P(test+|S)`,
    dist_P_testpos_A = `P(test+|A)`,
    dist_P_S_testpos = `P(S|test+)`,
    dist_P_A_testpos = `P(A|test+)`
  ) %>%
  mutate(dist_Se = dist_Se,
         dist_Sp = dist_Sp)

#---------------------------------------
# Plot priors
#---------------------------------------
simdatal = melt(simdata)

simdatal = simdatal %>% mutate(label = case_when(
  variable == "dist_P_S_tested" ~ "P(Symptomatic | tested)",
  variable == "dist_P_S_untested" ~  "P(Symptomatic | not tested)", 
  variable == "dist_P_testpos_S" ~  "P(Test + | Symptomatic)", 
  variable == "dist_P_testpos_A" ~  "P(Test + | Asymptomatic)", 
  variable == "dist_P_S_testpos" ~  "P(Symptomatic | Test +)",
  variable == "dist_P_A_testpos" ~  "P(Asymptomatic | Test +)",
  variable == "dist_Se" ~  "Test sensitivity",
  variable == "dist_Sp" ~  "Test specificity"
)) %>%
  mutate(label = factor(label, levels = c(
    "P(Symptomatic | tested)","P(Symptomatic | not tested)", 
    "P(Test + | Symptomatic)", "P(Test + | Asymptomatic)", 
    "P(Symptomatic | Test +)", "P(Asymptomatic | Test +)",
    "Test sensitivity","Test specificity"
  )))

dist_plot = ggplot(simdatal, aes(x = value)) +
  geom_histogram(color="black", fill="white", bins=80) + 
  facet_wrap(~label, ncol = 2, scales = "free") +
  scale_x_continuous(limits=c(0,1)) + 
  theme_bw() + xlab("") + ylab("") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#---------------------------------------
# Table of priors
#---------------------------------------
dist_tab = data.frame(
    lab = colnames(simdata),
    min = sprintf("%0.02f", sapply(simdata, min)),
    med = sprintf("%0.02f", sapply(simdata, median)),
    max = sprintf("%0.02f", sapply(simdata, max))
  )


#---------------------------------------
# Save output
#---------------------------------------
ggsave(dist_plot, filename = paste0(plot_path, "fig-distributions-early.png"),
       width=6, height=6)

saveRDS(simdata, paste0(results_path, "input_distributions_early.RDS"))

write.csv(dist_tab, paste0(results_path, "input_dist_early_range.csv"))
