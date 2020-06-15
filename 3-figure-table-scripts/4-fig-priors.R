#######################################
# COVID-19 estimated infections
# correcting for incomplete testing and
# imperfect test accuracy
#
# Priors figure
# Assumes priors are the same in all states
# except for P_testpos_A, P_testpos_S
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))


# priors = box_search("NO_PUSH_state_priors_out.RDS") %>% box_read()
priors = readRDS(paste0(results_path, "NO_PUSH_state_priors_out.RDS"))

state_abbrev <- read_csv(state_abbrev_path) %>%
  rename(state = Abbreviation,
         statename = State)


#--------------------------------------
# figure of static priors
#--------------------------------------
static_priors = priors$AK %>%
  dplyr::select(-c( P_testpos_A, P_testpos_S,
                   est_testpos)) %>%
  mutate(P_S_testpos = 1 - P_A_testpos)

priorsl = melt(static_priors)

priorsl = priorsl %>% mutate(variable = case_when(
  variable == "P_S_tested" ~ "P(S1|tested)",
  variable == "P_S_untested" ~ "P(S1|untested)",
  variable == "Z_S" ~ "alpha",
  variable == "Z_A" ~ "beta",
  variable == "P_testpos_S" ~ "P(test + |S1)",
  variable == "P_testpos_A" ~ "P(test + |S0)",
  variable == "P(S|test+)" ~ "P(S1 | test +)",
  variable == "P(A|test+)" ~ "P(S0 | test +)",
  variable == "dist_Se" ~ "Sensitivity",
  variable == "dist_Sp" ~ "Specificity"
))%>%
  filter(!is.na(variable)) %>%
  mutate(variable = factor(variable, levels= c(
  "P(S1|tested)", "P(S1|untested)",
  "alpha","beta",
  "P(test + |S1)","P(test + |S0)",
  "P(S1 | test +)","P(S0 | test +)",
  "Sensitivity", "Specificity"
)))

plot_dist = ggplot(priorsl, aes(x = value)) +
  geom_density(fill = "grey") +
  facet_wrap(~variable, ncol = 2, scales = "free") +
  scale_x_continuous(limits = c(0,1)) +
  xlab("Probability") + ylab("") +
  scale_fill_manual("Date", values = c("#FF57A3","#2181E7")) +
  theme_bw() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom")

ggsave(plot_dist, filename = paste0(plot_path, "fig-priors-static.pdf"),
       width = 8, height = 8)


#--------------------------------------
# figure of static priors: density functions
#--------------------------------------

# density functions
dens_P_S_tested <- function(x, alpha = 20, beta = 1.4){
  dbeta(x = x,shape1 = alpha,shape2 = beta,ncp = 0,log = FALSE)
}

# function factory for truncated beta densities
make_trunc_beta_dens <- function(a, b, mu, sd){
  stopifnot(all(c(a,b,mu,sd) >= 0))

  pars <- find_beta_shape_params(mu = mu, sd = sd)

  dens <- function(x){
    truncdist::dtrunc(
      x = x, spec = "beta", a = a, b = b,
      shape1 = pars$a, shape2 = pars$b, ncp = 0, log = FALSE
    )
  }

  return(dens)
}

# the densities that follow TruncBeta
dens_P_S_untested <- make_trunc_beta_dens(a = 0,b = 0.15,mu = 0.025,sd = 0.15^2)

dens_alpha <-  make_trunc_beta_dens(a = 0.8,b = 1,mu = 0.9,sd = 0.2^2)

dens_beta <-  make_trunc_beta_dens(a = 0.002,b = 0.4,mu = 0.15,sd = 0.3^2)

dens_se <- make_trunc_beta_dens(a = 0.65,b = 1,mu = 0.8,sd = 0.4^2)

dens_sp <- make_trunc_beta_dens(a = 0.998,b = 1,mu = 0.99995,sd = 0.01^2)

# densities for P(S0|test+) and P(S1|test+) prior to Bayesian melding
dens_P_S0_testpos <- make_trunc_beta_dens(a = 0.25,b = 0.7,mu = 0.4,sd = 0.35^2)

# the densities that underwent Bayesian melding and need to have a kernel smoothing done
# for visualisation

# get them into vectors for density estimation
P_S1_testpos_vec <- priorsl %>%
  filter(variable == "P(S1 | test +)") %>%
  select(value) %>%
  unlist() %>%
  unname()

P_S0_testpos_vec <- priorsl %>%
  filter(variable == "P(S0 | test +)") %>%
  select(value) %>%
  unlist() %>%
  unname()

P_S1_testpos_kde <- density(
  x = P_S1_testpos_vec,
  from = 0,to = 1,kernel = "gaussian",
  bw = 0.025,n = 1024
)

P_S0_testpos_kde <- density(
  x = P_S0_testpos_vec,
  from = 0,to = 1,kernel = "gaussian",
  bw = 0.025,n = 1024
)

xseq <- P_S0_testpos_kde$x

prior_df4plot <- data.frame(
  variable = rep(x = c("P(S1|tested)", "P(S1|untested)",
                       "alpha","beta",
                       "P(S1 | test +)","P(S0 | test +)",
                       "Sensitivity"),each = length(xseq)),
  x = rep(xseq,times = 7),
  y = NaN,
  stringsAsFactors = FALSE
)

prior_df4plot[prior_df4plot$variable == "P(S1|tested)","y"] <- dens_P_S_tested(x = xseq)
prior_df4plot[prior_df4plot$variable == "P(S1|untested)","y"] <- dens_P_S_untested(x = xseq)
prior_df4plot[prior_df4plot$variable == "alpha","y"] <- dens_alpha(x = xseq)
prior_df4plot[prior_df4plot$variable == "beta","y"] <- dens_beta(x = xseq)

# post Bayesian melding
prior_df4plot[prior_df4plot$variable == "P(S1 | test +)","y"] <- P_S1_testpos_kde$y
prior_df4plot[prior_df4plot$variable == "P(S0 | test +)","y"] <- P_S0_testpos_kde$y

# Sensitivity and specificity
prior_df4plot[prior_df4plot$variable == "Sensitivity","y"] <- dens_se(x = xseq)

# use the results from ggplot2 for specificity
plot_sp <- ggplot(priorsl[priorsl$variable=="Specificity",], aes(x = value)) +
  geom_density()
plot_sp_build <- ggplot_build(plot_sp)

# do specificity seperately because it has such a small range
prior_df4plot <- rbind(
  prior_df4plot,data.frame(
    variable = "Specificity",
    x = c(seq(from=0,to=0.999,length.out = 10),plot_sp_build$data[[1]]$x),
    y = c(rep(0,10),plot_sp_build$data[[1]]$scaled),
    stringsAsFactors = FALSE
  )
)

# the figure
prior_plot_smooth <- ggplot(data = prior_df4plot) +
  geom_path(aes(x=x,y=y),color="black") +
  geom_polygon(data = prior_df4plot[prior_df4plot$variable!="Specificity",],
               aes(x=x,y=y),fill="grey",color="black") +
  facet_wrap(~variable, ncol = 2, scales = "free") +
  scale_x_continuous(limits = c(0,1)) +
  xlab("Probability") + ylab("") +
  theme_bw() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom")

ggsave(prior_plot_smooth, filename = paste0(plot_path, "fig-priors-static-smooth.pdf"),
       width = 8, height = 8)

# pre Bayesian melding
prior_preMeld <- data.frame(
  variable = rep(c("P(S1 | test +)","P(S0 | test +)"),each = length(xseq)),
  x = rep(xseq, times = 2),
  y = NaN,
  stringsAsFactors = FALSE
)
prior_preMeld[prior_preMeld$variable == "P(S0 | test +)","y"] <- dens_P_S0_testpos(x = xseq)
prior_preMeld[prior_preMeld$variable == "P(S1 | test +)","y"] <- rev(dens_P_S0_testpos(x = xseq))

# the figure
prior_plot_smooth_prebayes <- ggplot(data = prior_df4plot) +
  geom_path(aes(x=x,y=y),color="black") +
  geom_polygon(data = prior_df4plot[prior_df4plot$variable!="Specificity",],
               aes(x=x,y=y),fill="grey",color="black") +
  geom_polygon(data = prior_preMeld,
               aes(x=x,y=y),alpha=0.5,color=adjustcolor("black",alpha.f = 0.65),fill="grey") +
  facet_wrap(~variable, ncol = 2, scales = "free") +
  scale_x_continuous(limits = c(0,1)) +
  xlab("Probability") + ylab("") +
  theme_bw() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom")

ggsave(prior_plot_smooth_prebayes, filename = paste0(plot_path, "fig-priors-static-smooth-prebayes.png"),
       width = 8, height = 8)

#--------------------------------------
# table - static priors
#--------------------------------------
get_prior_parameters <- function(){

  p2_shape <- find_beta_shape_params(mu = 0.025, sd = (0.15)^2)
  zs_shape <- find_beta_shape_params(mu = 0.9, sd = (0.2)^2)
  za_shape <- find_beta_shape_params(mu = 0.15, sd = (0.3)^2)
  p0_shape <- find_beta_shape_params(mu = 0.4, sd = (0.35)^2)
  Se_shape <- find_beta_shape_params(mu = 0.8, sd = (0.4)^2)
  Sp_shape <- find_beta_shape_params(mu = 0.99995, sd = (0.01)^2)

  prior_parameters <- list(
    p1_shape1 = 20, p1_shape2 = 1.4, # P(S1|tested)
    p2_a = 0, p2_b = 0.15, p2_shape1 = p2_shape$a, p2_shape2 = p2_shape$b, # P(S1|untested)
    zs_a = 0.8, zs_b = 1, zs_shape1 = zs_shape$a, zs_shape2 = zs_shape$b, # alpha
    za_a = 0.002, za_b = 0.4, za_shape1 = za_shape$a, za_shape2 = za_shape$b, # beta
    p0_a = 0.25, p0_b = 0.7, p0_shape1 = p0_shape$a, p0_shape2 = p0_shape$b, # P(S0|test+)
    Se_a = 0.65, Se_b = 1, Se_shape1 = Se_shape$a, Se_shape2 = Se_shape$b, # sensitivity
    Sp_a = 0.9998, Sp_b = 1, Sp_shape1 = Sp_shape$a, Sp_shape2 = Sp_shape$b # specificity
  )

  return(prior_parameters)
}

get_values_prior <- function(parameters){

  out <- replicate(n=7,expr={NULL})
  out <- setNames(out,nm = c("P(S1|tested)", "P(S1|untested)", "alpha", "beta", "P(S0|test+)", "Se", "Sp"))

  # P(S1|tested)
  mean_p1 <- parameters$p1_shape1 / (parameters$p1_shape1 + parameters$p1_shape2)

  out[["P(S1|tested)"]]$mean <- mean_p1
  out[["P(S1|tested)"]]$min <- 0
  out[["P(S1|tested)"]]$max <- 1
  out[["P(S1|tested)"]]$shape1 <- parameters$p1_shape1
  out[["P(S1|tested)"]]$shape2 <- parameters$p1_shape2

  # P(S1|untested)
  integrand <- function(x){
    truncdist::dtrunc(
      x = x, spec = "beta", a = parameters$p2_a, b = parameters$p2_b,
      shape1 = parameters$p2_shape1, shape2 = parameters$p2_shape2, ncp = 0
    ) * x
  }

  mean_p2 <- integrate(integrand,lower=0,upper=1)$value

  out[["P(S1|untested)"]]$mean <- mean_p2
  out[["P(S1|untested)"]]$min <- parameters$p2_a
  out[["P(S1|untested)"]]$max <- parameters$p2_b
  out[["P(S1|untested)"]]$shape1 <- parameters$p2_shape1
  out[["P(S1|untested)"]]$shape2 <- parameters$p2_shape2

  # alpha
  integrand <- function(x){
    truncdist::dtrunc(
      x = x, spec = "beta", a = parameters$zs_a, b = parameters$zs_b,
      shape1 = parameters$zs_shape1, shape2 = parameters$zs_shape2, ncp = 0
    ) * x
  }

  mean_zs <- integrate(integrand,lower=0,upper=1)$value

  out[["alpha"]]$mean <- mean_zs
  out[["alpha"]]$min <- parameters$zs_a
  out[["alpha"]]$max <- parameters$zs_b
  out[["alpha"]]$shape1 <- parameters$zs_shape1
  out[["alpha"]]$shape2 <- parameters$zs_shape2

  # beta
  integrand <- function(x){
    truncdist::dtrunc(
      x = x, spec = "beta", a = parameters$za_a, b = parameters$za_b,
      shape1 = parameters$za_shape1, shape2 = parameters$za_shape2, ncp = 0
    ) * x
  }

  mean_za <- integrate(integrand,lower=0,upper=1)$value

  out[["beta"]]$mean <- mean_za
  out[["beta"]]$min <- parameters$za_a
  out[["beta"]]$max <- parameters$za_b
  out[["beta"]]$shape1 <- parameters$za_shape1
  out[["beta"]]$shape2 <- parameters$za_shape2

  # P(S0|test+)
  integrand <- function(x){
    truncdist::dtrunc(
      x = x, spec = "beta", a = parameters$p0_a, b = parameters$p0_b,
      shape1 = parameters$p0_shape1, shape2 = parameters$p0_shape2, ncp = 0
    ) * x
  }

  mean_p0 <- integrate(integrand,lower=0,upper=1)$value

  out[["P(S0|test+)"]]$mean <- mean_p0
  out[["P(S0|test+)"]]$min <- parameters$p0_a
  out[["P(S0|test+)"]]$max <- parameters$p0_b
  out[["P(S0|test+)"]]$shape1 <- parameters$p0_shape1
  out[["P(S0|test+)"]]$shape2 <- parameters$p0_shape2

  # sensitivity
  integrand <- function(x){
    truncdist::dtrunc(
      x = x, spec = "beta", a = parameters$Se_a, b = parameters$Se_b,
      shape1 = parameters$Se_shape1, shape2 = parameters$Se_shape2, ncp = 0
    ) * x
  }

  mean_Se <- integrate(integrand,lower=0,upper=1)$value

  out[["Se"]]$mean <- mean_Se
  out[["Se"]]$min <- parameters$Se_a
  out[["Se"]]$max <- parameters$Se_b
  out[["Se"]]$shape1 <- parameters$Se_shape1
  out[["Se"]]$shape2 <- parameters$Se_shape2

  # specificity
  integrand <- function(x){
    truncdist::dtrunc(
      x = x, spec = "beta", a = parameters$Sp_a, b = parameters$Sp_b,
      shape1 = parameters$Sp_shape1, shape2 = parameters$Sp_shape2, ncp = 0
    ) * x
  }

  mean_Sp <- integrate(integrand,lower=parameters$Sp_a-0.01,upper=1,subdivisions=1e3)$value

  out[["Sp"]]$mean <- mean_Sp
  out[["Sp"]]$min <- parameters$Sp_a
  out[["Sp"]]$max <- parameters$Sp_b
  out[["Sp"]]$shape1 <- parameters$Sp_shape1
  out[["Sp"]]$shape2 <- parameters$Sp_shape2
  
  out <- do.call(rbind,out)
  out <- out[,c("min","mean","max","shape1","shape2")]
  storage.mode(out) <- "numeric"
  return(out)
}


pars <- get_prior_parameters()
prior_vals <- get_values_prior(parameters = pars)

write.csv(round(prior_vals,5), file = paste0(results_path, "input_dist_range.csv"))


#--------------------------------------
# table - state-specific priors
#--------------------------------------
sspriors = list()
for(i in 1:length(priors)){
  state_priors = priors[[i]]
  sspriors[[i]] = state_priors %>% dplyr::select(c(P_testpos_A, P_testpos_S))
}
names(sspriors) = names(priors)
sspriors_df = bind_rows(sspriors)
sspriors_df = sspriors_df %>% mutate(
  state = rep(names(priors), each=nrow(priors[[1]])))

sspriors_df = sspriors_df %>%
  left_join(state_abbrev, by = "state")

sspriors_df = sspriors_df %>%
  group_by(statename) %>%
  summarise(S1_min = sprintf("%0.03f", min(P_testpos_S)),
            S1_med = sprintf("%0.03f", median(P_testpos_S)),
            S1_max = sprintf("%0.03f", max(P_testpos_S)),
            S0_min = sprintf("%0.03f", min(P_testpos_A)),
            S0_med = sprintf("%0.03f", median(P_testpos_A)),
            S0_max = sprintf("%0.03f", max(P_testpos_A)))

write.csv(sspriors_df, file = paste0(results_path, "input_dist_range_state.csv"))
