#######################################
# COVID-19 estimated infections
# correcting for incomplete testing and
# imperfect test accuracy

# Plot of COVID-19 cumulative number of infections
# distribution from simulation
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

tmpshot <- fileSnapshot(paste0(results_path, "bias-corrected-distributions/state/"))
latest = rownames(tmpshot$info[which.max(tmpshot$info$mtime),])
dist = readRDS(paste0( results_path, "/bias-corrected-distributions/state/", latest))

data = readRDS(paste0(results_path, "covid_usa_state_adjusted.RDS"))

state_abbrev <- read_csv(state_abbrev_path) %>%
  rename(state = Abbreviation,
         statename = State)

#--------------------------------------
# process state distributions
#--------------------------------------
state_abbrev <- read_csv(state_abbrev_path)

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
  left_join(state_abbrev, by = c("state" = "Abbreviation")) %>%
  rename("statename" = "state")

plotdf$statename = factor(plotdf$statename)
plotdf$statename_f = fct_reorder(plotdf$statename, plotdf$med)

plotbreaks = c(0, 1000, 10000, 100000, 1000000)

# plot = ggplot(plotdf, aes(y = exp_cases, x = statename_f)) +
#   geom_boxplot(aes(fill = log10(med)),
#                outlier.stroke = 0.01, lwd = 0.2) +
#   scale_y_log10(breaks = plotbreaks,
#                 labels = format(plotbreaks, scientific = F, big.mark = ",")) +
#   scale_fill_viridis("log10(median)", begin = 0.3, end = 0.95, direction = -1, option = "A") +
#   ylab("Distribution of estimated COVID-19 infections") +
#   xlab("") +
#   coord_flip() +
#   theme_bw() +
#   theme(legend.position="none")
# plot

plotdf %>% 
  group_by(statename_f) %>%
  summarise(med = median(exp_cases), 
            q025 = quantile(x = exp_cases,probs = 0.025),
            q975 = quantile(x = exp_cases,probs = 0.975),
            q25 = quantile(x = exp_cases,probs = 0.25),
            q75 = quantile(x = exp_cases,probs = 0.75),
            name = unique(State)
            ) -> plotdf_summary

plotdf_summary$name = factor(plotdf_summary$name)
plotdf_summary$name = fct_reorder(plotdf_summary$name, plotdf_summary$med)

plot <- ggplot(data = plotdf_summary) +
  geom_boxplot(
    aes(
      x = name,
      middle = med, 
      ymin = q025, 
      ymax = q975, 
      lower = q25,
      upper = q75,
      fill = log10(med)
        ),
    outlier.shape = NA,stat = "identity"
    ) +
  scale_y_log10(breaks = plotbreaks,
                labels = format(plotbreaks, scientific = F, big.mark = ",")) +
  scale_fill_viridis("log10(median)", begin = 0.3, end = 0.95, direction = -1, option = "A") +
  ylab("Distribution of estimated COVID-19 infections") +
  xlab("") +
  coord_flip() +
  theme_bw() +
  theme(axis.title = element_text(size = rel(1.15)),axis.text = element_text(size = rel(1.10)), legend.position="none")

ggsave(plot, filename = paste0(plot_path, "fig-state-cases-distribution.png"),
       width = 10, height=8)
