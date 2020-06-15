#######################################
# COVID-19 estimated infections
# correcting for incomplete testing and
# imperfect test accuracy

# Plot of COVID-19 cumulative number of infections
# distribution from simulation
#######################################
rm(list=ls())
library(boxr)
source(paste0(here::here(), "/0-config.R"))

# samples = as.data.frame(box_search("NO_PUSH_corrected_samples_us_state", ancestor_folder_ids = "115224581369"))
# latest = samples %>% filter(modified_at == max(samples$modified_at))
# latest_file_id = latest$id
# dist = box_read(latest_file_id)

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

ggsave(plot, filename = paste0(plot_path, "fig-state-cases-distribution.png"),
       width = 10, height=8)