# --------------------------------------------------------------------------------
#
#   Produce results
#
# --------------------------------------------------------------------------------

rm(list=ls());gc()
library(tidyverse)
library(grid)
library(here)

# load base functions for the analysis
source(here::here("/0-config.R"))

# load data
sens <- readRDS(here::here("6-sensitivity/NO_PUSH_sens.RDS"))

# --------------------------------------------------------------------------------
#   process output
# --------------------------------------------------------------------------------

plot_data <- replicate(n = length(sens),expr = {NULL},simplify = FALSE)

state_abbrev <- read_csv(state_abbrev_path) %>%
  rename(state = Abbreviation,
         statename = State)

for(j in 1:length(sens)){

  cat("--- j: ",j," --- \n")

  dist <- sens[[j]]

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
  plotdf$scenario <- as.integer(j)

  plot_data[[j]] <- plotdf
}

plot_data <- do.call(what = rbind,args = plot_data)


# --------------------------------------------------------------------------------
#   plot
# --------------------------------------------------------------------------------

plotbreaks <- c(0, 1000, 10000, 100000, 1000000)

# plot <- ggplot(plot_data, aes(y = exp_cases, x = statename_f)) +
#   geom_boxplot(aes(fill=as.factor(scenario),ymin=lb,ymax=ub),
#                outlier.stroke = 0.01, lwd = 0.2, outlier.alpha = 0.25,outlier.size = 0.5) +
#   scale_y_log10(breaks = plotbreaks,
#                 labels = format(plotbreaks, scientific = F, big.mark = ",")) +
#   ylab("Distribution of estimated COVID-19 infections (Sensitivity Analysis)") +
#   xlab("") +
#   coord_flip() +
#   theme_bw() +
#   theme(legend.position="none")
# 
# ggsave(plot, filename = paste0(plot_path, "fig-sensitivity.png"),
#        width = 10, height=8)


# 2 panel version
panel1 <- c("PR","NY","NJ","CA","MI","TX","IL","PA","GA","FL","MA","OH","VI","IN","CO","MD","CT","LA","NC","AZ","WA","MO","SC","AL","TN","WI")
plot_data$panel1 <- plot_data$state %in% panel1

plot_panel1 <- ggplot(plot_data[plot_data$panel1 == T,], aes(y = exp_cases, x = statename_f)) +
  geom_boxplot(aes(fill=as.factor(scenario),ymin=lb,ymax=ub),
               outlier.stroke = 0.01, lwd = 0.2, outlier.alpha = 0.25,outlier.size = 0.5) +
  scale_y_log10(breaks = plotbreaks,
                labels = format(plotbreaks, scientific = F, big.mark = ",")) +
  ylab("Distribution of estimated COVID-19 infections (Sensitivity Analysis)") +
  xlab("") +
  coord_flip() +
  theme_bw() +
  theme(legend.position="none")

plot_panel2 <- ggplot(plot_data[plot_data$panel1 == F,], aes(y = exp_cases, x = statename_f)) +
  geom_boxplot(aes(fill=as.factor(scenario),ymin=lb,ymax=ub),
               outlier.stroke = 0.01, lwd = 0.2, outlier.alpha = 0.25,outlier.size = 0.5) +
  scale_y_log10(breaks = plotbreaks,
                labels = format(plotbreaks, scientific = F, big.mark = ",")) +
  # ylab("Distribution of estimated COVID-19 infections (Sensitivity Analysis)") +
  xlab("") +
  coord_flip() +
  theme_bw() +
  theme(legend.position="none",axis.title.x = element_blank())

# ggsave(plot_panel1, filename = paste0(plot_path, "fig-sensitivity-1.png"),
#        width = 10, height=8)
# 
# ggsave(plot_panel2, filename = paste0(plot_path, "fig-sensitivity-2.png"),
#        width = 10, height=8)

plot_2panel = grid.arrange(plot_panel1,plot_panel2, ncol = 2)

ggsave(plot_2panel, filename = paste0(plot_path, "fig-sensitivity-2panel.png"),
       width=12, height=7)
