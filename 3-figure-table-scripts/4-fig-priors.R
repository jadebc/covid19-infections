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
# table - static priors
#--------------------------------------
min = apply(static_priors, 2, min)
med = apply(static_priors, 2, median)
max = apply(static_priors, 2, max)

table = bind_rows(min, med, max) %>% t() %>% as.data.frame()

colnames(table) = c("min","med","max")

table = table %>% mutate(
  min = sprintf("%0.03f", min),
  med = sprintf("%0.03f", med),
  max = sprintf("%0.03f", max)
) %>%
  mutate(label = rownames(table)) %>%
  mutate(label = case_when(
    label == "P_S_tested" ~ "P(S|tested)",
    label == "P_S_untested" ~ "P(S|untested)",
    label == "Z_S" ~ "alpha",
    label == "Z_A" ~ "beta",
    label == "P_testpos_S" ~ "P(test + |S)",
    label == "P_testpos_A" ~ "P(test + |A)",
    label == "P_S_testpos" ~ "P(S | test +)",
    label == "P_A_testpos" ~ "P(A | test +)",
    label == "dist_Se" ~ "Sensitivity",
    label == "dist_Sp" ~ "Specificity"
  )) %>% filter(!is.na(label)) 


write.csv(table, file = paste0(results_path, "input_dist_range.csv"))



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

