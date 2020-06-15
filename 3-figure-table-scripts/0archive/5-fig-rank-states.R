#######################################
# COVID-19 Cases USA State

# Rank obs. vs. exp COVID-19 cases by USA State
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

covid_usa_state_adjusted = readRDS(paste0(results_path, "covid_usa_state_adjusted.RDS"))
# covid_usa_state_subset = readRDS(paste0(results_path, "covid_usa_state_subset.RDS"))

covid_usa_state_adjusted = covid_usa_state_adjusted %>% 
  filter(date==as.Date("2020-03-31"))

obs_rank = covid_usa_state_adjusted %>% 
  mutate(obs_perpop = positive/population * 1000) %>%
  dplyr::select(statename, obs_perpop) %>%
  arrange(obs_perpop) %>%
  mutate(label= "Confirmed cases") 

obs_rank$rank = rev(seq(1,nrow(obs_rank),1))

exp_rank = covid_usa_state_adjusted %>% 
  mutate(exp_perpop = estimated_cases/population * 1000) %>%
  dplyr::select(statename, exp_perpop) %>%
  arrange(exp_perpop) %>%
  mutate(label = "Estimated infections")

exp_rank$rank = rev(seq(1,nrow(exp_rank),1))

rank_diff_df = obs_rank %>% rename(obs_rank = rank) %>% 
  dplyr::select(statename, obs_rank, obs_perpop) %>%
  left_join(exp_rank %>% rename(exp_rank = rank), by = "statename") %>%
  mutate(rank_diff = obs_rank - exp_rank) %>%
  mutate(rank_diff_cat = case_when(
    rank_diff < 0 ~ "Lower", 
    abs(rank_diff) == 0 ~ "No change",
    rank_diff > 0 ~ "Higher")) %>%
  dplyr::select(-label)

rank_data = bind_rows(obs_rank, exp_rank) %>%
  dplyr::select(-c(obs_perpop, exp_perpop)) %>%
  mutate(label = as.factor(label)) %>%
  mutate(hjust = ifelse(label == "Confirmed cases", "right", "left"),
         nudge = ifelse(label == "Confirmed cases", -0.03, 0.03)) %>%
  left_join(rank_diff_df, by = "statename") 

  # mutate(int_label = paste0(statename, "\n", "Observed: ", 
  #                           format(round(obs_perpop), big.mark=","), " per 1,000"     ,
  #                           "\nObserved rank: ", format(round(obs_rank)),
  #                           "\nExpected: ", format(round(exp_perpop), big.mark=","), " per 1,000",
  #                           "\nExpected rank: ", format(round(exp_rank))))

#-----------------------------------------
# static plot
#-----------------------------------------
rank_plot = ggplot(rank_data, aes(y = rank, x = label, group = statename)) + 
  geom_point(size = 2, alpha = 0.8) +
  geom_line(data = rank_data %>% filter(rank_diff_cat=="Higher"), aes(col = rank_diff)) +
  geom_text(size = 4, aes(label = statename, hjust = rank_data$hjust, alpha = rank_diff_cat), 
            nudge_x = rank_data$nudge) +
  scale_y_reverse(min = 1, breaks = c(1,seq(5,50,5)), labels = c(1,seq(5,50,5))) +
  xlab("") + ylab("") +
  scale_color_gradient("", high = "#430259", low = "#E7A1FF", guide=F) +
  scale_x_discrete(position = "top") +

  scale_alpha_manual("", values = c(1,0.5,0.5)) +
  theme_minimal() +
  theme(legend.position = 'none',
        panel.grid.major = element_blank() ,
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        axis.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm")) 

ggsave(rank_plot, filename = paste0(plot_path, "fig-rank-state-obs-v-exp.png"),
       width=6, height=10)

#-----------------------------------------
# interactive plot
#-----------------------------------------
# 
# rank_plot_int <- ggplotly(rank_plot, tooltip="text")
# %>% 
#   style(hoverlabel = list(align = "left")) %>% 
#   layout(annotations = list(
#     text = "Observed COVID-19 cases",
#     xref = "paper",
#     yref = "paper",
#     yanchor = "bottom",
#     xanchor = "center",
#     align = "center",
#     x = 0.2,
#     y = 1,
#     showarrow = FALSE))
# 
# key_obs_int 