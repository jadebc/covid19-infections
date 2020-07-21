#######################################
# COVID-19 estimated infections 
# correcting for incomplete testing and 
# imperfect test accuracy

# Bar plot of COVID-19 cases vs. 
# estimated number of infections by USA State
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(viridis)
library(grid)

covid_usa_state_adjusted = readRDS(paste0(results_path, "covid_usa_state_adjusted.RDS"))

gnbu_colors = brewer.pal(n=6,"GnBu")[2:6]
orrd_colors = brewer.pal(n=6,"OrRd")[2:6]

#######################################
# compare observed vs estimated test positive rate
# by state (Panel B)
#######################################
# calculate % diff in obs vs. exp
bar_data_perdiff = covid_usa_state_adjusted %>%
  mutate(factor_diff = estimated_cases / positive,
         factor_diff_lb = estimated_cases_lb / positive,
         factor_diff_ub = estimated_cases_ub / positive) %>%
  mutate(statename_f = fct_reorder(statename, factor_diff, max)) %>%
  mutate(label = paste0(
    statename, 
    "\nConfirmed cases: ", format(round(positive, 0), scientific=F, big.mark=","),
    "\nEstimated cases: ", format(round(estimated_cases, 0), scientific=F, big.mark=","),
    "\nEstimated:confirmed ", format(round(factor_diff,0), scientific=F, big.mark=","),
    " (",format(round(factor_diff_lb,0), scientific=F, big.mark=","), ", ",
    format(round(factor_diff_ub,0), scientific=F, big.mark=","),")"))

ratio_quantiles <- quantile(bar_data_perdiff$factor_diff, c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm=TRUE)

# modify interval labels
label_interval <- function(breaks) {
  paste0(breaks[1:length(breaks) - 1], " - ", breaks[2:length(breaks)])
}

bar_data_perdiff = bar_data_perdiff %>%
  mutate(factor_diff_cat = cut(factor_diff, 
                               ratio_quantiles,
                               include.lowest=TRUE,
                               labels = label_interval(round(ratio_quantiles, 1))
  ))

perdiff_solo_plot = ggplot(bar_data_perdiff, aes(x = statename_f, y = factor_diff, text = label))+
  geom_bar(aes(fill = factor_diff_cat), stat="identity") +
  geom_linerange(aes(ymin = factor_diff_lb, ymax = factor_diff_ub)) + 
  xlab("") +
  scale_fill_manual(values = gnbu_colors) +
  scale_y_continuous(labels = seq(0,80,5), breaks = seq(0,80,5)) +
  ylab("Ratio of estimated infections vs. confirmed cases") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text = element_text(size=10.5)) +
  theme(legend.position = "none")

perdiff_plot = perdiff_solo_plot +  ggtitle("B) Ratio of estimated infections vs. confirmed cases")

perdiff_plot_int <- ggplotly(perdiff_plot, tooltip="text") %>% 
  style(hoverlabel = list(align = "left")) %>% 
  layout(annotations = list(
    text = "B) Ratio of estimated infections vs. confirmed cases",
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "left",
    x = 0.3,
    y = 1,
    showarrow = FALSE)) %>% 
  config(displayModeBar = F)


perdiff_solo_plot_int <- ggplotly(perdiff_solo_plot, tooltip="text") %>% 
  style(hoverlabel = list(align = "left")) %>% 
  layout(annotations = list(
    text = "Ratio of estimated infections vs. confirmed cases",
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "left",
    x = 0.3,
    y = 1,
    showarrow = FALSE))%>% 
  config(displayModeBar = F)

perdiff_plot_int

############################################################
# Bar chart of estimated counts (Panel A)
############################################################

bar_data =  bar_data_perdiff %>%
  mutate(exp = estimated_cases,
         exp_perpop = estimated_cases/population*1000,
         exp_perpop_lb = estimated_cases_lb / population*1000,
         exp_perpop_ub = estimated_cases_ub / population*1000) %>%
  dplyr::select(statename, statename_f,  exp_perpop,
                exp_perpop_lb, exp_perpop_ub) %>%
  melt(id.vars = c("statename", "statename_f", "exp_perpop_lb","exp_perpop_ub")) %>%
  mutate(variable = as.character(variable)) %>%
  mutate(variable_f = as.factor("Estimated infections")) %>%
  mutate(variable_f = factor(variable_f, "Estimated infections")) %>%
  mutate(exp_perpop_lb = exp_perpop_lb, exp_perpop_ub = exp_perpop_ub)


# create label for interactive plot
bar_labels = bar_data %>% dplyr::select(statename) %>%
  left_join(bar_data %>% filter(variable == "exp_perpop") %>%
              dplyr::select(statename, value), by = "statename") %>%
  left_join(bar_data %>% filter(variable == "exp_perpop") %>% 
              dplyr::select(statename, exp_perpop_lb, exp_perpop_ub), by = "statename") %>%
  mutate(label = paste0("<b>", statename, "</b>\n",
                        "Estimated Cases: ", 
                        format(value, big.mark=",", digits=1, scientific=F, trim = TRUE), 
                        " (", format(exp_perpop_lb, big.mark=",", digits=1, scientific=F, trim = TRUE), 
                        ", ", format(exp_perpop_ub, big.mark=",", digits=1, scientific=F, trim = TRUE), ")"
                        )) %>%
  dplyr::select(statename, label)

bar_data = bar_data %>%
  left_join(bar_labels, by = "statename")


bar_data$variable_f = factor(bar_data$variable_f, "Estimated infections")


############################################################
# natural scale plot
############################################################

nat_bar_solo = ggplot(bar_data,
                      aes(x = statename_f, y = value , fill = variable_f, text = label)) +
  geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.8) +
  geom_linerange(aes(ymin = exp_perpop_lb, ymax = exp_perpop_ub),
                 position=position_dodge(width=0.8)) +

  scale_fill_manual("", values = "#858585")  +
  
  theme_minimal() +
  theme(axis.text.y = element_text(size=10.5)) +
  xlab("") +
  ylab("Cumulative estimated COVID-19 infections per 1,000") +
  theme(legend.position = "none")+
  coord_flip()+
  
  scale_y_continuous(breaks = seq(0,150,10),
                     labels = seq(0,150,10)) %>% 
  config(displayModeBar = F)

nat_bar_solo

nat_bar = nat_bar_solo + ggtitle("A) Cumulative estimated COVID-19 infections per 1,000") 


bar_data_forint = bar_data %>% 
  mutate(variable_f = factor(bar_data$variable_f, 
                             levels= c("Estimated infections")))

nat_bar_solo_forint = ggplot(bar_data_forint, 
                             aes(x = statename_f, y = value , fill = variable_f, text = label)) +
  
  geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.8) +
  geom_linerange(aes(ymin = exp_perpop_lb, ymax = exp_perpop_ub), 
                 position=position_dodge(width=0.8)) +
  scale_fill_manual("", values = c("#E5BC4D","#858585"))  + 
  
  theme_minimal() + 
  xlab("") + 
  ylab("Cumulative estimated COVID-19 infections per 1,000") +
  theme(legend.position = "none")+
  coord_flip()

nat_bar_forint = nat_bar_solo_forint +  ggtitle("A) Cumulative case count") 

nat_bar_int =  ggplotly(nat_bar_forint + scale_fill_manual("", values = c("#858585","#E5BC4D")) , tooltip="text") %>% 
  style(hoverlabel = list(align = "left")) %>% 
  layout(barmode = 'overlay',
         annotations = list(
           text = "A) Cumulative estimated COVID-19 infections per 1,000",
           xref = "paper",
           yref = "paper",
           yanchor = "bottom",
           xanchor = "center",
           align = "center",
           x = 0.2,
           y = 1,
           showarrow = FALSE))%>% 
  config(displayModeBar = F)


nat_bar_solo_int =  ggplotly(nat_bar_solo_forint + scale_fill_manual("", values = c("#858585","#E5BC4D")) , tooltip="text") %>% 
  style(hoverlabel = list(align = "left")) %>% 
  layout(barmode = 'overlay',
         annotations = list(
           text = "Cumulative estimated COVID-19 infections per 1,000",
           xref = "paper",
           yref = "paper",
           yanchor = "bottom",
           xanchor = "center",
           align = "center",
           x = 0.2,
           y = 1,
           showarrow = FALSE))%>% 
  config(displayModeBar = F)

############################################################
# combine plots
############################################################
### save separate plots
ggsave(nat_bar_solo, filename = paste0(plot_path, "fig-usa-state-cases-bar-obs.png"),
       width=8, height=7)

ggsave(perdiff_solo_plot, filename = paste0(plot_path, "fig-usa-state-cases-bar-ratio.png"),
       width=8, height=7)


### save combined plots
bar_plot = grid.arrange(nat_bar, perdiff_plot, ncol = 2)

ggsave(bar_plot, filename = paste0(plot_path, "fig-usa-state-cases-bar.png"),
       width=12, height=8)

### save separate interactive plots
saveWidget(nat_bar_solo_int, paste0(plot_path, "fig-usa-state-cases-bar-obs.html"), selfcontained = T)
saveWidget(perdiff_solo_plot_int, paste0(plot_path, "fig-usa-state-cases-bar-ratio.html"), selfcontained = T)

