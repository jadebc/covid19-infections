#######################################
# COVID-19 estimated infections 
# correcting for incomplete testing and 
# imperfect test accuracy

# Percent of difference in infections 
# due to undertesting 
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(viridis)
library(grid)

# results with perfect sens / spec
covid_undertesting = readRDS(paste0(results_path, "covid_usa_state_adjusted_undertesting.RDS"))

covid_undertesting = covid_undertesting %>%
  mutate(statename = fct_reorder(statename, percent_und)) %>%
  mutate(percent_und = percent_und *100,
         percent_und_lb = percent_und_lb * 100,
         percent_und_ub = percent_und_ub * 100)

plot = ggplot(covid_undertesting, aes(x = statename, 
                                         y = percent_und)) +
  geom_point()  +
  geom_linerange(aes(ymin = percent_und_lb,
                     ymax = percent_und_ub))  +
  scale_y_continuous(limits = c(40,103),
                     labels = seq(40,100, 10),
                     breaks = seq(40,100,10)) +
  coord_flip() +
  theme_bw() +
  xlab("") + ylab("Percent of undetected cases due to incomplete testing")

ggsave(plot, filename = paste0(plot_path, "fig-percent-undertesting-state.png"),
       width = 5, height = 7)



