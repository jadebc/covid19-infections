#######################################
# COVID-19 Cases USA State

# Plot of COVID-19 cases by USA State
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

covid_usa = readRDS(paste0(results_path, "covid_usa_adjusted.RDS"))
covid_all_usa_early = readRDS(paste0(results_path, "covid_usa_adjusted_early.RDS"))

covid_early = covid_all_usa_early %>% arrange(date) %>%
  mutate(type = "early") %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  rename(total = tested)

covid_usa = covid_usa %>% mutate(type = "main")

covid_all = bind_rows(covid_early, covid_usa) %>%
  mutate(
  obs_cases_perpop = total / population * 1000,
  exp_cases_perpop = estimated_cases / population * 1000,
  exp_lb_perpop = estimated_cases_lb / population * 1000,
  exp_ub_perpop = estimated_cases_ub / population * 1000
) 

# small overlap in data in early march, drop redundant rows
drops = which(covid_all$date >=as.Date("2020-03-04") &
                covid_all$date <=as.Date("2020-03-13") & 
                covid_all$type == "early")

covid_all = covid_all[-drops,]

# check there is no more date overlap
assert_that(all(covid_all %>% dplyr::select(date) %>%
  distinct(date) %>% duplicated()) == F)

############################################################
# Plot US total by calendar day 
# Excluding early data from CDC only because
# priors would need to be adjusted for each date of 
# change in testing protocol - if you want to see the early
# data remove filtering on type==main
############################################################
us_plot_obs = ggplot(covid_all %>% filter(type=="main"), aes(x = date, y = obs_cases_perpop)) +
  geom_line(col = "#0DD36D") + 
  scale_x_date(date_breaks = "7 days", date_labels = "%b %d") +
  ylab("Cumulative COVID-19 cases per 1,000") +
  xlab("Date") + 
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("Observed cases")

us_plot_exp = ggplot(covid_all %>% filter(type=="main"), aes(x = date, y = exp_cases_perpop)) +
  geom_line(col = "#0D8AD3") + 
  scale_x_date(date_breaks = "7 days", date_labels = "%b %d") +
  ylab("") +
  xlab("Date") + 
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("Expected cases")

us_plot = grid.arrange(us_plot_obs,us_plot_exp,  ncol = 2)

ggsave(us_plot, filename = paste0(plot_path, "fig-usa-cases.png"),
       width=7, height=4)

