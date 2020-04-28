#######################################
# COVID-19 estimated infections 
# correcting for incomplete testing and 
# imperfect test accuracy

# Figure showing P(test+|tested)
# among those tested by date and state
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

covid_usa_state <- load_state_data(min_date = "2020-02-28", max_date = "2020-04-18")

covid_usa_state = covid_usa_state %>% 
  filter(date <= as.Date("2020-04-18"))

covid_usa_state = covid_usa_state %>% mutate(
  posrate = positive/total *100,
  testrate= total/population *100
) 

plot_all = ggplot(covid_usa_state, aes(x = date, y = posrate, group = statename)) +
  geom_line(aes(col = statename)) +
  scale_y_continuous(limits = c(0,100)) + 
  scale_x_date(date_breaks = "5 days", date_labels = "%b %d",
               limits = as.Date(c("2020-02-28", "2020-04-18"))) +
  ylab("P(test+ | tested)") +
  xlab("Date")+
  theme_bw()+
  theme(legend.position = "none")

plot_later = ggplot(covid_usa_state %>% filter(testrate>0.6), aes(x = date, y = posrate, group = statename, text=statename)) +
  geom_line(aes(col = statename)) +
  scale_y_continuous(limits = c(0,100)) + 
  scale_x_date(date_breaks = "5 days", date_labels = "%b %d",
               limits = as.Date(c("2020-02-28", "2020-04-18"))) + 
  ylab("P(test+ | tested)") +
  xlab("Date")+
  theme_bw()+
  theme(legend.position = "none")

ggsave(plot_all, filename = paste0(plot_path, "fig-testpos-state.png"),
       width = 7, height = 4)

ggsave(plot_later, filename = paste0(plot_path, "fig-testpos-later-state.png"),
       width = 7, height = 4)




