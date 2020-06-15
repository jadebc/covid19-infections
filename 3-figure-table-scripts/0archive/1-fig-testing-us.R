#######################################
# COVID-19 Cases USA State

# Figure of testing rates - US
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

#---------------------------------------
# Observed case counts
#---------------------------------------
# Read in covid US and state data 
covid_usa_state <- load_state_data()

covid_us = covid_usa_state %>%
  filter(date >= as.Date("2020-03-07") & date <= as.Date("2020-04-18")) %>%
  group_by(date) %>%
  summarise(total = sum(total),
            population = sum(population)) %>%
  mutate(testrate = total/ population * 1000) 

testplot = ggplot(covid_us, aes(x = date, y = testrate)) + 
  geom_line() +
  ylab("Population tested per 1,000") +
  xlab("Date") +
  scale_y_continuous(breaks = seq(0,30,5), labels = seq(0,30,5)) +
  scale_x_date(date_breaks = "weeks", date_labels = "%m-%d") + 
  theme_bw()  +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=6))
testplot

ggsave(testplot, filename = paste0(plot_path, "fig-testrates-state.png"),
       width = 10, height = 5)

