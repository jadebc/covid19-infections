#######################################
# COVID-19 estimated infections 
# correcting for incomplete testing and 
# imperfect test accuracy

# Figure of testing rates by state 
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

#---------------------------------------
# Observed case counts
#---------------------------------------
# read in public health response data 
# load most recent file saved in directory
testing_protocols = read.csv(paste0(results_path, "testing_by_state.csv"))
state_abb_key = read.csv(state_abbrev_path) %>% select(statename = State, state = Abbreviation)

# exclude Puerto Rico 
state_abb_key = state_abb_key[state_abb_key$state != 'PR', ]


testing_protocols = testing_protocols %>% select(-X, state = State) %>% 
  mutate(Date.start = as.Date(Date.start),
         Date.end = as.Date(Date.end)) %>% 
  left_join(state_abb_key, by = "state") %>% 
  mutate(`Testing Protocol` = case_when(test_cat == "hosp" ~ "Hospitalized Patients",
                                        test_cat == "default" ~ "Default Protocol",
                                        test_cat == "all" ~ "All Symptomatic and Asymtomatic Patients",
                                        test_cat == "hosp_sev_mod_mild" ~ "All Inpatient and Outpatient"))

# Read in covid US and state data, excluding Puerto Rico 
covid_usa_state <- load_state_data(min_date = "2020-02-28", max_date = "2020-04-18")
covid_usa_state = covid_usa_state[covid_usa_state$state != 'PR', ]

maxdate = max(covid_usa_state$date)

region1 = c("Alaska", "Washington", "Oregon")
region2 = c("Idaho", "Montana", "Wyoming")
region3 = c("Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", 
            "North Dakota", "South Dakota")
region4 = c("Illinois", "Indiana", "Michigan","Ohio", "Wisconsin")
region5 = c("Maine","New Hampshire","Vermont","Massachusetts",
            "Rhode Island","Connecticut","New Jersey","New York",
            "Pennsylvania")

region6 = c("California", "Nevada", "Hawaii")
region7 = c("Utah", "Arizona","Colorado","New Mexico")
region8 = c("Texas",  "Oklahoma", "Arkansas", "Louisiana")
region9 = c("Kentucky", "Tennessee", "Mississippi","Alabama")
region10 = c("North Carolina","South Carolina","Georgia","Florida",
             "Delaware","Maryland","Virginia","West Virginia", 
             "District of Columbia")

covid_state = covid_usa_state %>%
  dplyr::select(date, state, statename, positive, total, population) %>%
  mutate(testrate = total/ population * 1000) %>%
  mutate(region = case_when(
    statename %in% region1 ~ "Northwest",
    statename %in% region2 ~ "Mountain - North",
    statename %in% region3 ~ "Midwest - West",
    statename %in% region4 ~ "Midwest - East",
    statename %in% region5 ~ "Northeast",
    
    statename %in% region6 ~ "West",
    statename %in% region7 ~ "Mountain - South",
    statename %in% region8 ~ "South - West",
    statename %in% region9 ~ "South - East Central",
    statename %in% region10 ~ "South - Atlantic"
  )) %>%
  # reorder region spatially 
  mutate(region = factor(region, levels = c(
    "Northwest", "Mountain - North","Midwest - West", "Midwest - East","Northeast",
    "West","Mountain - South", "South - West","South - East Central","South - Atlantic"
  ))) 

testrate_quantiles = covid_state %>%
  filter(date == as.Date(maxdate)) %>%
  mutate(testrate_cat = cut(testrate, 
                            quantile(testrate, prob = c(0, 0.2, 0.4, 0.6, 0.8, 1)),
                            labels = paste0("Quintile ", c(1:5)), include.lowest = T, right = T)) %>%
  dplyr::select(state, testrate_cat) %>%
  mutate(testrate_cat_rev = fct_rev(testrate_cat))

covid_state = covid_state %>%
  left_join(testrate_quantiles, by = "state")

ordered_states_testrate = covid_state %>% filter(date == max(date)) %>% arrange(desc(testrate)) %>% pull(statename)
covid_state = covid_state %>% mutate(statename = factor(statename, levels = ordered_states_testrate))

# define y position for text
state_pos = covid_state %>%
  filter(date == as.Date(maxdate)) %>%
  group_by(region) %>%
  arrange(region, testrate_cat_rev, statename) %>%
  mutate(state_order = seq_along(statename)) %>%
  mutate(y_text = 35 - state_order*2.2) %>%
  ungroup() %>%
  dplyr::select(statename, y_text)

covid_state = covid_state %>% 
  left_join(state_pos, by = "statename")

region_labels = covid_state %>%
  mutate(region_states = case_when(
    statename %in% region1 ~ paste(region1, collapse = "\n"),
    statename %in% region2 ~ paste(region2, collapse = "\n"),
    statename %in% region3 ~ paste(region3, collapse = "\n"),
    statename %in% region4 ~ paste(region4, collapse = "\n"),
    statename %in% region5 ~ paste(region5, collapse = "\n"),
    statename %in% region6 ~ paste(region6, collapse = "\n"),
    statename %in% region7 ~ paste(region7, collapse = "\n"),
    statename %in% region8 ~ paste(region8, collapse = "\n"),
    statename %in% region9 ~ paste(region9, collapse = "\n"),
    statename %in% region10 ~ paste(region10, collapse = "\n")
  )) %>%
  dplyr::select(region, region_states) %>%
  distinct()

covid_state = covid_state %>% 
  mutate(mylabel = paste0(statename, "\n",
                          date, "\n",
                          sprintf("%0.1f", testrate), " per 1,000"))



palette = viridis(n = 5, option = "C", begin=0, end=0.9, direction = -1)

testplot = ggplot(covid_state, aes(x = date, y = testrate, group = state,  
                                   text = mylabel)) + 
  geom_line(aes(col = testrate_cat)) +
  ylab("Population tested per 1,000") +
  xlab("Date") +
  scale_y_continuous(breaks = seq(0,30,5), labels = seq(0,30,5)) +
  scale_x_date(date_breaks = "10 days", date_labels = "%b %d") + 
  scale_color_manual("Testing per 1,000\nby April 18, 2020", values = palette) +
  facet_wrap(~region, ncol = 5)+
  geom_text(aes(label=statename, col = testrate_cat, y = y_text), 
            x = as.Date("2020-02-28"), 
            hjust=0, # left align
            vjust = 1,
            size=3,
            inherit.aes = FALSE, show.legend = FALSE) +
  theme_bw()  +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=5))
testplot

ggsave(testplot, filename = paste0(plot_path, "fig-testrates-state.png"),
       width = 10, height = 5)


testplot_with_policy_change = ggplot(covid_state, aes(x = date, y = testrate, group = state,  
                                                      text = mylabel)) + 
  geom_line() +
  ylab("Population tested per 1,000") +
  xlab("Date") +
  scale_y_continuous(breaks = seq(0,30,5), labels = seq(0,30,5)) +
  scale_x_date(date_breaks = "10 days", date_labels = "%b %d") + 
  scale_color_manual(values = palette) +
  facet_wrap(~statename, ncol = 5)+
  geom_vline(data = testing_protocols %>% filter(state %in% unique(covid_state$state)), 
             aes(xintercept = Date.start, color = `Testing Protocol`), linetype = 5) +
  theme_bw()  +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=5))
testplot_with_policy_change

ggsave(testplot_with_policy_change, filename = paste0(plot_path, "fig-testrates-state-with-protocol.png"),
       width = 8, height = 11)


## Interactive plot 
testplot_int = ggplotly(testplot + scale_color_manual("Testing per 1,000\nby April 18, 2020", values = palette), tooltip="text") %>% 
  style(hoverlabel = list(align = "left")) %>% 
  layout(barmode = 'overlay')%>% 
  config(displayModeBar = F)

saveWidget(testplot_int, paste0(plot_path, "fig-testrates-state.html"), selfcontained = T)


