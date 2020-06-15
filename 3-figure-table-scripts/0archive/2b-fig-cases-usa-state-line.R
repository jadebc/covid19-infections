#######################################
# COVID-19 Cases USA State

# Line plot of COVID-19 cases by USA State
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

covid_usa_state_adjusted = readRDS(paste0(results_path, "covid_usa_state_adjusted.RDS"))
covid_usa_state_subset = readRDS(paste0(results_path, "covid_usa_state_subset.RDS"))

# read in public health response data 
# load most recent file saved in directory
tmpshot <- fileSnapshot(state_responses_pre_path)
latest_file = rownames(tmpshot$info[which.max(tmpshot$info$mtime),])
int_raw = read.csv(paste0(state_responses_pre_path, latest_file))

int = int_raw %>%
  filter(level == "state") %>%
  dplyr::select(-c(county, level)) %>%
  rename(statename = state) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) 

covid_usa_state_adjusted = covid_usa_state_adjusted  %>%
  filter(date <= as.Date("2020-03-31")) %>%
  left_join(int, by = c("statename","date")) %>%
  mutate(order_yn = factor(ifelse(is.na(order), "None", "Shelter in place")))

# us total
covid_usa = covid_usa_state_adjusted %>%
  group_by(date) %>%
  filter(date <= as.Date("2020-03-31")) %>%
  summarise(positive = sum(positive),
            estimated_cases = sum(estimated_cases),
            population = sum(population)) %>%
  mutate(state = "US",
         statename = "United States") %>%
  dplyr::select(date, state, statename, 
                positive, estimated_cases, population)

covid_state = covid_usa_state_adjusted %>%
  dplyr::select(date, state, statename, 
                positive, estimated_cases, population) %>%
  bind_rows(covid_usa)

############################################################
# Observed data
############################################################
topN = 10

#-----------------------------------------
# preprocess observed data
#-----------------------------------------
# identify states in top ten on last day
# of current data 
obs_level = covid_state %>% 
  filter(state!="US") %>% 
  filter(date == as.Date("2020-03-31")) %>%
  group_by(statename) %>% 
  mutate(pos_pop = positive/population*1000) %>%
  arrange(pos_pop) %>% 
  dplyr::select(statename, pos_pop) %>%
  pull(statename) %>% rev()

obs_level = obs_level[1:topN]
obs_level_all = c(obs_level[1:topN], "Other", "United States")

covid_state = covid_state %>%
  mutate(key_state = ifelse(statename %in% obs_level,1,0),
         key_state_name = ifelse(statename %in% obs_level, statename, "Other")) %>%
  mutate(
    key_state = as.factor(ifelse(statename=="United States", 1, key_state)),
    key_state_name = ifelse(statename == "United States", "United States",
                            key_state_name)) %>%
  mutate(key_state_name = factor(key_state_name, levels = obs_level_all)) %>%
  mutate(obs_label = paste0(statename, "\n", format(date, "%b %d, %Y"), "\n",
                            sprintf("%0.0f", positive/population*1000), " cases per 1,000", "\n", 
                            format(positive, big.mark=",", digits=0, scientific=F, trim = TRUE), " total cases")) %>%
  mutate(exp_label = paste0(statename, "\n", format(date, "%b %d, %Y"), "\n",
                            sprintf("%0.0f", estimated_cases/population*1000), " cases per 1,000", "\n", 
                            format(estimated_cases, big.mark=",", digits=0, scientific=F, trim = TRUE), " total cases"))

covid_state = covid_state %>% mutate(
  pos_per_pop = positive/population*1000
)

#-----------------------------------------
# plot observed data
#-----------------------------------------
# define color palette
# obs_pal = viridis(n = topN, begin = 0, end = 0.9, option = "C")
# obs_pal = brewer.pal(n = topN, name = "Spectral")
obs_pal = gg_color_hue(n = topN)
obs_pal = c(obs_pal, "#B8B8B8", 'black')

# observed cases
key_obs = ggplot(covid_state, 
                 aes(x = date, y = pos_per_pop, 
                     group = statename, text = obs_label)) +
  
  geom_line(aes(col = key_state_name, size = key_state, alpha = key_state)) +
  
  scale_color_manual(values = obs_pal) + 
  scale_alpha_manual(values = c(0.15, 1), guide = F) + 
  scale_size_manual(values = c(0.5,1), guide = F) + 
  
  theme_minimal() + 
  
  ggtitle("Observed COVID-19 cases") +
  xlab("Date") + 
  ylab("Cumulative confirmed COVID-19 cases per 1,000") +
  labs(color = "State") +
  theme(legend.position = "none")

key_obs_int <- ggplotly(key_obs, tooltip="text") %>% 
  style(hoverlabel = list(align = "left")) %>% 
  layout(annotations = list(
    text = "Observed COVID-19 cases",
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.2,
    y = 1,
    showarrow = FALSE)) %>% 
  config(displayModeBar = F)

key_obs_int 

############################################################
# Expected data
############################################################

#-----------------------------------------
# plot expected data
#-----------------------------------------
# estimated real cases
key_exp = ggplot(covid_state, 
                 aes(x = date, y = estimated_cases/population*1000, 
                     group = statename, text = exp_label)) +
  
  geom_line(aes(col = key_state_name, size = key_state, alpha = key_state)) +
  
  scale_color_manual(values = obs_pal) + 
  scale_alpha_manual(values = c(0.5, 1), guide = F) + 
  scale_size_manual(values = c(0.5,1), guide = F) +   
  
  theme_minimal() + 
  ggtitle("Estimated burden of COVID-19 infection") +
  xlab("Date") + 
  ylab("Cumulative estimated COVID-19 infections per 1,000") +
  labs(color = "State")

key_exp_int <- ggplotly(key_exp, tooltip="text") %>% 
  style(hoverlabel = list(align = "left")) %>% 
  layout(annotations = list(
    text = "Estimated burden of COVID-19 infection",
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.3,
    y = 1,
    showarrow = FALSE)) %>% 
  config(displayModeBar = F)

# Reformat legend
for (i in 1:length(key_exp_int$x$data)){
  if (!is.null(key_exp_int$x$data[[i]]$name)){
    key_exp_int$x$data[[i]]$name =  gsub("\\(","",str_split(key_exp_int$x$data[[i]]$name,",")[[1]][1])
  }
}

key_exp_int

#-----------------------------------------
# combine and save plots
#-----------------------------------------
plot_key = grid.arrange(key_obs, key_exp,
                        ncol = 2, widths = c(0.8, 1))
ggsave(paste0(plot_path, "fig-usa-state-cases-key.png"), plot_key, width = 12, height = 6)

### save interactive plot 
plot_key_int = subplot(key_obs_int, key_exp_int, titleX = TRUE, titleY = TRUE, margin = 0.05) %>% layout(title = NA)

for (i in 1:12){
  plot_key_int$x$data[[i]]$showlegend <- FALSE
}

saveWidget(plot_key_int, paste0(plot_path, "fig-usa-state-cases-key.html"), selfcontained = T)
