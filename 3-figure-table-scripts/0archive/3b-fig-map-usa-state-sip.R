#######################################
# COVID-19 Cases USA State

# Plot of COVID-19 cases by USA State
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
  left_join(int, by = c("statename","date")) %>%
  mutate(order_yn = factor(ifelse(is.na(order), "None", "Shelter in place")))

############################################################
# Expected data
############################################################
#-----------------------------------------
# preprocess expected data
#-----------------------------------------
topN = 10

# identify states in top ten on last day
# of current data 
exp_level = covid_usa_state_adjusted %>% 
  filter(date == max(covid_usa_state_subset$date)) %>%
  group_by(statename) %>% 
  mutate(pos_pop = estimated_cases/population*1000) %>%
  arrange(pos_pop) %>% 
  dplyr::select(statename, pos_pop) %>%
  pull(statename) %>% rev()

exp_level = exp_level[1:topN]
exp_level_all = c(exp_level[1:topN], "Other")

covid_usa_state_adjusted = covid_usa_state_adjusted %>%
  mutate(key_state = as.factor(ifelse(statename %in% exp_level,1,0)),
         key_state_name = ifelse(statename %in% exp_level, statename, "Other")) %>%
  mutate(key_state_name = factor(key_state_name, levels = exp_level_all)) %>%
  mutate(label = paste0(statename, "\n", date, "\n",
                        sprintf("%0.0f", estimated_cases/population*1000), " per 1,000",
                        "\n", format(estimated_cases, big.mark=",", digits=0, scientific=F), " total"))

#-----------------------------------------
# plot expected data
#-----------------------------------------
# define color palette
# exp_pal = viridis(n = topN, begin = 0, end = 0.9, option = "C")
# exp_pal = brewer.pal(n = topN, name = "Spectral")
# exp_pal = c(exp_pal, "#B8B8B8")
exp_pal = gg_color_hue(n = topN)
exp_pal = c(exp_pal, "#B8B8B8")

# point for shelter in place 

ggplot(covid_usa_state_adjusted, 
       aes(x = date, y = estimated_cases/population*1000, 
           group = statename )) +
  
  geom_line(aes(col = key_state_name, size = key_state, alpha = key_state)) +
  geom_point(data = covid_usa_state_adjusted %>% filter(order_yn=="Shelter in place"),
             aes(size = key_state, alpha = key_state, shape = order_yn), col="black") +
  
  scale_color_manual(values = exp_pal) + 
  scale_alpha_manual(values = c(0.5, 1), guide = F) + 
  scale_size_manual(values = c(0.5,1), guide = F) +   
  scale_shape_manual(values = c(16,21), guide = F) +
  
  theme_minimal() + 
  ggtitle("Estimated burden of COVID-19 infection") +
  xlab("Date") + 
  ylab("Cumulative COVID-19 infections per 1,000") +
  labs(color = "State")



