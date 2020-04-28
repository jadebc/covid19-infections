#######################################
# COVID-19 estimated infections 
# correcting for incomplete testing and 
# imperfect test accuracy

# Table of data quality 
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

covid_raw <- get_states_current()

# load state name data
state_abbrev <- read_csv(state_abbrev_path)
state_abbrev = state_abbrev %>% 
  rename(state=Abbreviation,
         statename = State)

covid = left_join(covid_raw, state_abbrev, by = "state") %>%
  dplyr::select(statename, grade) %>%
  filter(!is.na(statename))

grade_A = paste(covid$statename[covid$grade=="A"], collapse = ", ")
grade_B = paste(covid$statename[covid$grade=="B"], collapse = ", ")
grade_C = paste(covid$statename[covid$grade=="C"], collapse = ", ")

table = data.frame(
  grade = c("A", "B", "C"),
  state = c(grade_A, grade_B, grade_C)
)

write.csv(table, paste0(results_path, "table-data-quality.csv"))
