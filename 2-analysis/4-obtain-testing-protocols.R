####################################################################
# Find testing protocols for each state
####################################################################

# Load data from Google Sheets
state_testing_data_url = 'docs.google.com/spreadsheets/d/1jqsPfH34oyzPsQEqpqFFOYd54MZ1MUvgqBXZndaEv8s#gid=290834725'
state_testing_data_csv = gsheet2text(state_testing_data_url, format = "csv")
state_testing = read.csv(text=state_testing_data_csv, stringsAsFactors=FALSE)

df = state_testing
  
df_testing_cols = df %>% dplyr::select(
  Hospitalized.patients,
  Outpatient.with.severe.symptoms,
  Outpatient.with.moderate.symptoms,
  Outpatient.with.mild.symptoms,
  Asymptomatic
)

df_testing_cols = df_testing_cols %>% mutate_all(~replace(., . == "?", "No")) %>%
  mutate_all(~replace(., . == "", "No"))


list_num_tested_groups = c()
for (i in 1:nrow(df)) {
  print(i)
  row = df_testing_cols[i, ]
  num_tested_groups = sum(row == "Yes")
  if(num_tested_groups != sum(row[1:num_tested_groups] == "Yes")){
    num_tested_groups = max(which(row == "Yes"))
  }
  list_num_tested_groups = append(list_num_tested_groups, num_tested_groups)
}

state_dist_types = df %>%
  mutate(
    num_groups_tested = list_num_tested_groups,
    test_cat = case_when(
      num_groups_tested == 0 ~ "default",
      num_groups_tested == 1 ~ "hosp",
      num_groups_tested == 2 ~ "hosp_sev",
      num_groups_tested == 3 ~ "hosp_sev_mod",
      num_groups_tested == 4 ~ "hosp_sev_mod_mild",
      num_groups_tested == 5 ~ "all"
    )) %>% 
  select(State, Date.start, Date.end, test_cat)
  

state_dist_types = state_dist_types %>% mutate(Date.end = ifelse(Date.end == "" | Date.end == "?", format(Sys.Date(), "%Y-%m-%d"), Date.end)) %>% 
  mutate(Date.start = ifelse(Date.start == "" | Date.start == "?", "2020-03-01", Date.start))


write.csv(state_dist_types, paste0(results_path, "testing_by_state.csv"))
