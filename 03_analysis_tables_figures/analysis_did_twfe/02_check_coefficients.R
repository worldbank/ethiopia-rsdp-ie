# DiD & TWFE: Check Coefficient Values

# Load data --------------------------------------------------------------------
did_df <- file.path(panel_rsdp_imp_dir,
                    "all_units",
                    "results_datasets",
                    "individual_datasets") %>%
  list.files(full.names = T,
             pattern = "*.Rds") %>%
  str_subset("dynamic_did_attgt") %>%
  map_df(readRDS) %>%
  mutate(est_type = "did") %>%
  dplyr::rename(years_since_improved = time,
                b = att) %>%
  mutate(p025 = b - se * critical_value_95p,
         p975 = b + se * critical_value_95p) 

tw_df <- file.path(panel_rsdp_imp_dir,
                   "all_units",
                   "results_datasets",
                   "individual_datasets") %>%
  list.files(full.names = T,
             pattern = "*.Rds") %>%
  str_subset("twowayFE") %>%
  map_df(readRDS) %>%
  mutate(est_type = "tw") %>%
  mutate(controls = case_when(
    controls == "" ~ "none",
    TRUE ~ controls
  ))

# Check coefficients -----------------------------------------------------------
did_df$dep_var %>% unique()

result_i <- did_df %>%
  dplyr::filter(dataset %in% "kebele",
                addis_distance %in% "All",
                controls %in% "none",
                dep_var %in% "dmspols_harmon_ihs",
                ntl_num_groups %in% 4,
                indep_var %in% "year_improvedroad") %>%
  dplyr::filter(years_since_improved > 0 & years_since_improved <= 10) %>%
  group_by(ntl_group) %>%
  dplyr::summarise(b = max(b))

result_i

result_i$indep_var %>% table()





