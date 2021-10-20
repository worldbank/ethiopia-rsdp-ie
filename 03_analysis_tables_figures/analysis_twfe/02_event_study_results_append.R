# Analysis: Coefficient Each Year - Results

results_df <- file.path(panel_rsdp_imp_dir,
                        "all_units", "results_datasets",
                        "individual_datasets") %>%
  list.files(pattern = "*.Rds", full.names = T) %>%
  str_subset("twowayFE_") %>%
  map_df(readRDS)

saveRDS(results_df, 
        file.path(panel_rsdp_imp_dir,
                  "all_units", "results_datasets",
                  "appended_datasets",
                  "twowayFE_results.Rds"))


