# Merge Datasets Together

# Load Data / Create Dataset Lists ---------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "woreda", "merged_datasets", "panel_data_clean.Rds"))

data <- data %>%
  dplyr::select(woreda_id, dmspols_1996_bin4) %>%
  distinct()

data$dmspols_1996_bin4 <- data$dmspols_1996_bin4 %>% as.character()

# Export -----------------------------------------------------------------------
saveRDS(data, file.path(panel_rsdp_imp_data_file_path, "woreda", "merged_datasets", "woreda_bins.Rds"))

