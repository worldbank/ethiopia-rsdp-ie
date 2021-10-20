# Append Market Access Datasets

ma_df <- file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets") %>%
  list.files(pattern = "*.Rds", full.names = T) %>%
  str_subset("ma2_market_access_") %>%
  map_df(readRDS)

saveRDS(ma_df, file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "ma3_market_access.Rds"))

