# Woreda Summary Trends

# Woreda Data ------------------------------------------------------------------
grid_full <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_ethiopia", "merged_datasets", "panel_data_clean.Rds"))
grid_full <- grid_full %>% 
  filter(year == 1996) %>%
  dplyr::select(dmspols)

grid_full$dmspols[grid_full$dmspols > 0] %>% median(na.rm=T)
