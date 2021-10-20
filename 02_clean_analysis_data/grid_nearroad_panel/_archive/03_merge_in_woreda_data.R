# Merge in Woreda Data

# Merge in nighttime lights data at woreda level. In some analyses, use
# woreda level information to subset grids

# Load Data --------------------------------------------------------------------
woreda_data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "woreda", "merged_datasets", "panel_data_clean.Rds"))
grid_data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_nearroad", "merged_datasets", "grid_data_clean.Rds"))

# Prep Woreda Data -------------------------------------------------------------
woreda_data <- woreda_data %>%
  dplyr::select(woreda_id, year, 
                dmspols_1996) %>%
  dplyr::rename(dmspols_1996_woreda = dmspols_1996)

# Merge ------------------------------------------------------------------------
grid_data <- grid_data %>% data.table()
woreda_data <- woreda_data %>% data.table()
grid_data_m <- merge(grid_data, 
                   woreda_data, 
                   by=c("woreda_id", "year")) %>%
  as.data.frame()

grid_data <- grid_data %>%
  group_by(woreda_id)

# Export -----------------------------------------------------------------------
saveRDS(grid_data, file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_nearroad", "merged_datasets", "grid_data_clean_woredadata.Rds"))

