# Merge Datasets Together

# Load Data / Create Dataset Lists -----------------------------------------------
# Load data that we'll merge datasets into and create lists of datasets to merge
# into this dataset.

#### Load dataset to merge into
points <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_nearroad_randomsample", "individual_datasets", "points.Rds"))

#### Names of datasets to merge in
# Separate by:
#  -- time invarient (merge by cell_id)
#  -- time varying (merge by cell_id and year)

DATASETS_TIME_INVARIANT <- c("adm_units.Rds",
                             "distance_anyroad_improved_ever.Rds",
                             "distance_anyroad2016.Rds",
                             "distance_cities.Rds",
                             "distance_rsdp_phases.Rds",
                             "distance_least_cost_path_mst.Rds")

DATASETS_TIME_VARYING <- c("dmspols_zhang.Rds",
                           "dmspols.Rds",
                           "viirs.Rds",
                           "globcover.Rds",
                           "temperature.Rds",
                           "precipitation.Rds",
                           #"ndvi.Rds",
                           "distance_roads_byspeed.Rds",
                           #"distance_improved_roads_byspeed_before.Rds",
                           "distance_improved_roads_byspeed_after.Rds")

# Merge ------------------------------------------------------------------------
for(dataset in DATASETS_TIME_VARYING){
  print(dataset)
  dataset_temp <- readRDS(file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", dataset)) %>% data.table 
  points_all <- merge(points_all, dataset_temp, by=c("cell_id", "year"), all=T)
  rm(dataset_temp); gc()
}

for(dataset in DATASETS_TIME_INVARIANT){
  print(dataset)
  dataset_temp <- readRDS(file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", dataset)) %>% data.table 
  points_all <- merge(points_all, dataset_temp, by="cell_id", all=T)
  rm(dataset_temp); gc()
}

# Export -----------------------------------------------------------------------
#### Remove Unneeded variables
points_all$woreda_hdx_z_name <- NULL
points_all$woreda_hdx_w_name <- NULL
points_all$woreda_hdx_w_pop2007 <- NULL
points_all$woreda_hdx_w_density <- NULL

points_all$distance_city_popsize_3groups_g1 <- NULL
points_all$distance_city_popsize_3groups_g2 <- NULL
points_all$distance_city_popsize_3groups_g3 <- NULL
points_all$distance_city_all <- NULL

points_all$globcover_cropland_rainfed <- NULL
points_all$globcover_cropland_irrigated <- NULL
points_all$globcover_cropland_mosaic <- NULL

saveRDS(points_all, file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_nearroad_randomsample", "merged_datasets", "panel_data.Rds"))






