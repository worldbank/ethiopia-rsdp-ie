# Merge Datasets Together

# Load Data / Create Dataset Lists -----------------------------------------------

#### Load dataset to merge into
points_all <- readRDS(file.path(panel_rsdp_imp_dir, "dmspols_grid_nearroad", 
                                "individual_datasets", "dmspolsharmon.Rds")) %>%
  data.table()

#### Names of datasets to merge in
# Separate by:
#  -- time invarient (merge by cell_id)
#  -- time varying (merge by cell_id and year)

DATASETS_TIME_INVARIANT <- c(#"distance_roads_by_rsdp_phase.Rds",
                             "distance_roads_any_improved_ever.Rds",
                             "distance_roads_any_2016_ever.Rds",
                             #"distance_hypothetical_road_least_cost_mst.Rds",
                             "distance_addis.Rds",
                             "adm_units.Rds")

DATASETS_TIME_VARYING <- c(#"viirs.Rds",
                           "temperature.Rds",
                           "precipitation.Rds",
                           #"ndvi.Rds",
                           "globcover.Rds",
                           #"dmspols_intercalibrated_zhang.Rds",
                           #"dmspolsharmon.Rds",
                           #"distance_roads_improved_by_speedlimit_before.Rds",
                           "distance_roads_improved_by_speedlimit_after.Rds",
                           "distance_roads_by_speedlimit.Rds")

# Merge ------------------------------------------------------------------------
for(dataset in DATASETS_TIME_VARYING){
  print(dataset)
  dataset_temp <- readRDS(file.path(panel_rsdp_imp_dir, "dmspols_grid_nearroad", "individual_datasets", dataset)) %>% data.table 
  points_all <- merge(points_all, dataset_temp, by=c("cell_id", "year"), all=T)
  rm(dataset_temp); gc()
}

for(dataset in DATASETS_TIME_INVARIANT){
  print(dataset)
  dataset_temp <- readRDS(file.path(panel_rsdp_imp_dir, "dmspols_grid_nearroad", "individual_datasets", dataset)) %>% data.table 
  points_all <- merge(points_all, dataset_temp, by="cell_id", all=T)
  rm(dataset_temp); gc()
}

# Remove Unneeded Variables ----------------------------------------------------
# Remove unneeded variables (variables will never use), to save memory later on.
points_all$woreda_pop2007 <- NULL
points_all$woreda_density2007 <- NULL

# points_all$distance_rsdp_phase1 <- NULL
# points_all$distance_rsdp_phase2 <- NULL
# points_all$distance_rsdp_phase3 <- NULL
# points_all$distance_rsdp_phase4 <- NULL

# Subset -----------------------------------------------------------------------
#points_all <- points_all[points_all$distance_anyroad2016 <= 5000,]
points_all <- points_all[points_all$distance_anyimproved_ever <= 5000,]
points_all <- points_all[points_all$distance_anyimproved_ever >= 1000,]
points_all <- points_all[!is.na(points_all$woreda_id),]

# Export -----------------------------------------------------------------------
saveRDS(points_all, file.path(panel_rsdp_imp_dir, "dmspols_grid_nearroad", "merged_datasets", "panel_data.Rds"))


