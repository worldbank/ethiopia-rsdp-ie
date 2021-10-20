# Merge Datasets Together

# Load Data / Create Dataset Lists -----------------------------------------------

#### Load dataset to merge into
points_all <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "individual_datasets", "dmspols.Rds"))

#### Names of datasets to merge in
# Separate by:
#  -- time invarient (merge by cell_id)
#  -- time varying (merge by cell_id and year)

DATASETS_TIME_INVARIANT <- c("distance_roads_by_rsdp_phase.Rds",
                             "distance_roads_any_improved_ever.Rds",
                             "distance_roads_any_2016_ever.Rds",
                             "distance_hypothetical_road_least_cost_mst.Rds",
                             "distance_hypothetical_road_min_dist_mst.Rds",
                             "distance_rsdp_iii_roads_mst_and_targetted_areas.Rds",
                             "distance_rsdp_iv_roads_mst_and_targetted_areas.Rds",
                             "adm_units.Rds",
                             "area.Rds",
                             "cluster_n_cells.Rds",
                             "distance_cities.Rds")

DATASETS_TIME_VARYING <- c("viirs.Rds",
                           "temperature.Rds",
                           "precipitation.Rds",
                           "ndvi.Rds",
                           "ma2_market_access.Rds",
                           "globcover.Rds",
                           "dmspolsharmon.Rds",
                           "dmspols_intercalibrated_zhang.Rds",
                           #"distance_roads_improved_by_speedlimit_before.Rds",
                           "distance_roads_improved_by_speedlimit_after.Rds",
                           "distance_roads_by_speedlimit.Rds")

# Merge ------------------------------------------------------------------------
for(dataset in DATASETS_TIME_VARYING){
  print(dataset)
  dataset_temp <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "individual_datasets", dataset)) 
  points_all <- merge(points_all, dataset_temp, by=c("cell_id", "year"), all=T)
}

for(dataset in DATASETS_TIME_INVARIANT){
  print(dataset)
  dataset_temp <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "individual_datasets", dataset))
  points_all <- merge(points_all, dataset_temp, by="cell_id", all=T)
}

# Export -----------------------------------------------------------------------
saveRDS(points_all, file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "merged_datasets", "panel_data.Rds"))


