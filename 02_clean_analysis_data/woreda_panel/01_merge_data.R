# Merge Datasets Together

# Load Data / Create Dataset Lists -----------------------------------------------

#### Load dataset to merge into
# TODO: doesn't contain all woredas, as cut the roads. Need FULL dataset.
#points_all <- readRDS(file.path(panel_rsdp_imp_data_file_path, "woreda", "individual_datasets", "dmspols.Rds"))

## Make blank data to merge into
# Not all the time varying datasets contain all the woredas, hence we do this.
# This is because when we remove areas near a road, some complete woredas are
# removed
df_nrow <- readRDS(file.path(panel_rsdp_imp_dir, "woreda", "individual_datasets", "area.Rds")) %>%
  nrow()

points_all <- data.frame(cell_id = 1:df_nrow)
points_all$year <- rep(1992:2019, length.out = nrow(points_all))

points_all <- points_all %>% 
  complete(cell_id, year)

#### Names of datasets to merge in
# Separate by:
#  -- time invarient (merge by cell_id)
#  -- time varying (merge by cell_id and year)

DATASETS_TIME_INVARIANT <- c("woreda_details.Rds",
                             "distance_roads_by_rsdp_phase.Rds",
                             "distance_roads_any_improved_ever.Rds",
                             "distance_roads_any_2016_ever.Rds",
                             #"distance_hypothetical_road_least_cost_mst.Rds",
                             "distance_rsdp_iii_roads_mst_and_targetted_areas.Rds",
                             #"distance_rsdp_iv_roads_mst_and_targetted_areas.Rds",
                             "distance_addis.Rds",
                             "gpw.Rds",
                             "area.Rds")

DATASETS_TIME_VARYING <- c(#"viirs.Rds",
                           "temperature.Rds",
                           "precipitation.Rds",
                           "road_length.Rds",
                           #"ndvi.Rds",
                           "ma3_market_access.Rds", #TODO: Should this be ma3_market_access.Rds?
                           "globcover.Rds",
                           #"dmspols.Rds",
                           "dmspolsharmon.Rds",
                           #"dmspols_intercalibrated_zhang.Rds",
                           #"distance_roads_improved_by_speedlimit_before.Rds",
                           #"distance_roads_improved_by_speedlimit_after.Rds",
                           "distance_roads_by_speedlimit.Rds")

# Merge ------------------------------------------------------------------------
for(dataset in DATASETS_TIME_VARYING){
  print(dataset)
  dataset_temp <- readRDS(file.path(panel_rsdp_imp_dir, "woreda", "individual_datasets", dataset)) %>% data.table 
  points_all <- merge(points_all, dataset_temp, by=c("cell_id", "year"), all=T)
}

for(dataset in DATASETS_TIME_INVARIANT){
  print(dataset)
  dataset_temp <- readRDS(file.path(panel_rsdp_imp_dir, "woreda", "individual_datasets", dataset)) %>% data.table 
  points_all <- merge(points_all, dataset_temp, by="cell_id", all=T)
}

# Export -----------------------------------------------------------------------
saveRDS(points_all, file.path(panel_rsdp_imp_dir, "woreda", "merged_datasets", "panel_data.Rds"))






