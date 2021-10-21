# Merge Datasets Together

#### Parameters
# Vector of all baseline and endline years that would later use
YEARS_USE <- c(1992, 1996, 2009, 2013, 2016)

# Load Data / Create Dataset Lists -----------------------------------------------

## Load dataset to merge into
# Use panel data with all years
points_all <- readRDS(file.path(panel_rsdp_imp_dir, "dmspols_grid_ethiopia", 
                                "individual_datasets", "globcover.Rds")) %>% 
  data.table

points_all <- points_all[points_all$year %in% YEARS_USE,]

#### Names of datasets to merge in
# Separate by:
#  -- time invarient (merge by cell_id)
#  -- time varying (merge by cell_id and year)

DATASETS_TIME_INVARIANT <- c("distance_roads_any_improved_ever.Rds",
                             "distance_roads_any_improved_by2012.Rds",
                             "distance_roads_any_2012_ever.Rds",
                             "distance_roads_any_2016_ever.Rds",
                             "distance_rsdp_iii_roads_mst_and_targetted_areas.Rds",
                             "distance_cities.Rds",
                             "adm_units.Rds",
                             "ddistance_woreda_populous_location.Rds")

DATASETS_TIME_VARYING <- c("temperature.Rds",
                           "precipitation.Rds",
                           "dmspolsharmon.Rds")

# Merge ------------------------------------------------------------------------
for(dataset in DATASETS_TIME_VARYING){
  print(dataset)
  
  dataset_temp <- readRDS(file.path(panel_rsdp_imp_dir, "dmspols_grid_ethiopia", 
                                    "individual_datasets", dataset)) %>% data.table
  
  dataset_temp <- dataset_temp[dataset_temp$year %in% YEARS_USE,]
  
  points_all <- merge(points_all, dataset_temp, by=c("cell_id", "year"), all=T)
}

for(dataset in DATASETS_TIME_INVARIANT){
  print(dataset)
  
  dataset_temp <- readRDS(file.path(panel_rsdp_imp_dir, "dmspols_grid_ethiopia", 
                                    "individual_datasets", dataset)) %>% data.table 
  
  points_all <- merge(points_all, dataset_temp, by="cell_id", all=T)
}

# Export -----------------------------------------------------------------------
saveRDS(points_all, file.path(panel_rsdp_imp_dir, "dmspols_grid_ethiopia", "merged_datasets", "panel_data.Rds"))

