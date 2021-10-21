# Distance to Roads by Speed Limit and Year

# Calculate distance of each point to the closest road for each speed limit in 
# each year.

# Load and Prep Data -----------------------------------------------------------
# Load data and project to Ethiopia UTM. UTM better for distance calculations.

#### Load points
if(GRID_DATASET){
  points <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "points.Rds"))
} else{
  points <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "points_no_road_cut.Rds"))
}
points <- points %>% spTransform(CRS(UTM_ETH))

#### Load roads
roads_sdf <- readRDS(file.path(rsdp_dir, "RawData", "RoadNetworkPanelData_1996_2016.Rds"))

## Restrict to improved
roads_sdf <- roads_sdf[roads_sdf$Speed2016 > roads_sdf$Speed1996,]

roads_sdf$id <- 1 # useful to have a variable the same for all obs when aggreagting roads later
roads_sdf <- roads_sdf %>% spTransform(CRS(UTM_ETH))

# Calculate Distance -----------------------------------------------------------
roads_sdf <- roads_sdf %>% st_as_sf() %>% st_combine() %>% as("Spatial")
roads_sdf$id <- 1

points$distance_anyimproved_ever <- gDistance_chunks(points, roads_sdf, CHUNK_SIZE_DIST_ROADS)

# Export -----------------------------------------------------------------------
saveRDS(points@data, file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "distance_roads_any_improved_ever.Rds"))

