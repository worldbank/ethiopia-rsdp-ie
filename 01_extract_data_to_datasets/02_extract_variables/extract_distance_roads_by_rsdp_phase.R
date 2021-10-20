# Distance to Roads by Speed Limit and Year

# Calculate distance of each point to the closest road for each speed limit in 
# each year.

# Load and Prep Data -----------------------------------------------------------
# Load data and reporject to Ethiopia UTM. UTM better for distance calculations 
# than WGS84.

#### Load points
if(GRID_DATASET){
  points <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "points.Rds"))
} else{
  points <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "points_no_road_cut.Rds"))
}
points <- points %>% spTransform(CRS(UTM_ETH))

#### Load roads
roads_sdf <- readRDS(file.path(rsdp_dir, "RawData", "RoadNetworkPanelData_1996_2016.Rds"))
roads_sdf$id <- 1 # useful to have a variable the same for all obs when aggreagting roads later
roads_sdf <- roads_sdf %>% spTransform(CRS(UTM_ETH))

# Separate into Phases ---------------------------------------------------------
roads_sdf_p1 <- roads_sdf[roads_sdf$rsdp_phase %in% 1,] %>% st_as_sf() %>% st_combine() %>% as("Spatial")
roads_sdf_p2 <- roads_sdf[roads_sdf$rsdp_phase %in% 2,] %>% st_as_sf() %>% st_combine() %>% as("Spatial")
roads_sdf_p3 <- roads_sdf[roads_sdf$rsdp_phase %in% 3,] %>% st_as_sf() %>% st_combine() %>% as("Spatial")
roads_sdf_p4 <- roads_sdf[roads_sdf$rsdp_phase %in% 4,] %>% st_as_sf() %>% st_combine() %>% as("Spatial")

roads_sdf_p1$id <- 1
roads_sdf_p2$id <- 1
roads_sdf_p3$id <- 1
roads_sdf_p4$id <- 1

# Distance to Phases -----------------------------------------------------------
points$distance_rsdp_phase1 <- gDistance_chunks(points, roads_sdf_p1, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)
points$distance_rsdp_phase2 <- gDistance_chunks(points, roads_sdf_p2, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)
points$distance_rsdp_phase3 <- gDistance_chunks(points, roads_sdf_p3, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)
points$distance_rsdp_phase4 <- gDistance_chunks(points, roads_sdf_p4, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)

# Export -----------------------------------------------------------------------
saveRDS(points@data, file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "distance_roads_by_rsdp_phase.Rds"))

