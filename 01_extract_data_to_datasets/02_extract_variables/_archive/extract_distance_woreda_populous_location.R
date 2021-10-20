# Extract Distance to Cities

# Load Data --------------------------------------------------------------------
if(GRID_DATASET){
  points <- readRDS(file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "points.Rds"))
} else{
  points <- readRDS(file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "points_no_road_cut.Rds"))
}

points <- points %>% spTransform(CRS(UTM_ETH))

#### Cities
woreda_points <- readRDS(file.path(data_file_path, "Hypothetical Road Networks", "points_to_connect.Rds"))
woreda_points <- woreda_points %>% spTransform(CRS(UTM_ETH))

# Aggregating only accepts SpatialPolyons, so buffer by small amount
woreda_points <- woreda_points %>% gBuffer(width=.1, byid=T)

#### All Cities
woreda_points$id <- 1
woreda_points <- woreda_points %>% raster::aggregate(by="id")

# Calculate Distance -----------------------------------------------------------
points$distance_woreda_pop_location <- gDistance_chunks(points, woreda_points, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)

# Export -----------------------------------------------------------------------
saveRDS(points@data, file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "ddistance_woreda_populous_location.Rds"))




