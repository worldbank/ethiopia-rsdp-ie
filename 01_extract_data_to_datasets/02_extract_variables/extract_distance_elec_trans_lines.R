# Distance to Electricity Transmission Lines

# Calculate distance of each point to the closest road for each speed limit in 
# each year.

# Load and Prep Data -----------------------------------------------------------
# Load data and reporject to Ethiopia UTM. UTM better for distance calculations.

#### Load points
if(GRID_DATASET){
  points <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "points.Rds"))
} else{
  points <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "points_no_road_cut.Rds"))
}
points <- points %>% spTransform(CRS(UTM_ETH))

#### Load roads
elec_sdf <- readOGR(file.path(elec_net_dir, "RawData", "Ethiopia Electricity Transmission Network.shp"))
elec_sdf <- elec_sdf %>% spTransform(CRS(UTM_ETH))

# Calculate Distance -----------------------------------------------------------
elec_sdf <- elec_sdf %>% st_as_sf() %>% st_combine() %>% as("Spatial")
elec_sdf$id <- 1

points$distance_elec_trans <- gDistance_chunks(points, elec_sdf, CHUNK_SIZE_DIST_ROADS)

# Export -----------------------------------------------------------------------
saveRDS(points@data, file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "distance_elec_trans_lines.Rds"))

