# Extract Distance to Cities

# Load Data --------------------------------------------------------------------
#### Points
if(GRID_DATASET){
  points <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "points.Rds"))
} else{
  points <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "points_no_road_cut.Rds"))
}

points <- spTransform(points, CRS(UTM_ETH))

#### Cities
addis_sp <- data.frame(
  latitude = 9.03,
  longitude = 38.74,
  name = "Addis Ababa"
)

coordinates(addis_sp) <- ~longitude+latitude
crs(addis_sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
addis_sp <- spTransform(addis_sp, CRS(UTM_ETH))

# Distance ---------------------------------------------------------------------
points$distance_city_addisababa <- gDistance_chunks(points, addis_sp, chunk_size = 1000) %>%
  as.numeric()

# Export -----------------------------------------------------------------------
saveRDS(points@data, file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "distance_addis.Rds"))




