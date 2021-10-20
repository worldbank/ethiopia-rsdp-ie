# Extract GADM to Points

# Load Data --------------------------------------------------------------------
if(GRID_DATASET){
  sdf <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "polygons.Rds"))
} else{
  sdf <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "polygons_no_road_cut.Rds"))
}
sdf <- spTransform(sdf, CRS(UTM_ETH))

sdf$area_polygon <- gArea(sdf, byid=T) %>% as.vector()
sdf$area_polygon <- sdf$area_polygon / 1000^2

# Export -----------------------------------------------------------------------
saveRDS(sdf@data, file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "area.Rds"))
