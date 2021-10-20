# Extract GADM to Points

# Load Data --------------------------------------------------------------------
points <- readRDS(file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "points.Rds"))

# Add Data ---------------------------------------------------------------------
extract_viirs_to_points <- function(year, points){
  print(year)
  viirs_median <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS", "RawData", "Annual", "median", paste0("eth_viirs_median_",year,".tif")))
  viirs_mean <- raster(file.path(data_file_path,   "Nighttime Lights", "VIIRS", "RawData", "Annual", "mean",   paste0("eth_viirs_mean_",year,".tif")))
  viirs_max <- raster(file.path(data_file_path,    "Nighttime Lights", "VIIRS", "RawData", "Annual", "max",    paste0("eth_viirs_max_",year,".tif")))
  
  if(grepl("grid", DATASET_TYPE)){
    points$viirs_median <- velox(viirs_median)$extract_points(sp=points) %>% as.numeric
    points$viirs_mean   <- velox(viirs_mean)$extract_points(sp=points) %>% as.numeric
    points$viirs_max    <- velox(viirs_max)$extract_points(sp=points) %>% as.numeric
    
  } else{
    points$viirs_median <- velox(viirs_median)$extract(sp=points, fun=function(x){median(x, na.rm=T)}) %>% as.numeric
    points$viirs_mean <- velox(viirs_mean)$extract(sp=points, fun=function(x){mean(x, na.rm=T)}) %>% as.numeric
    points$viirs_max <- velox(viirs_max)$extract(sp=points, fun=function(x){max(x, na.rm=T)}) %>% as.numeric
    
    points$viirs_mean_2 <- velox(viirs_mean)$extract(sp=points, fun=function(x){mean(x >= 2, na.rm=T)}) %>% as.numeric
    points$viirs_mean_6 <- velox(viirs_mean)$extract(sp=points, fun=function(x){mean(x >= 6, na.rm=T)}) %>% as.numeric
  }
  
  points$year <- year
  return(points@data)
}

points_all <- lapply(2012:2019, extract_viirs_to_points, points) %>% bind_rows

# Export -----------------------------------------------------------------------
saveRDS(points_all, file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "viirs.Rds"))

