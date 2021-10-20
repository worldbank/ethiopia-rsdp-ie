# Extract GADM to Points

# Load Data --------------------------------------------------------------------
points <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "points.Rds"))

# Add Data ---------------------------------------------------------------------
extract_precip_to_points <- function(year, points){
  print(year)
  temp_avg <- raster(file.path(temp_dir, "RawData", paste0("eth_temp_",year,"_avg_FLDAS_10km.tif")))
  temp_min <- raster(file.path(temp_dir, "RawData", paste0("eth_temp_",year,"_min_FLDAS_10km.tif")))
  temp_max <- raster(file.path(temp_dir, "RawData", paste0("eth_temp_",year,"_max_FLDAS_10km.tif")))
  
  if(grepl("grid", DATASET_TYPE)){
    points$temp_avg <- velox(temp_avg)$extract_points(sp=points) %>% as.numeric
    points$temp_min <- velox(temp_min)$extract_points(sp=points) %>% as.numeric
    points$temp_max <- velox(temp_max)$extract_points(sp=points) %>% as.numeric
  } else{
    points$temp_avg <- velox(temp_avg)$extract(sp=points, fun=function(x){mean(x, na.rm=T)}) %>% as.numeric
    points$temp_min <- velox(temp_min)$extract(sp=points, fun=function(x){mean(x, na.rm=T)}) %>% as.numeric
    points$temp_max <- velox(temp_max)$extract(sp=points, fun=function(x){mean(x, na.rm=T)}) %>% as.numeric
    
    #### If NA, use centroid
    points_NA <- points[is.na(points$temp_avg),] 
    points_NA <- coordinates(points_NA) %>%
      as.data.frame() %>%
      dplyr::rename(long = V1,
                    lat = V2)
    coordinates(points_NA) <- ~long+lat
    crs(points_NA) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    points_NA$temp_avg <- raster::extract(temp_avg, points_NA, fun = mean)
    points_NA$temp_min <- raster::extract(temp_min, points_NA, fun = mean)
    points_NA$temp_max <- raster::extract(temp_max, points_NA, fun = mean)
    
    points$temp_avg[is.na(points$temp_avg)] <- points_NA$temp_avg
    points$temp_min[is.na(points$temp_avg)] <- points_NA$temp_min
    points$temp_max[is.na(points$temp_avg)] <- points_NA$temp_max

  }
  
  points$year <- year
  return(points@data)
}

points_all <- lapply(1992:2018, extract_precip_to_points, points) %>% bind_rows

# Export -----------------------------------------------------------------------
saveRDS(points_all, file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "temperature.Rds"))

