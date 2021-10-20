# Extract DMSPOLS-Intercalibrated to Points

# Load Data --------------------------------------------------------------------
points <- readRDS(file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "points.Rds"))

read_ntl <- function(year){
  # Load NTL and crop to points extent. When 2 nigthttime light datasets for a year,
  # average together
  
  raster_filepaths <- list.files(file.path(data_file_path, "Nighttime Lights", "DMSPOLS_Intercalibrated", "RawData"),
             pattern=as.character(year),
             full.names = T)
  
  if(length(raster_filepaths) == 1){
    ntl <- raster(raster_filepaths[1]) %>% crop(extent(points))
  } else if(length(raster_filepaths) == 2){
    ntl_1 <- raster(raster_filepaths[1]) %>% crop(extent(points))
    ntl_2 <- raster(raster_filepaths[2]) %>% crop(extent(points))

    ntl <- ntl_1
    ntl[] <- (ntl_1[] + ntl_2[])/2
  }
  
  ntl[] <- ntl[]*0.01
  
  return(ntl)
}

# Add Data ---------------------------------------------------------------------
extract_raster_to_points <- function(year, points){
  print(year)
  dmspols <- read_ntl(year)
  
  dmspols_vx <- velox(dmspols)

  if(grepl("grid", DATASET_TYPE)){
    points$dmspols_zhang <- dmspols_vx$extract_points(sp=points) %>% as.numeric
    
  } else {
    # Average NTL value
    points$dmspols_zhang   <- dmspols_vx$extract(sp=points, fun=function(x){mean(x, na.rm=T)}, small = T) %>% as.numeric
    
    # Proportion of unit above NTL threshold
    points$dmspols_zhang_1 <- dmspols_vx$extract(sp=points, fun=function(x){mean(x >= 1, na.rm=T)}, small = T) %>% as.numeric
    points$dmspols_zhang_2 <- dmspols_vx$extract(sp=points, fun=function(x){mean(x >= 2, na.rm=T)}, small = T) %>% as.numeric
    points$dmspols_zhang_6 <- dmspols_vx$extract(sp=points, fun=function(x){mean(x >= 6, na.rm=T)}, small = T) %>% as.numeric
    points$dmspols_zhang_10 <- dmspols_vx$extract(sp=points, fun=function(x){mean(x >= 10, na.rm=T)}, small = T) %>% as.numeric
    points$dmspols_zhang_15 <- dmspols_vx$extract(sp=points, fun=function(x){mean(x >= 15, na.rm=T)}, small = T) %>% as.numeric
    points$dmspols_zhang_20 <- dmspols_vx$extract(sp=points, fun=function(x){mean(x >= 20, na.rm=T)}, small = T) %>% as.numeric
    points$dmspols_zhang_25 <- dmspols_vx$extract(sp=points, fun=function(x){mean(x >= 25, na.rm=T)}, small = T) %>% as.numeric
    points$dmspols_zhang_30 <- dmspols_vx$extract(sp=points, fun=function(x){mean(x >= 30, na.rm=T)}, small = T) %>% as.numeric
    points$dmspols_zhang_33 <- dmspols_vx$extract(sp=points, fun=function(x){mean(x > 33, na.rm=T)}, small = T) %>% as.numeric
    
    # Number of cells above NTL threshold
    points$dmspols_zhang_sum         <- dmspols_vx$extract(sp=points, fun=function(x){sum(x, na.rm=T)}, small = T) %>% as.numeric
    points$dmspols_zhang_sum0greater <- dmspols_vx$extract(sp=points, fun=function(x){sum(x > 0, na.rm=T)}, small = T) %>% as.numeric
    points$dmspols_zhang_sum1        <- dmspols_vx$extract(sp=points, fun=function(x){sum(x >= 1, na.rm=T)}, small = T) %>% as.numeric
    points$dmspols_zhang_sum2        <- dmspols_vx$extract(sp=points, fun=function(x){sum(x >= 2, na.rm=T)}, small = T) %>% as.numeric
    points$dmspols_zhang_sum6        <- dmspols_vx$extract(sp=points, fun=function(x){sum(x >= 6, na.rm=T)}, small = T) %>% as.numeric
    points$dmspols_zhang_sum10        <- dmspols_vx$extract(sp=points, fun=function(x){sum(x >= 10, na.rm=T)}, small = T) %>% as.numeric
    points$dmspols_zhang_sum15        <- dmspols_vx$extract(sp=points, fun=function(x){sum(x >= 15, na.rm=T)}, small = T) %>% as.numeric
    points$dmspols_zhang_sum20        <- dmspols_vx$extract(sp=points, fun=function(x){sum(x >= 20, na.rm=T)}, small = T) %>% as.numeric
    points$dmspols_zhang_sum25        <- dmspols_vx$extract(sp=points, fun=function(x){sum(x >= 25, na.rm=T)}, small = T) %>% as.numeric
    points$dmspols_zhang_sum30        <- dmspols_vx$extract(sp=points, fun=function(x){sum(x >= 30, na.rm=T)}, small = T) %>% as.numeric
    points$dmspols_zhang_sum33        <- dmspols_vx$extract(sp=points, fun=function(x){sum(x > 33, na.rm=T)}, small = T) %>% as.numeric
    
  } 
  
  points$year <- year
  return(points@data)
}

points_all <- lapply(1992:2012, extract_raster_to_points, points) %>% bind_rows

# Export -----------------------------------------------------------------------
saveRDS(points_all, file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "dmspols_intercalibrated_zhang.Rds"))

