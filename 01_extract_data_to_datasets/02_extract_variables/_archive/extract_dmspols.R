# Extract DMSP-OLS to Points

# Load Data --------------------------------------------------------------------
sdf <- readRDS(file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "points.Rds"))

# Add Data ---------------------------------------------------------------------
extract_dmspols_to_points <- function(year, sdf){
  print(year)
  dmspols <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Stacked", "eth_dmspols_allyears.tif"),(year-1991))
  
  dmspols_vx <- velox(dmspols)
  
  if(grepl("grid", DATASET_TYPE)){
    sdf$dmspols <- dmspols_vx$extract_points(sp=sdf) %>% as.numeric
  } else {
    # Average NTL value
    sdf$dmspols   <- dmspols_vx$extract(sp=sdf, fun=function(x){mean(x, na.rm=T)}, small = T) %>% as.numeric
    
    # Proportion of unit above NTL threshold
    sdf$dmspols_1 <- dmspols_vx$extract(sp=sdf, fun=function(x){mean(x >= 1, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_2 <- dmspols_vx$extract(sp=sdf, fun=function(x){mean(x >= 2, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_6 <- dmspols_vx$extract(sp=sdf, fun=function(x){mean(x >= 6, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_10 <- dmspols_vx$extract(sp=sdf, fun=function(x){mean(x >= 10, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_15 <- dmspols_vx$extract(sp=sdf, fun=function(x){mean(x >= 15, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_20 <- dmspols_vx$extract(sp=sdf, fun=function(x){mean(x >= 20, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_25 <- dmspols_vx$extract(sp=sdf, fun=function(x){mean(x >= 25, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_30 <- dmspols_vx$extract(sp=sdf, fun=function(x){mean(x >= 30, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_33 <- dmspols_vx$extract(sp=sdf, fun=function(x){mean(x > 33, na.rm=T)}, small = T) %>% as.numeric
    
    # Number of cells above NTL threshold
    sdf$dmspols_sum         <- dmspols_vx$extract(sp=sdf, fun=function(x){sum(x, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_sum0greater <- dmspols_vx$extract(sp=sdf, fun=function(x){sum(x > 0, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_sum1        <- dmspols_vx$extract(sp=sdf, fun=function(x){sum(x >= 1, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_sum2        <- dmspols_vx$extract(sp=sdf, fun=function(x){sum(x >= 2, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_sum6        <- dmspols_vx$extract(sp=sdf, fun=function(x){sum(x >= 6, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_sum10        <- dmspols_vx$extract(sp=sdf, fun=function(x){sum(x >= 10, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_sum15        <- dmspols_vx$extract(sp=sdf, fun=function(x){sum(x >= 15, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_sum20        <- dmspols_vx$extract(sp=sdf, fun=function(x){sum(x >= 20, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_sum25        <- dmspols_vx$extract(sp=sdf, fun=function(x){sum(x >= 25, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_sum30        <- dmspols_vx$extract(sp=sdf, fun=function(x){sum(x >= 30, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_sum33       <- dmspols_vx$extract(sp=sdf, fun=function(x){sum(x >  33, na.rm=T)}, small = T) %>% as.numeric
  } 
  
  sdf$year <- year
  return(sdf@data)
}

sdf_all <- lapply(1992:2013, extract_dmspols_to_points, sdf) %>% bind_rows

# Export -----------------------------------------------------------------------
saveRDS(sdf_all, file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "dmspols.Rds"))

