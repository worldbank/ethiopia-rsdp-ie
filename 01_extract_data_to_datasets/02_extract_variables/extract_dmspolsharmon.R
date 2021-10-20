# Extract DMSP-OLS to Points

# Load Data --------------------------------------------------------------------
sdf <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "points.Rds"))

# Add Data ---------------------------------------------------------------------
extract_dmspols_harmon_to_points <- function(year, sdf){
  print(year)
  if(year %in% 1992:2013){
    dmspols <- raster(file.path(ntl_harmon_dir, "RawData", paste0("Harmonized_DN_NTL_",year,"_calDMSP.tif"))) %>% crop(sdf)
  } else{
    dmspols <- raster(file.path(ntl_harmon_dir, "RawData", paste0("Harmonized_DN_NTL_",year,"_simVIIRS.tif"))) %>% crop(sdf)
  }
  
  if(grepl("grid", DATASET_TYPE)){
    sdf$dmspols_harmon <- raster::extract(dmspols, sdf) %>% as.numeric()
    
    #sdf$dmspols_harmon <- dmspols_vx$extract_points(sp=sdf) %>% as.numeric
  } else {
    dmspols_vx <- velox(dmspols)
    
    # Average NTL value
    sdf$dmspols_harmon   <- dmspols_vx$extract(sp=sdf, fun=function(x){mean(x, na.rm=T)}, small = T) %>% as.numeric
    
    # Proportion of unit above NTL threshold
    sdf$dmspols_harmon_1 <- dmspols_vx$extract(sp=sdf, fun=function(x){mean(x >= 1, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_harmon_2 <- dmspols_vx$extract(sp=sdf, fun=function(x){mean(x >= 2, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_harmon_6 <- dmspols_vx$extract(sp=sdf, fun=function(x){mean(x >= 6, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_harmon_10 <- dmspols_vx$extract(sp=sdf, fun=function(x){mean(x >= 10, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_harmon_15 <- dmspols_vx$extract(sp=sdf, fun=function(x){mean(x >= 15, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_harmon_20 <- dmspols_vx$extract(sp=sdf, fun=function(x){mean(x >= 20, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_harmon_25 <- dmspols_vx$extract(sp=sdf, fun=function(x){mean(x >= 25, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_harmon_30 <- dmspols_vx$extract(sp=sdf, fun=function(x){mean(x >= 30, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_harmon_33 <- dmspols_vx$extract(sp=sdf, fun=function(x){mean(x > 33, na.rm=T)}, small = T) %>% as.numeric
    
    # Number of cells above NTL threshold
    sdf$dmspols_harmon_sum         <- dmspols_vx$extract(sp=sdf, fun=function(x){sum(x, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_harmon_sum0greater <- dmspols_vx$extract(sp=sdf, fun=function(x){sum(x > 0, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_harmon_sum1        <- dmspols_vx$extract(sp=sdf, fun=function(x){sum(x >= 1, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_harmon_sum2        <- dmspols_vx$extract(sp=sdf, fun=function(x){sum(x >= 2, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_harmon_sum6        <- dmspols_vx$extract(sp=sdf, fun=function(x){sum(x >= 6, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_harmon_sum10        <- dmspols_vx$extract(sp=sdf, fun=function(x){sum(x >= 10, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_harmon_sum15        <- dmspols_vx$extract(sp=sdf, fun=function(x){sum(x >= 15, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_harmon_sum20        <- dmspols_vx$extract(sp=sdf, fun=function(x){sum(x >= 20, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_harmon_sum25        <- dmspols_vx$extract(sp=sdf, fun=function(x){sum(x >= 25, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_harmon_sum30        <- dmspols_vx$extract(sp=sdf, fun=function(x){sum(x >= 30, na.rm=T)}, small = T) %>% as.numeric
    sdf$dmspols_harmon_sum33       <- dmspols_vx$extract(sp=sdf, fun=function(x){sum(x >  33, na.rm=T)}, small = T) %>% as.numeric
  } 
  
  sdf$year <- year
  return(sdf@data)
}

sdf_all <- lapply(1992:2018, extract_dmspols_harmon_to_points, sdf) %>% bind_rows

# Export -----------------------------------------------------------------------
saveRDS(sdf_all, file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "dmspolsharmon.Rds"))

