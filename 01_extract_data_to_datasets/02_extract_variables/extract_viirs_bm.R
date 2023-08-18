# Extract VIIRS BlackMarble

# Load Data --------------------------------------------------------------------
sdf <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "points.Rds"))

# Add Data ---------------------------------------------------------------------
extract_viirs_to_points <- function(year, sdf){
  print(year)
  
  viirs_r <- raster(file.path(ntl_bm_dir, "RawData", paste0("VNP46A4_t",year,".tif"))) %>% crop(sdf)
  
  if(grepl("grid", DATASET_TYPE)){
    sdf$viirs_bm <- raster::extract(viirs_r, sdf) %>% as.numeric()
  } else{
    sdf$viirs_bm <- exact_extract(viirs_r, sdf, 'mean')
  }
  
  sdf$year <- year
  
  return(sdf@data)
}

sdf_all <- map_df(2012:2022, extract_viirs_to_points, sdf)

# Export -----------------------------------------------------------------------
saveRDS(sdf_all, file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "viirs_bm.Rds"))

