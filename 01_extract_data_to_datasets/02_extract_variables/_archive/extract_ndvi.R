# Extract NDVI

# Extract average NDVI in polygons -- use all areas and areas just defined
# as cropland.

# NOTE: This script takes about an hour to run

INCLUDE_NDVI_ONLY_IN_CROPLAND <- F

# Load Polygon Data ------------------------------------------------------------
polygons <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "polygons.Rds"))

# Determine Constant Cropland Areas --------------------------------------------
# Start with cropland in baseline year calling it cropland_constant. Then, loop 
# through all years of globcover. Here, we update cropland_constant, removing
# areas not cropland in the current year.

if(INCLUDE_NDVI_ONLY_IN_CROPLAND){
  cropland_constant <- raster(file.path(gc_dir, "RawData", "1992_2015_data", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), band=(1992-1991)) %>% crop(extent(polygons))
  cropland_constant[] <- as.numeric(cropland_constant[] %in% c(10,20,30))
  
  for(year in 1993:2015){
    print(year)
    cropland_yyyy <- raster(file.path(gc_dir, "RawData", "1992_2015_data", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), band=(year-1991)) %>% crop(extent(polygons))
    cropland_constant[] <- as.numeric((cropland_constant[] %in% 1) & (cropland_yyyy[] %in% c(10,20,30))) 
  }
  
  for(year in 2016:2018){
    print(year)
    cropland_yyyy <- raster(file.path(gc_dir, "RawData", "2016_2018_data", paste0("C3S-LC-L4-LCCS-Map-300m-P1Y-",year,"-v2.1.1.tif"))) %>% crop(extent(polygons))
    cropland_constant[] <- as.numeric((cropland_constant[] %in% 1) & (cropland_yyyy[] %in% c(10,20,30))) 
  }
  
  cropland_constant[][cropland_constant[] %in% 0] <- NA
}

# Add Data ---------------------------------------------------------------------
# Extract two things:
# (1) Average NDVI within each polygon
# (2) Average NDVI within each polygon, only considering cropland areas. So
#     if a polygon only is partially covered by cropland, we only consider
#     that area.

extract_ndvi_to_polygons <- function(year, polygons){
  print(year)
  
  #### Load NDVI
  #if(year <= 1998) ndvi <- raster(file.path(data_file_path, "NDVI", "RawData", "Landsat", paste0("eth_ls5_ndvi_annual_",year,".tif")))  
  #if(year > 1998)  ndvi <- raster(file.path(data_file_path, "NDVI", "RawData", "Landsat", paste0("eth_ls7_ndvi_annual_",year,".tif")))  
  
  if(year <= 1998) ndvi <- raster(file.path(ndvi_dir, "RawData", paste0("eth_ls5_ndvi_annual_",year,".tif")))  
  if(year > 1998)  ndvi <- raster(file.path(ndvi_dir, "RawData", paste0("eth_ls7_ndvi_annual_",year,".tif")))  
  
  #### Extract NDVI
  polygons$ndvi <- velox(ndvi)$extract(sp=polygons, fun=function(x){mean(x, na.rm=T)}) %>% as.vector()
  
  #### Extract NDVI in Cropland Areas
  if(INCLUDE_NDVI_ONLY_IN_CROPLAND){
    ndvi_resample <- resample(ndvi, cropland_constant)
    ndvi_cropland <- overlay(ndvi_resample, cropland_constant, fun=function(x,y){return(x*y)} )
    polygons$ndvi_cropland <- velox(ndvi_cropland)$extract(sp=polygons, fun=function(x){mean(x, na.rm=T)}) %>% as.vector()
  } 

  polygons$year <- year
  
  return(polygons@data)
}

polygons_ndvi <- lapply(1992:2018, extract_ndvi_to_polygons, polygons) %>% bind_rows

# Export -----------------------------------------------------------------------
saveRDS(polygons_ndvi, file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "ndvi.Rds"))

