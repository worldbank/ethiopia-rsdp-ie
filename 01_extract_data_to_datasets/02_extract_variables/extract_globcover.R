# Extract Globcover to Points

# Load Data --------------------------------------------------------------------
polygons <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "polygons.Rds"))

# Add Data ---------------------------------------------------------------------
extract_globcover <- function(year){
  print(year)
  
  if(year %in% 1992:2015) globcover <- raster(file.path(gc_dir, "RawData", "1992_2015_data", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), (year-1991)) %>% crop(extent(polygons))
  if(year %in% 2016:2018) globcover <- raster(file.path(gc_dir, "RawData", "2016_2018_data", paste0("C3S-LC-L4-LCCS-Map-300m-P1Y-",year,"-v2.1.1.tif"))) %>% crop(extent(polygons))

  globcover_urban_fun <- function(r){
    r[] <- as.numeric(r[] %in% c(190))
    return(r)
  }
  
  globcover_cropland_fun <- function(r){
    r[] <- as.numeric(r[] %in% c(10,11,12,20,30))
    return(r)
  }
  
  globcover_cropland_rainfed_fun <- function(r){
    r[] <- as.numeric(r[] %in% c(10))
    return(r)
  }
  
  globcover_cropland_irrigated_fun <- function(r){
    r[] <- as.numeric(r[] %in% c(20))
    return(r)
  }
  
  globcover_cropland_mosaic_fun <- function(r){
    r[] <- as.numeric(r[] %in% c(30))
    return(r)
  }
  
  globcover_urban <- calc(globcover, fun=globcover_urban_fun)
  globcover_cropland <- calc(globcover, fun=globcover_cropland_fun)

  polygons$globcover_urban <- velox(globcover_urban)$extract(polygons, fun=mean, small = T)
  polygons$globcover_cropland <- velox(globcover_cropland)$extract(polygons, fun=mean, small = T)

  # If not a grid dataset, also take the sum (for total cells classifed as urban, for example)
  if(!grepl("grid", DATASET_TYPE)){
    polygons$globcover_urban_sum <- velox(globcover_urban)$extract(polygons, fun=sum, small = T)
    polygons$globcover_cropland_sum <- velox(globcover_cropland)$extract(polygons, fun=sum, small = T)
  }
  
  polygons$year <- year
  
  return(polygons@data)
}

polygons_globcover <- lapply(1992:2018, extract_globcover) %>% bind_rows

# Export -----------------------------------------------------------------------
saveRDS(polygons_globcover, file.path(panel_rsdp_imp_dir, DATASET_TYPE, 
                                      "individual_datasets", "globcover.Rds"))

