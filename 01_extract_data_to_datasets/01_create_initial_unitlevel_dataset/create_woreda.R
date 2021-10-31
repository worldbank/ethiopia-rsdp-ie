# Create Woreda Level Shapefile

# Create clean woreda level shapefile to merge into. Cut out areas within
# 1km of the road to prevent against capturing affects of just capturing the roads.

# Load Data --------------------------------------------------------------------
#### Woredas
woreda <- readRDS(file.path(woreda_dir, "FinalData", "woreda.Rds"))
#woreda <- spTransform(woreda, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
woreda <- spTransform(woreda, CRS(UTM_ETH))

woreda_blank <- woreda
woreda_blank@data <- woreda@data %>%
  dplyr::select(cell_id)

woreda_blank <- spTransform(woreda_blank, CRS(UTM_ETH))

#### Roads
roads <- readRDS(file.path(rsdp_dir, "RawData", "RoadNetworkPanelData_1996_2016.Rds"))

# Improved roads
roads$improved <- roads$Speed2016 > roads$Speed1996
roads <- roads[roads$improved %in% T,]

# Project
roads <- spTransform(roads, CRS(UTM_ETH))

# Cut Road Areas Out -----------------------------------------------------------
# Buffer using geo.buffer, an accuracy way to deal with distance. Only works on
# points and polylines; not lines. First buffer by very small amount using
# gBuffer to make a polygon, then buffer using geo.buffer
roads_1km_buff <- gBuffer_chunks(sdf = roads, width = 1000, chunk_size = 51)

# 735 736 737
woreda_clean <- lapply(1:nrow(woreda_blank), function(i){
  print(paste0(i, " / ", nrow(woreda_blank)))
  
  woreda_blank_i <- woreda_blank[i,]
  
  ## Cleans up self-intersection issues
  woreda_blank_i <- gBuffer(woreda_blank_i, byid=T, width=0)
  
  roads_1km_buff_i <- raster::intersect(roads_1km_buff, woreda_blank_i)
  
  # If doesn't intersect with any roads, keep whole woreda
  if(is.null(roads_1km_buff_i)){ 
    woreda_blank_i_e <- woreda_blank_i
    
    # If does intersect with roads, cut out the road
  } else{
    woreda_blank_i_e <- erase(woreda_blank_i, roads_1km_buff_i)
    
    # Check if fully removes polygon; if does, use full Woreda
    if(nrow(woreda_blank_i_e) %in% 0){
      woreda_blank_i_e <- woreda_blank_i
    }
    
  }
  
  return(woreda_blank_i_e)
  
}) %>% 
  unlist() %>% # remove NULLs
  do.call(what="rbind")

# Repoject Back to WGS84 -------------------------------------------------------
woreda_blank <- spTransform(woreda_blank, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
woreda_clean <- spTransform(woreda_clean, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Export -----------------------------------------------------------------------
# We save "polygon" and "points" file, where "points" is actually just the polygon.
# We do this to make compatible with some scripts that also process grid data

## Main Files - 1km road cut out
saveRDS(woreda_clean, file.path(panel_rsdp_imp_dir, "woreda", 
                                "individual_datasets", "polygons.Rds"))
saveRDS(woreda_clean, file.path(panel_rsdp_imp_dir, "woreda", 
                                "individual_datasets", "points.Rds"))

## Full Data Files - 1km road not cut of
saveRDS(woreda_blank, file.path(panel_rsdp_imp_dir, "woreda", 
                                "individual_datasets", "polygons_no_road_cut.Rds"))
saveRDS(woreda_blank, file.path(panel_rsdp_imp_dir, "woreda", 
                                "individual_datasets", "points_no_road_cut.Rds"))

## Woreda Info
saveRDS(woreda@data, file.path(panel_rsdp_imp_dir, "woreda", 
                               "individual_datasets", "woreda_details.Rds"))



