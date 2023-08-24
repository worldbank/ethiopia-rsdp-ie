# Distance to Roads by Speed Limit and Year

# Calculate distance of each point to the closest road for each speed limit in 
# each year.

# Load and Prep Data -----------------------------------------------------------
# Load data and reporject to Ethiopia UTM. UTM better for distance calculations 
# than WGS84.

#### Load points
if(GRID_DATASET){
  points <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "points.Rds"))
} else{
  points <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "points_no_road_cut.Rds"))
}
points <- points %>% spTransform(CRS(UTM_ETH))

#### Load roads
roads_sdf <- readRDS(file.path(rsdp_dir, "RawData", "RoadNetworkPanelData_1996_2016.Rds"))
roads_sdf$id <- 1 # useful to have a variable the same for all obs when aggreagting roads later
roads_sdf <- roads_sdf %>% spTransform(CRS(UTM_ETH))

# Calculate Distance -----------------------------------------------------------
determine_distance_to_points <- function(year, points, roads){
  
  print(paste(year, "========================================================"))
  
  # Grab roads for relevant year. Select if speed limit above 0 (ie, road exists)
  roads_yyyy <- roads[roads[[paste0("Speed",year)]] > 0,]
  
  # Loop through speeds. Subset road based on that speed. Add that speed to the
  # points dataframe
  speed_vec <- sort(unique(roads_yyyy[[paste0("Speed", year)]]))
  for(speed in speed_vec){
    print(paste(speed, "-----------------------------------------------------"))
    
    # Restrict to roads that are speed limit "speed" and aggregate to one row
    roads_subset <- roads_yyyy[roads_yyyy[[paste0("Speed", year)]] %in% speed,] #%>% raster::aggregate(by="id")
    
    roads_subset <- roads_subset %>% st_as_sf() %>% st_combine() %>% as("Spatial")
    roads_subset$id <- 1
    
    points[[paste0("distance_road_speed_", speed)]] <- gDistance_chunks(points, roads_subset, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS) 
  }
  
  points$year <- year
  
  return(points@data)
}

points_p1to3 <- lapply(1996:2016, determine_distance_to_points, points, 
                       roads_sdf[roads_sdf$rsdp_phase %in% 1:3,]) %>% 
  bind_rows()

points_p4 <- lapply(1996:2016, determine_distance_to_points, points, 
                    roads_sdf[roads_sdf$rsdp_phase %in% 4,]) %>% 
  bind_rows()

# Export -----------------------------------------------------------------------
saveRDS(points_all, file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "distance_roads_by_speedlimit.Rds"))

