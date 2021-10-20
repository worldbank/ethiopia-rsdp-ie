# Distance to Improved Roads by Speed Limit and Year

# Calculate distance of each point to the closest improved road for each speed 
# limit in each year. Only considers roads improved in the current year. Consequently,
# if createing a variable that indicates if near an improved road, variable would
# could look like: 0 0 1 0 0 1 0 0 (where near improved road in two years). If
# wanted variable that turns and stays on after first time improved, need to further
# clean this variable. In short, only considers year of treatment.

# Uses speed of improved road (so if speed went from 30 to 50, we use 50.)

# Load and Prep Data -----------------------------------------------------------
# Load data and reporject to Ethiopia UTM. UTM better for distance calculations 
# than WGS84.

#### Load points
if(GRID_DATASET){
  points <- readRDS(file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "points.Rds"))
} else{
  points <- readRDS(file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "points_no_road_cut.Rds"))
}
points <- points %>% spTransform(CRS(UTM_ETH))

#### Load roads
roads_sdf <- readRDS(file.path(data_file_path, "RSDP Roads", "FinalData", "RoadNetworkPanelData_1996_2016.Rds"))
roads_sdf$id <- 1 # useful to have a variable the same for all obs when aggreagting roads later
roads_sdf <- roads_sdf %>% spTransform(CRS(UTM_ETH))

# Calculate Distance -----------------------------------------------------------
determine_distance_to_points <- function(year, points, roads){
  
  print("* -------------------------")
  print(year)
  
  # Grab roads for relevant year
  roads_yyyy <- roads[roads[[paste0("Speed",year)]] > 0,]
  
  # Subset to improved roads. Determined improved by whether speed is greater in
  # this year compared to previous year.
  roads_yyyy <- roads_yyyy[roads_yyyy[[paste0("Speed",year)]] > roads_yyyy[[paste0("Speed", year-1 )]],]
  
  # Loop through speeds. Subset road based on that speed. Add that speed to the
  # points dataframe
  for(speed in sort(unique(roads_yyyy[[paste0("Speed", year-1)]]))){
    print("* -------------------------")
    print(paste(speed, year))
    
    roads_subset <- roads_yyyy[roads_yyyy[[paste0("Speed", year-1)]] %in% speed,] #%>% raster::aggregate(by="id")
    roads_subset <- roads_subset %>% st_as_sf() %>% st_combine() %>% as("Spatial")
    roads_subset$id <- 1
    
    points[[paste0("distance_improvedroad_speedbefore_", speed)]] <- gDistance_chunks(points, roads_subset, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS) 
  }
  
  points$year <- year
  
  rm(roads)
  return(points@data)
}

points_all <- lapply(1997:2016, determine_distance_to_points, points, roads_sdf) %>% bind_rows

# Export -----------------------------------------------------------------------
saveRDS(points_all, file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "distance_roads_improved_by_speedlimit_before.Rds"))

