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

for(dataset_type in c("rsdp_rand_treated", "rsdp_rand", "rsdp_rand_restrict", "rsdp")){
  
  #### Load points
  if(GRID_DATASET){
    points <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "points.Rds"))
  } else{
    points <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "points_no_road_cut.Rds"))
  }
  points <- points %>% spTransform(CRS(UTM_ETH))
  
  #### Load roads
  #roads_sdf <- readRDS(file.path(rsdp_dir, "RawData", "RoadNetworkPanelData_1996_2016.Rds"))
  
  #### Load roads
  if(dataset_type == "rsdp"){
    roads_sdf <- readRDS(file.path(rsdp_dir, "RawData", "RoadNetworkPanelData_1996_2016.Rds"))
    out_suffix <- ""
  }
  
  if(dataset_type == "rsdp_rand"){
    roads_sdf <- readRDS(file.path(rsdp_dir, "FinalData", "RoadNetworkPanelData_1996_2016_rand_year.Rds"))
    out_suffix <- "_rand"
  }
  
  if(dataset_type == "rsdp_rand_restrict"){
    roads_sdf <- readRDS(file.path(rsdp_dir, "FinalData", "RoadNetworkPanelData_1996_2016_rand_year_restrict.Rds"))
    out_suffix <- "_randrestrict"
  }
  
  if(dataset_type == "rsdp_rand_treated"){
    roads_sdf <- readRDS(file.path(rsdp_dir, "FinalData", "RoadNetworkPanelData_1996_2016_rand_year_treated.Rds"))
    out_suffix <- "_randtreated"
  }
  
  roads_sdf$id <- 1 # useful to have a variable the same for all obs when aggreagting roads later
  roads_sdf <- roads_sdf %>% spTransform(CRS(UTM_ETH))
  
  roads_anyyear_sdf <- roads_sdf[roads_sdf$Speed2016 > roads_sdf$Speed1996,]
  
  # Calculate Distance -----------------------------------------------------------
  determine_distance_to_points <- function(year, points, roads){
    
    print(paste(year, "--------------------------------------------------------"))
    
    # Grab roads for relevant year
    roads_yyyy <- roads[roads[[paste0("Speed",year)]] > 0,]
    
    # Subset to improved roads. Determined improved by whether speed is greater in
    # this year compared to previous year.
    roads_yyyy <- roads_yyyy[roads_yyyy[[paste0("Speed",year)]] > roads_yyyy[[paste0("Speed", year-1 )]],]
    
    # Loop through speeds. Subset road based on that speed. Add that speed to the
    # points dataframe
    speed_vec <- sort(unique(roads_yyyy[[paste0("Speed", year)]]))
    for(speed in speed_vec){
      print(paste(speed, year, "-----------------------------------------------"))
      
      roads_subset <- roads_yyyy[roads_yyyy[[paste0("Speed", year)]] %in% speed,] 
      roads_subset <- roads_subset %>% st_as_sf() %>% st_combine() %>% as("Spatial")
      roads_subset$id <- 1
      
      points[[paste0("distance_improvedroad_speedafter_", speed)]] <- gDistance_chunks(points, 
                                                                                       roads_subset, 
                                                                                       CHUNK_SIZE_DIST_ROADS, 
                                                                                       MCCORS_DIST_ROADS) 
    }
    
    points$year <- year
    
    rm(roads)
    return(points@data)
  }
  
  points_all <- lapply(1997:2016, determine_distance_to_points, points, roads_sdf) %>% 
    bind_rows()
  
  points_rsdp1to3 <- lapply(1997:2016, determine_distance_to_points, points, 
                            roads_sdf[roads_sdf$Speed2009 > roads_sdf$Speed1996,]) %>% 
    bind_rows() %>%
    dplyr::rename_at(vars(contains("speedafter")), . %>% str_replace_all("speedafter", "speedafter_p1to3"))
  
  # roads_sdf$Speed2016 > roads_sdf$Speed2009
  points_rsdp4 <- lapply(1997:2016, determine_distance_to_points, points, 
                         roads_sdf[roads_sdf$Speed2016 > roads_sdf$Speed2012,]) %>% 
    bind_rows() %>%
    dplyr::rename_at(vars(contains("speedafter")), . %>% str_replace_all("speedafter", "speedafter_p4"))
  
  points_all <- points_all %>%
    full_join(points_rsdp1to3, by = c("cell_id", "year")) %>%
    full_join(points_rsdp4,    by = c("cell_id", "year"))
  
  # Export -----------------------------------------------------------------------
  saveRDS(points_all, file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", 
                                paste0("distance_roads_improved_by_speedlimit_after",out_suffix,".Rds")))
  
}



