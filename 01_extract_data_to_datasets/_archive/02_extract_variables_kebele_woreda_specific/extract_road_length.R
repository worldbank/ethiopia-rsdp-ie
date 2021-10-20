# Extract GADM to Points

# Load Data --------------------------------------------------------------------
#### Load polygons
if(GRID_DATASET){
  polygons <- readRDS(file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "polygons.Rds"))
} else{
  polygons <- readRDS(file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "polygons_no_road_cut.Rds"))
}
polygons <- polygons %>% spTransform(CRS(UTM_ETH))

#### Load roads
roads    <- readRDS(file.path(data_file_path, "RSDP Roads", "FinalData", "RoadNetworkPanelData_1996_2016.Rds"))
roads$id <- 1 # useful to have a variable the same for all obs when aggreagting roads later
roads    <- roads %>% spTransform(CRS(UTM_ETH))

# Reoad Density by Year --------------------------------------------------------
#### Function to calculate road density
calc_road_density_i <- function(i, polygons, roads){
  # Given polygons, determines length of road in polygon i
  
  polygons_i <- polygons[i,]
  polygons_i <- gBuffer(polygons_i, byid=T, width=0) # fix self intersections
  roads_i <- raster::intersect(roads, polygons_i)
  
  if(is.null(roads_i)){
    out <- 0
  } else{
    roads_i$length_km <- gLength(roads_i, byid=T) / 1000
    out <- roads_i$length_km %>% sum()
  }
  
  return(out)
}

calc_road_density <- function(polygons, roads){
  # Given polygons, determines length of road in each polygon
  
  out <- pbmclapply(1:nrow(polygons), 
                    calc_road_density_i,
                    polygons,
                    roads) %>% unlist()
  
  return(out)
}

#### Create dataframe of road length
polygon_roadlength <- lapply(1996:2016, function(year){
  
  print(paste(year, "--------------------------------------------------------"))

  #### Loop through speeds within a year and bind columns to create a dataframe
  speed_vec <- roads[[paste0("Speed", year)]] %>% unique()
  
  df_yyyy <- lapply(speed_vec, function(speed_i){
    
    roads_i <- roads[roads[[paste0("Speed", year)]] %in% speed_i,]
    out <- calc_road_density(polygons, roads_i)
    out_df <- out %>% 
      as.data.frame() 
    names(out_df) <- paste0("road_length_", speed_i)
    
    return(out_df)
  }) %>% bind_cols()
  df_yyyy$year <- year
  df_yyyy$cell_id <- polygons$cell_id
  
  return(df_yyyy)
}) %>% bind_rows()

# Export -----------------------------------------------------------------------
saveRDS(polygon_roadlength, file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "road_length.Rds"))





