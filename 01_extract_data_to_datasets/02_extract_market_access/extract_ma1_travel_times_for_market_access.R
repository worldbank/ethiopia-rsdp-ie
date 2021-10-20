# Travel Time

SEP_ROAD_SHAPEFILES <- T # Use separate road shapefiles
RESOLUTION_KM <- 3
WALKING_SPEED <- 5

# Load Data --------------------------------------------------------------------
woreda_wgs84 <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", 
                                  "polygons_no_road_cut.Rds"))

gpw <- raster(file.path(gpw_dir, "RawData", "gpw-v4-population-density_2000.tif"))
gpw <- gpw %>% crop(woreda_wgs84)

roads    <- readRDS(file.path(rsdp_dir, "RawData", "RoadNetworkPanelData_1996_2016.Rds"))
roads$id <- 1 # useful to have a variable the same for all obs when aggreagting roads later
roads    <- roads %>% spTransform(CRS(UTM_ETH))

# Location with largest population with woreda ---------------------------------
woreda_points <- lapply(1:nrow(woreda_wgs84), function(i){
  
  print(i)
  
  gpw_i <- gpw %>% 
    crop(woreda_wgs84[i,]) %>%
    mask(woreda_wgs84[i,])
  
  df <- gpw_i %>% coordinates() %>% as.data.frame()
  df$pop <- gpw_i[]
  
  loc_df <- df[which.max(df$pop),] %>%
    dplyr::select(x,y)
  
  if(nrow(loc_df) %in% 0){
    loc_df <- coordinates(woreda_wgs84[i,]) %>%
      as.data.frame() %>%
      dplyr::rename(x= V1,
                    y= V2)
  }
  
  return(loc_df)
  
}) %>% bind_rows()

woreda_points$uid <- woreda_wgs84$uid
coordinates(woreda_points) <- ~x+y
crs(woreda_points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
woreda_points$cell_id <- woreda_wgs84$cell_id

# Reproject to Ethiopia Projection ---------------------------------------------
# Reproject to UTM. Better for distance calculations (eg, for setting grid cell size)
woreda_points <- spTransform(woreda_points, UTM_ETH)
woreda        <- spTransform(woreda_wgs84, UTM_ETH)

# Crete Raster BaseLayer -------------------------------------------------------
if(DATASET_TYPE %in% "clusters_of_ntl") woreda <- gBuffer(woreda, width = 3000, byid=T)
r <- raster(xmn=woreda@bbox[1,1], 
            xmx=woreda@bbox[1,2], 
            ymn=woreda@bbox[2,1], 
            ymx=woreda@bbox[2,2], 
            crs=UTM_ETH, 
            resolution = RESOLUTION_KM*1000)

# Function for Travel Times ----------------------------------------------------
calc_travel_time <- function(year, woreda_points){
  # If SEP_ROAD_SHAPEFILES=T, then "roads" is ignored, as loads roads within
  # the function.
  
  print(paste(year, "--------------------------------------------------------"))

  year_road <- year
  
  speed_var <- paste0("Speed", year_road)
  roads$SpeedYYYY <- roads[[speed_var]]
  roads$SpeedYYYY[roads$SpeedYYYY %in% 0] <- WALKING_SPEED
  
  #### Sort by Speed
  # If multiple polylines interesect with a cell, velox uses the last polygon from
  # the spatial polygons dataframe. Consequently, we sort by speeds from slowest to
  # fastest so that velox uses the fastest speed. 
  roads <- roads[order(roads$SpeedYYYY),] 
  
  #### Rasterize
  roads_r <- r
  roads_r[] <- 0
  roads_r_vx <- velox(roads_r)
  roads_r_vx$rasterize(roads, field="SpeedYYYY", background=WALKING_SPEED) # background should be walking speed (5km/hr); https://en.wikipedia.org/wiki/Preferred_walking_speed
  roads_r <- roads_r_vx$as.RasterLayer()
  
  #### Make Transition Layer
  # Roads is currently speed; calculate how long it takes to move across cell Now, values are the number
  # of hours it takes to cross the cell.
  roads_r[] <- RESOLUTION_KM/roads_r[]
  
  cost_t <- transition(roads_r, function(x) 1/mean(x), directions=8)
  #cost_t <- geoCorrection(cost_t, type="c")
  
  #### Calculate Travel Time for Each Location
  tt_df <- lapply(1:nrow(woreda_points), function(i){
    if((i %% 10) %in% 0) print(i)
    
    tt <- costDistance(cost_t,
                       woreda_points[i,],
                       woreda_points) %>% as.numeric()
    
    df_out <- data.frame(dest_uid = woreda_points$cell_id,
                         travel_time = tt)
    
    df_out$orig_uid <- woreda_points$cell_id[i]
    return(df_out)
  }) %>% bind_rows
  
  tt_df$year <- year
  
  saveRDS(as.data.table(tt_df), 
          file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", paste0("ma1_travel_times_for_market_access_",year,".Rds")))
  
  return(NULL)
}

tmp <- lapply(2016:1996, calc_travel_time, woreda_points)

# Calculate Linear Distance ----------------------------------------------------
distance_df <- lapply(1:nrow(woreda_points), function(i){
  if((i %% 100) %in% 0) print(i)
  
  distance <- gDistance(woreda_points[i,],
                        woreda_points,
                        byid=T) %>% 
    as.vector()
  
  df_out <- data.frame(dest_uid = woreda_points$cell_id,
                       distance = distance)
  
  df_out$orig_uid <- woreda_points$cell_id[i]
  return(df_out)
}) %>% 
  bind_rows %>%
  as.data.table()

saveRDS(distance_df, file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "ma1_travel_times_for_market_access_lineardist.Rds"))






