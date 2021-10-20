
DATASET_TYPE = "woreda"
set.seed(42)

# Prep Data ====================================================================
# Travel Time

#source("~/Documents/Github/Ethiopia-Corridors-IE/Code/_ethiopia_ie_master.R")

SEP_ROAD_SHAPEFILES <- T # Use separate road shapefiles
RESOLUTION_KM <- 3
WALKING_SPEED <- 5

# Load Data --------------------------------------------------------------------
woreda_wgs84 <- readRDS(file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "polygons_no_road_cut.Rds"))

gpw <- raster(file.path(data_file_path, "Gridded Population of the World", "RawData", "gpw-v4-population-density_2000.tif"))
gpw <- gpw %>% crop(woreda_wgs84)

roads    <- readRDS(file.path(data_file_path, "RSDP Roads", "FinalData", "RoadNetworkPanelData_1996_2016.Rds"))
roads$id <- 1 # useful to have a variable the same for all obs when aggreagting roads later
roads    <- roads %>% spTransform(CRS(UTM_ETH))

# Location with largest population with woreda ---------------------------------
woreda_points <- lapply(1:nrow(woreda_wgs84), function(i){
  print(i)
  
  woreda_wgs84_i <- woreda_wgs84[i,]
  
  gpw_i <- gpw %>% 
    crop(woreda_wgs84_i) %>%
    mask(woreda_wgs84_i)
  
  df_out <- coordinates(woreda_wgs84_i) %>%
    as.data.frame() %>%
    dplyr::rename(x = V1,
                  y = V2)
  df_out$pop <- sum(gpw_i[], na.rm=T)
  
  return(df_out)
  
}) %>% bind_rows()

woreda_points$uid <- woreda_wgs84$uid
coordinates(woreda_points) <- ~x+y
crs(woreda_points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
woreda_points$cell_id <- woreda_wgs84$cell_id

# Reproject to Ethiopia Projection ---------------------------------------------
# Reproject to UTM. Better for distance calculations (eg, for setting grid cell size)
woreda_points <- spTransform(woreda_points, UTM_ETH)
woreda <- spTransform(woreda_wgs84, UTM_ETH)

# Crete Raster BaseLayer -------------------------------------------------------
if(DATASET_TYPE %in% "clusters_of_ntl") woreda <- gBuffer(woreda, width = 3000, byid=T)
r <- raster(xmn=woreda@bbox[1,1], 
            xmx=woreda@bbox[1,2], 
            ymn=woreda@bbox[2,1], 
            ymx=woreda@bbox[2,2], 
            crs=UTM_ETH, 
            resolution = RESOLUTION_KM*1000)

# Cost Surface -----------------------------------------------------------------
year <- 2000

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

# FIGURE =======================================================================

rand_is <- sample(size = 30, x = 1:nrow(woreda_points))

woreda_points$pop_log <- woreda_points$pop / (434197.1/5)

for(i in rand_is){
  
  #i <- 100
  tt1 <- shortestPath(cost_t,
                      woreda_points[i,],
                      woreda_points[-i,],
                      output = "SpatialLines")
  tt1$id <- 1:length(tt1)
  
  woreda_s <- gSimplify(woreda, tol = 5000)
  
  p <- ggplot() +
    geom_polygon(data = woreda_s, aes(x = long, y = lat, group = group),
                 color = "gray90", fill = "gray90") +
    geom_path(data = tt1, aes(x = long, y = lat, group = group)) +
    geom_point(data = as.data.frame(woreda_points[-i,]), 
               aes(x = x, y = y),
               color = "green4",
               fill = "green2",
               pch = 21,
               size = woreda_points$pop_log[-i],
               alpha = 0.8) +
    geom_point(data = as.data.frame(woreda_points[i,]), aes(x = x, y = y),
               color = "red",
               size = 4) +
    theme_void()
  
  ggsave(p, filename = file.path(project_file_path, "Data", "Panel Data RSDP Impacts",
                                 "Outputs",  "market_access_gif", "pngs",  paste0(i, ".png")),
         height = 6, width = 6.5)
}

