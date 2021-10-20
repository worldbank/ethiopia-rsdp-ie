# Figures Illustrating Estimating Travel Time

SEP_ROAD_SHAPEFILES <- T # Use separate road shapefiles
RESOLUTION_KM <- 3
WALKING_SPEED <- 5

# Load Data --------------------------------------------------------------------
eth_adm <- readRDS(file.path(gadm_dir, "RawData", "gadm36_ETH_0_sp.rds"))
eth_adm    <- eth_adm %>% spTransform(CRS(UTM_ETH))

woreda_wgs84 <- readRDS(file.path(panel_rsdp_imp_dir, "woreda", "individual_datasets", 
                                  "polygons_no_road_cut.Rds"))

gpw <- raster(file.path(gpw_dir, "RawData", "gpw-v4-population-density_2000.tif"))
gpw <- gpw %>% crop(woreda_wgs84)

roads    <- readRDS(file.path(rsdp_dir, "RawData", "RoadNetworkPanelData_1996_2016.Rds"))
roads$id <- 1 # useful to have a variable the same for all obs when aggreagting roads later
roads    <- roads %>% spTransform(CRS(UTM_ETH))

# Location with largest population with woreda ---------------------------------
woreda_points <- map_df(1:nrow(woreda_wgs84), function(i){
  
  print(paste(i, "/", nrow(woreda_wgs84)))
  
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
  
}) 

#woreda_points$uid <- 1:nrow(woreda_points)
coordinates(woreda_points) <- ~x+y
crs(woreda_points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
woreda_points$cell_id <- woreda_wgs84$cell_id

# Reproject to Ethiopia Projection ---------------------------------------------
# Reproject to UTM. Better for distance calculations (eg, for setting grid cell size)
woreda_points <- spTransform(woreda_points, UTM_ETH)
woreda <- spTransform(woreda_wgs84, UTM_ETH)

# Crete Raster BaseLayer -------------------------------------------------------
r <- raster(xmn=woreda@bbox[1,1], 
            xmx=woreda@bbox[1,2], 
            ymn=woreda@bbox[2,1], 
            ymx=woreda@bbox[2,2], 
            crs=UTM_ETH, 
            resolution = RESOLUTION_KM*1000)

# Rasters/Transition Objects ---------------------------------------------------
make_raster_transition <- function(year){
  
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
  roads_r_speed <- roads_r
  roads_r[] <- RESOLUTION_KM/roads_r[]
  
  cost_t <- transition(roads_r, function(x) 1/mean(x), directions=8)
  
  return(list(roads_r = roads_r,
              roads_r_speed = roads_r_speed,
              cost_t = cost_t))
}

rt_96 <- make_raster_transition(1996)
r_1996 <- rt_96$roads_r_speed
t_1996 <- rt_96$cost_t

rt_16 <- make_raster_transition(2016)
r_2016 <- rt_16$roads_r_speed
t_2016 <- rt_16$cost_t

r_1996_df <- r_1996 %>%
  coordinates() %>%
  as.data.frame() 
r_1996_df$value <- r_1996[]

r_2016_df <- r_2016 %>%
  coordinates() %>%
  as.data.frame() 
r_2016_df$value <- r_2016[]

pal <- wes_palette("Zissou1", 100, type = "continuous")

# Full Country Raster ----------------------------------------------------------
r_eth_96 <- ggplot() +
  geom_polygon(data = eth_adm,
               aes(x = long, y = lat, group = group,
                   color = "5 (Walking)"),
               fill = "gray20") +
  geom_raster(data = r_1996_df[r_1996_df$value > 5,] , 
              aes(x = x, y = y,
                  fill = value)) +
  labs(title = "1996",
       fill = "Speed\nLimit\n(km/hr)",
       color = NULL) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_color_manual(values = "gray20") +
  scale_fill_gradientn(colours = pal,
                       limits = c(10, 70)) 

r_eth_16 <- ggplot() +
  geom_polygon(data = eth_adm,
               aes(x = long, y = lat, group = group,
                   color = "5 (Walking)"),
               fill = "gray20") +
  geom_raster(data = r_2016_df[r_2016_df$value > 5,] , 
              aes(x = x, y = y,
                  fill = value)) +
  labs(title = "2016",
       fill = "Speed\nLimit\n(km/hr)",
       color = NULL) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_color_manual(values = "gray20") +
  scale_fill_gradientn(colours = pal,
                       limits = c(10, 70)) 

# Example Travel Time Raster ---------------------------------------------------
p_area <- woreda_points[woreda_points$cell_id %in% c(308, 728),] %>% 
  gBuffer(width = 130000, byid=T) %>%
  bbox()

r_1996_c <- crop(r_1996, p_area)
r_2016_c <- crop(r_2016, p_area)

r_1996_c_df <- r_1996_c %>%
  coordinates() %>%
  as.data.frame() 
r_1996_c_df$value <- r_1996_c[]

r_2016_c_df <- r_2016_c %>%
  coordinates() %>%
  as.data.frame() 
r_2016_c_df$value <- r_2016_c[]

eth_adm_c <- eth_adm %>% crop(p_area)

costDistance(t_1996,
             woreda_points[woreda_points$cell_id %in% 308,],
             woreda_points[woreda_points$cell_id %in% 728,])

tt_96 <- shortestPath(t_1996,
                      woreda_points[woreda_points$cell_id %in% 308,],
                      woreda_points[woreda_points$cell_id %in% 728,],
                      output = "SpatialLines")
gLength(tt_96) / 1000
tt_96$id <- 1

costDistance(t_2016,
             woreda_points[woreda_points$cell_id %in% 308,],
             woreda_points[woreda_points$cell_id %in% 728,])
tt_16 <- shortestPath(t_2016,
                      woreda_points[woreda_points$cell_id %in% 308,],
                      woreda_points[woreda_points$cell_id %in% 728,],
                      output = "SpatialLines")
gLength(tt_16) / 1000
tt_16$id <- 1

woreda_points_df <- as.data.frame(woreda_points)

r_eth_96_c <- ggplot() +
  geom_polygon(data = eth_adm_c,
               aes(x = long, y = lat, group = group,
                   color = "5 (Walking)"),
               fill = "gray20") +
  geom_raster(data = r_1996_c_df[r_1996_c_df$value > 5,] , 
              aes(x = x, y = y,
                  fill = value),
              alpha = 0.7) +
  geom_point(data = woreda_points_df[woreda_points_df$cell_id %in% c(308, 728),],
             aes(x = x, y = y),
             color = "green") +
  labs(title = "1996",
       fill = "Speed\nLimit\n(km/hr)",
       color = NULL) +
  geom_path(data = tt_96,
            aes(x = long, y = lat, group = group),
            color = "white",
            size = 0.35) +
  geom_point(data = woreda_points_df[woreda_points_df$cell_id %in% c(308, 728),],
             aes(x = x, y = y),
             pch = 21,
             size=3,
             fill = "green2",
             color = "white") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_color_manual(values = "gray20") +
  scale_fill_gradientn(colours = pal,
                       limits = c(10, 70)) 

r_eth_16_c <- ggplot() +
  geom_polygon(data = eth_adm_c,
               aes(x = long, y = lat, group = group,
                   color = "5 (Walking)"),
               fill = "gray20") +
  geom_raster(data = r_2016_c_df[r_2016_c_df$value > 5,] , 
              aes(x = x, y = y,
                  fill = value),
              alpha = 0.7) +
  geom_path(data = tt_16,
            aes(x = long, y = lat, group = group),
            color = "white",
            size = 0.35) +
  geom_point(data = woreda_points_df[woreda_points_df$cell_id %in% c(308, 728),],
             aes(x = x, y = y),
             pch = 21,
             size=3,
             fill = "green2",
             color = "white") +
  labs(title = "2016",
       fill = "Speed\nLimit\n(km/hr)",
       color = NULL) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_color_manual(values = "gray20") +
  scale_fill_gradientn(colours = pal,
                       limits = c(10, 70)) 

p <- ggarrange(r_eth_96_c,
               r_eth_16_c,
               common.legend = T,
               legend = "right")
ggsave(p, filename = file.path(paper_figures,
                               "tt_example.png"),
       height = 5, width = 6)

p <- ggarrange(r_eth_96,
               r_eth_16,
               common.legend = T,
               legend = "right")
ggsave(p, filename = file.path(paper_figures,
                               "tt_eth_example.png"),
       height = 4, width = 10)


