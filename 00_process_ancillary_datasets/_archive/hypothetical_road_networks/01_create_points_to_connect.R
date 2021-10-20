# Create Minimal Spanning Tree

# Load Data --------------------------------------------------------------------
## Woredas
woreda <- readOGR(file.path(woreda_dir, "RawData", "Ethioworeda.shp"))
woreda <- spTransform(woreda, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
woreda$uid <- 1:nrow(woreda)

## Ethiopia ADM boundary
# Could use woreda file, but faster to load country-level GADM then buffer. Just
# used for cropping population around Ethiopia boundary (population layer is
# for the world).
eth <- readRDS(file.path(gadm_dir, "RawData", "gadm36_ETH_0_sp.rds")) %>%
  gBuffer(width = 10/111.12)

## Population
gpw <- raster(file.path(gpw_dir, "RawData", "gpw-v4-population-density_2000.tif"))
gpw <- gpw %>% crop(eth)

# Location with largest population with woreda ---------------------------------
woreda_points <- map_df(1:nrow(woreda), function(i){
  print(paste(i, "/", nrow(woreda)))
  
  ## Mask populartion grid around Woreda i
  gpw_i <- gpw %>% 
    crop(woreda[i,]) %>%
    mask(woreda[i,])
  
  ## Convert raster to dataframe
  df <- gpw_i %>% coordinates() %>% as.data.frame()
  df$pop <- gpw_i[]
  
  ## Select location with maximum lat/lon
  loc_df <- df[which.max(df$pop),] %>%
    dplyr::select(x,y)
  
  ## If no location returned above, use centroid
  if(nrow(loc_df) %in% 0){
    loc_df <- coordinates(woreda[i,]) %>%
      as.data.frame() %>%
      dplyr::rename(x= V1,
                    y= V2)
  }
  
  return(loc_df)
  
})

woreda_points$uid <- 1:nrow(woreda_points)
coordinates(woreda_points) <- ~x+y
crs(woreda_points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Export -----------------------------------------------------------------------
saveRDS(woreda_points, 
        file.path(hyp_net_dir, "FinalData", 
                  "points_to_connect.Rds"))


