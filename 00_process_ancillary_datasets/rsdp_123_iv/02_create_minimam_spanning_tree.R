# Create Minimal Spanning Tree

# GADM: Prep Regions -----------------------------------------------------------
eth <- readRDS(file.path(gadm_dir, "RawData", "gadm36_ETH_1_sp.rds"))
eth$NAME_1[eth$NAME_1 %in% "Addis Abeba"] <- "Oromia"
eth$NAME_1[eth$NAME_1 %in% "Dire Dawa"] <- "Oromia"
eth <- raster::aggregate(eth, by = "NAME_1")

# Loop through "All" and regions
for(region in c("All", unique(eth$NAME_1))){
  print(paste(region, "------------------------------------------------------"))
  
  # Load Data --------------------------------------------------------------------
  ## Points to Connect
  points_sdf <- readRDS(file.path(rsdp123_iv_dir, "FinalData", "targetted_locations.Rds"))
  
  if(region != "All"){
    points_sdf <- points_sdf[as.vector(gIntersects(points_sdf, eth[eth$NAME_1 %in% region,], byid=T)),]
  }
  
  ## Elevation/Slope
  elevation <- raster(file.path(elev_dir, "RawData", "eth_elevation_1000m.tif"))
  slope <- terrain(elevation, opt="slope", unit="degrees",neighbors=8)
  
  ## Ethiopia ADM boundary
  eth_buff <- readRDS(file.path(gadm_dir, "RawData", "gadm36_ETH_0_sp.rds")) %>%
    gBuffer(width = 10/111.12)
  
  ## Globcover
  land_cover <- raster(file.path(gc_dir, "RawData", "1992_2015_data", 
                                 "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), 1) %>%
    crop(eth_buff)
  
  land_cover_urban   <- calc(land_cover, function(x) x %in% 190)
  land_cover_wetland <- calc(land_cover, function(x) x %in% 180)
  land_cover_water   <- calc(land_cover, function(x) x %in% 210)
  
  land_cover_urban   <- resample(land_cover_urban, slope)
  land_cover_wetland <- resample(land_cover_wetland, slope)
  land_cover_water   <- resample(land_cover_water, slope)
  
  # To 0/1
  land_cover_urban   <- calc(land_cover_urban, function(x) as.numeric(x > 0))
  land_cover_wetland <- calc(land_cover_wetland, function(x) as.numeric(x > 0))
  land_cover_water   <- calc(land_cover_water, function(x) as.numeric(x > 0))
  
  # Cost Surface -----------------------------------------------------------------
  cost_r <- 1 + slope + 25*land_cover_urban + 25*land_cover_wetland + 25*land_cover_water
  cost_r <- cost_r %>% raster::mask(eth_buff)
  
  # Least Cost Path --------------------------------------------------------------
  # https://stackoverflow.com/questions/52601127/r-how-to-find-least-cost-path-through-raster-image
  cost_t <- transition(cost_r, function(x) 1/mean(x), directions=8)
  
  extract_path_cost <- function(i){
    
    print(i)
    
    path_i <- shortestPath(cost_t, 
                           points_sdf[i,], 
                           points_sdf[(i+1):nrow(points_sdf),], 
                           output = "SpatialLines")
    
    cost_i <- costDistance(cost_t, 
                           points_sdf[i,], 
                           points_sdf[(i+1):nrow(points_sdf),])
    
    path_i$temp <- 1:length(path_i)
    path_i$cost <- cost_i %>% as.vector()
    path_i$origin <- points_sdf$uid[i]
    path_i$dest <- points_sdf$uid[(i+1):nrow(points_sdf)]
    
    return(path_i)
  }
  
  least_cost_paths_sdf <- lapply(1:(nrow(points_sdf)-1), extract_path_cost) %>% 
    do.call(what="rbind")
  least_cost_paths_sdf$edge_uid <- 1:nrow(least_cost_paths_sdf)
  
  # Create minimal spanning tree -----------------------------------------------
  least_cost_paths_network <- readshpnw(least_cost_paths_sdf)
  least_cost_paths_graph <- nel2igraph(least_cost_paths_network[[2]],
                                       least_cost_paths_network[[3]],
                                       weight=least_cost_paths_network[[5]]$cost,
                                       eadf=least_cost_paths_network[[5]])
  minimal_spanning_tree <- mst(least_cost_paths_graph)
  
  # Shapefile of Minimal Spanning Tree -----------------------------------------
  minimal_spanning_tree_data <- igraph::as_data_frame(minimal_spanning_tree)
  minimal_spanning_tree_sdf <- least_cost_paths_sdf[least_cost_paths_sdf$edge_uid %in% minimal_spanning_tree_data$edge_uid,]
  
  # Export -----------------------------------------------------------------------
  minimal_spanning_tree_sdf@data <- minimal_spanning_tree_sdf@data %>%
    dplyr::mutate(road_id = 1:n()) %>%
    dplyr::select(road_id,cost,origin,dest)
  
  #minimal_spanning_tree_sdf$road_id <- 1:nrow(minimal_spanning_tree_sdf)
  #minimal_spanning_tree_sdf <- subset(minimal_spanning_tree_sdf, select=c(road_id,cost,origin,dest))
  
  if(region == "All"){
    saveRDS(minimal_spanning_tree_sdf,
            file.path(rsdp123_iv_dir, "FinalData", 
                      "rsdpi_iii_targetted_loc_leastcost_mst.Rds"))
  } else{
    saveRDS(minimal_spanning_tree_sdf,
            file.path(rsdp123_iv_dir, "FinalData", 
                      "mst_by_region",
                      paste0("rsdpi_iii_targetted_loc_leastcost_mst_",region,".Rds")))
  }
  
}

