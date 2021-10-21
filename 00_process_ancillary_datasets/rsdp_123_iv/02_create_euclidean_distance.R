# Create Minimal Spanning Tree

# GADM: Prep Regions -----------------------------------------------------------
eth <- readRDS(file.path(gadm_dir, "RawData", "gadm36_ETH_1_sp.rds"))

# Add cities to region that surrounds them
eth$NAME_1[eth$NAME_1 %in% "Addis Abeba"] <- "Oromia"
eth$NAME_1[eth$NAME_1 %in% "Dire Dawa"] <- "Oromia"
eth <- raster::aggregate(eth, by = "NAME_1")

# Loop through "All" and regions
for(region in c("All", sort(unique(eth$NAME_1)))){
  print(paste(region, "------------------------------------------------------"))
  
  # Load Data ------------------------------------------------------------------
  ## Points to Connect
  points_sdf <- readRDS(file.path(rsdp123_iv_dir, "FinalData", "targetted_locations_unique.Rds"))
  
  if(region != "All"){
    points_sdf <- points_sdf[as.vector(gIntersects(points_sdf, eth[eth$NAME_1 %in% region,], byid=T)),]
  }
  
  points_sdf <- spTransform(points_sdf, CRS(UTM_ETH))
  points_sdf$uid <- 1:nrow(points_sdf)
  
  # Least Cost Path ------------------------------------------------------------
  coords <- points_sdf %>% 
    coordinates() %>% 
    as.data.frame() %>%
    dplyr::rename(long = X1, lat = X2) %>%
    distinct(long, lat, .keep_all = T)
  coords$uid <- points_sdf$uid
  
  extract_path_cost <- function(i){
    
    print(paste(i, "/", nrow(points_sdf), "-", region))
    
    coords_i <- coords[i,]
    coords_noti <- coords[(i+1):(nrow(coords)),]
    
    coords_noti_orig <- coords_noti
    coords_noti_orig$lat <- coords_i$lat
    coords_noti_orig$long <- coords_i$long
    
    path_i <- bind_rows(coords_noti, coords_noti_orig) %>%
      filter(uid != i) %>%
      mutate(temp = 1:n()) %>%
      sf::st_as_sf(coords = c("long","lat")) %>% 
      sf::st_set_crs(20138) %>%
      group_by(uid) %>% 
      dplyr::summarize(m = mean(temp)) %>% 
      st_cast("LINESTRING") %>%
      as("Spatial")
    
    path_i$cost <- gLength(path_i, byid = T) %>% as.numeric()
    
    path_i$origin <- coords$uid[i]
    path_i$dest <- coords$uid[(i+1):nrow(coords)]
    path_i$m <- NULL
    
    return(path_i)
  }
  
  least_cost_paths_sdf <- lapply(1:(nrow(points_sdf)-1), extract_path_cost) %>% do.call(what="rbind")
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
  minimal_spanning_tree_sdf <- least_cost_paths_sdf[least_cost_paths_sdf$edge_uid %in% 
                                                      minimal_spanning_tree_data$edge_uid,]
  
  # Export -----------------------------------------------------------------------
  minimal_spanning_tree_sdf@data <- minimal_spanning_tree_sdf@data %>%
    dplyr::mutate(road_id = 1:n()) %>%
    dplyr::select(road_id,cost,origin,dest)
  
  if(region == "All"){
  saveRDS(minimal_spanning_tree_sdf,
          file.path(rsdp123_iv_dir, "FinalData", 
                    "rsdpi_iii_targetted_loc_eucdist_mst.Rds"))
  } else{
    saveRDS(minimal_spanning_tree_sdf,
            file.path(rsdp123_iv_dir, "FinalData", 
                      "mst_by_region",
                      paste0("rsdpi_iii_targetted_loc_eucdist_mst_",region,".Rds")))
  }
  
}
