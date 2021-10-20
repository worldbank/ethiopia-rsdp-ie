# Create Minimal Spanning Tree

# Load Data --------------------------------------------------------------------
## Points to Connect
woreda_points <- readRDS(file.path(hyp_net_dir, "FinalData", "points_to_connect.Rds"))

# Least Cost Path --------------------------------------------------------------
# https://stackoverflow.com/questions/52601127/r-how-to-find-least-cost-path-through-raster-image

coords <- woreda_points %>% 
  coordinates() %>% 
  as.data.frame() %>%
  dplyr::rename(long = x, lat = y) 
coords$uid <- woreda_points$uid

extract_path_cost <- function(i){
  
  print(i)
  
  coords_i <- coords[i,]
  coords_noti <- coords[(i+1):(nrow(coords)),]
  
  coords_noti_orig <- coords_noti
  coords_noti_orig$lat <- coords_i$lat
  coords_noti_orig$long <- coords_i$long
  
  path_i <- bind_rows(coords_noti, coords_noti_orig) %>%
    filter(uid != i) %>%
    mutate(temp = 1:n()) %>%
    sf::st_as_sf(coords = c("long","lat")) %>% 
    sf::st_set_crs(4326) %>%
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

least_cost_paths_sdf <- lapply(1:(nrow(woreda_points)-1), extract_path_cost) %>% 
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
minimal_spanning_tree_data <- as_data_frame(minimal_spanning_tree)
minimal_spanning_tree_sdf <- least_cost_paths_sdf[least_cost_paths_sdf$edge_uid %in% minimal_spanning_tree_data$edge_uid,]

# Export -----------------------------------------------------------------------
minimal_spanning_tree_sdf@data <- minimal_spanning_tree_sdf@data %>%
  dplyr::mutate(road_id = 1:n()) %>%
  dplyr::select(road_id, cost, origin, dest)

#minimal_spanning_tree_sdf$road_id <- 1:nrow(minimal_spanning_tree_sdf)
#minimal_spanning_tree_sdf <- subset(minimal_spanning_tree_sdf, select=c(road_id,cost,origin,dest))

saveRDS(minimal_spanning_tree_sdf, 
        file.path(hyp_net_dir, "FinalData", "least_euc_distance_path_mst.Rds"))



