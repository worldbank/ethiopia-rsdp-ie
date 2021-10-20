# Distance to Roads to RSDP 1-3, targetted areas, and MST from targetted areas

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

#### Load RSDP I-III files
rsdp_i_iv_roads <- readRDS(file.path(data_file_path, "RSDP Phase I-IV Roads and Targetted Locations", "FinalData", 
                                      "roads_rsdp_i_iv.Rds"))

targetted_locations <- readRDS(file.path(data_file_path, "RSDP Phase I-IV Roads and Targetted Locations", "FinalData", 
                                      "targetted_locations_unique.Rds"))

##
mst_euc_dist <- readRDS(file.path(data_file_path, "RSDP Phase I-IV Roads and Targetted Locations", "FinalData",
                                         "rsdpi_iv_targetted_loc_eucdist_mst.Rds"))

mst_lc_dist <- readRDS(file.path(data_file_path, "RSDP Phase I-IV Roads and Targetted Locations", "FinalData",
                                  "rsdpi_iv_targetted_loc_leastcost_mst.Rds"))

##
mst_euc_region_dist <- readRDS(file.path(data_file_path, "RSDP Phase I-IV Roads and Targetted Locations", "FinalData",
                                  "rsdpi_iv_targetted_loc_eucdist_mst_region_appended.Rds"))

mst_lc_region_dist <- readRDS(file.path(data_file_path, "RSDP Phase I-IV Roads and Targetted Locations", "FinalData",
                                 "rsdpi_iv_targetted_loc_leastcost_mst_region_appended.Rds"))

#### Project
rsdp_i_iv_roads     <- rsdp_i_iv_roads     %>% spTransform(CRS(UTM_ETH))
targetted_locations <- targetted_locations %>% spTransform(CRS(UTM_ETH))
mst_euc_dist        <- mst_euc_dist        %>% spTransform(CRS(UTM_ETH))
mst_lc_dist         <- mst_lc_dist         %>% spTransform(CRS(UTM_ETH))
mst_euc_region_dist <- mst_euc_region_dist %>% spTransform(CRS(UTM_ETH))
mst_lc_region_dist  <- mst_lc_region_dist  %>% spTransform(CRS(UTM_ETH))

# Buffer points slightly (so can collapse) -------------------------------------
# Buffer by 0.1 meter
targetted_locations <- gBuffer(targetted_locations, byid = T, width = 0.1)

# Aggregate Spatial Units to One Row -------------------------------------------
rsdp_i_iv_roads     <- rsdp_i_iv_roads     %>% st_as_sf() %>% st_combine() %>% as("Spatial")
targetted_locations <- targetted_locations %>% st_as_sf() %>% st_combine() %>% as("Spatial")
mst_euc_dist        <- mst_euc_dist        %>% st_as_sf() %>% st_combine() %>% as("Spatial")
mst_lc_dist         <- mst_lc_dist         %>% st_as_sf() %>% st_combine() %>% as("Spatial")
mst_euc_region_dist <- mst_euc_region_dist %>% st_as_sf() %>% st_combine() %>% as("Spatial")
mst_lc_region_dist  <- mst_lc_region_dist  %>% st_as_sf() %>% st_combine() %>% as("Spatial")

rsdp_i_iv_roads$id    <- 1
targetted_locations$id <- 1
mst_euc_dist$id        <- 1
mst_lc_dist$id         <- 1
mst_euc_region_dist$id <- 1
mst_lc_region_dist$id  <- 1

# Distance to Phases -----------------------------------------------------------
points$distance_rsdp1234                <- gDistance_chunks(points, rsdp_i_iv_roads,     CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)
points$distance_rsdp1234_targettedlocs  <- gDistance_chunks(points, targetted_locations, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)
points$distance_rsdp1234_mst_euc        <- gDistance_chunks(points, mst_euc_dist,        CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)
points$distance_rsdp1234_mst_lc         <- gDistance_chunks(points, mst_lc_dist,         CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)
points$distance_rsdp1234_mst_euc_region <- gDistance_chunks(points, mst_euc_dist,        CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)
points$distance_rsdp1234_mst_lc_region  <- gDistance_chunks(points, mst_lc_dist,         CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)

# Export -----------------------------------------------------------------------
saveRDS(points@data, file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "distance_rsdp_iv_roads_mst_and_targetted_areas.Rds"))
