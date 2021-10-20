# Create Kebele Level Shapefile

# Create clean woreda level shapefile to merge into. Cut out areas within
# 1km of the road to prevent against capturing affects of just capturing the roads.

# Load Data --------------------------------------------------------------------
#### Kebeles
kebele_no_som <- readOGR(file.path(kebele_dir, "RawData", "0cfabb08-2469-4e3a-8770-6aa5425bc49d2020328-1-lp203n.6k02l.shp"))
#kebele_no_som <- spTransform(kebele_no_som, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

kebele_no_som@data <- kebele_no_som@data %>%
  dplyr::select(R_NAME,
                Z_NAME,
                W_NAME,
                RK_NAME,
                R_CODE,
                Z_CODE,
                W_CODE,
                RK_CODE)

#### Woredas
woreda <- readOGR(file.path(woreda_dir, "RawData", "Ethioworeda.shp"))
woreda <- spTransform(woreda, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

woreda_som <- woreda[woreda$R_NAME %in% "SOMALI REGION",]

woreda_som$RK_NAME <- woreda_som$W_NAME
woreda_som$RK_CODE <- woreda_som$W_CODE

woreda_som@data <- woreda_som@data %>%
  dplyr::select(R_NAME,
                Z_NAME,
                W_NAME,
                RK_NAME,
                R_CODE,
                Z_CODE,
                W_CODE,
                RK_CODE)

#### Append
# The kebele shapefile does not include polygons for the Somali region
kebele <- rbind(kebele_no_som, woreda_som)

#### Adjust
kebele$cell_id <- 1:nrow(kebele)

kebele_blank <- kebele
kebele_blank@data <- kebele@data %>%
  dplyr::select(cell_id)

#### Buffer by 0
# Buffering fixes some issues with polygons
kebele_blank <- kebele_blank %>% gBuffer_chunks(width = 0, chunk_size = 100)

#kebele_blank <- spTransform(kebele_blank, CRS(UTM_ETH))
kebele_clean <- kebele_blank

# #### Roads
# roads <- readRDS(file.path(data_file_path, "RSDP Roads", "FinalData", "RoadNetworkPanelData_1996_2016.Rds"))
# 
# # Improved roads
# roads$improved <- roads$Speed2016 > roads$Speed1996
# roads <- roads[roads$improved %in% T,]
# 
# # Project
# roads <- spTransform(roads, CRS(UTM_ETH))
# 
# # Cut Road Areas Out -----------------------------------------------------------
# roads_1km_buff <- gBuffer_chunks(roads, width=1000, 51)
# 
# # 735 736 737
# woreda_clean <- lapply(1:nrow(woreda_blank), function(i){
#   print(i)
# 
#   woreda_blank_i <- woreda_blank[i,]
#   
#   ## Cleans up self-intersection issues
#   woreda_blank_i <- gBuffer(woreda_blank_i, byid=T, width=0)
#   
#   roads_1km_buff_i <- raster::intersect(roads_1km_buff, woreda_blank_i)
#   
#   # Catch errors in removing roads from polygons. An error occurs when, through
#   # this process, no part of the woreda is left. If that occurs, we return NULL,
#   # so that polygon is excluded
#   woreda_blank_i_e <- woreda_blank_i # default to blank road
#   tryCatch({  
#     
#     # If doesn't intersect with any roads, keep whole woreda
#     if(is.null(roads_1km_buff_i)){ 
#       woreda_blank_i_e <- woreda_blank_i
#       
#     # If does intersect with roads, cut out the road
#     } else{
#       woreda_blank_i_e <- erase(woreda_blank_i, roads_1km_buff_i)
#     }
# 
#     return(woreda_blank_i_e)
#   }, 
#   error = function(e) return(NULL)
#   )
#   
#   return(woreda_blank_i_e)
#   
# }) %>% 
#   unlist() %>% # remove NULLs
#   do.call(what="rbind")

# Repoject Back to WGS84 -------------------------------------------------------
# woreda_blank <- spTransform(kebele_blank, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# woreda_clean <- spTransform(woreda_clean, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Export -----------------------------------------------------------------------
# We save "polygon" and "points" file, where "points" is actually just the polygon.
# We do this to make compatible with some scripts that also process grid data

## Main Files - 1km road cut out
saveRDS(kebele_clean, file.path(panel_rsdp_imp_dir, "kebele", "individual_datasets", "polygons.Rds"))
saveRDS(kebele_clean, file.path(panel_rsdp_imp_dir, "kebele", "individual_datasets", "points.Rds"))

## Full Data Files - 1km road not cut of
saveRDS(kebele_blank, file.path(panel_rsdp_imp_dir, "kebele", "individual_datasets", "polygons_no_road_cut.Rds"))
saveRDS(kebele_blank, file.path(panel_rsdp_imp_dir, "kebele", "individual_datasets", "points_no_road_cut.Rds"))

## Kebele Info
saveRDS(kebele@data %>%
          dplyr::select(cell_id,
                        R_NAME,
                        Z_NAME,
                        W_NAME,
                        RK_NAME,
                        R_CODE,
                        Z_CODE,
                        W_CODE,
                        RK_CODE), file.path(panel_rsdp_imp_dir, 
                                            "kebele", 
                                            "individual_datasets", 
                                            "kebele_details.Rds"))



