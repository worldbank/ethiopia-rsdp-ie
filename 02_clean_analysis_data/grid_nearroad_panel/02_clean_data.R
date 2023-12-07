# Create Varibles for Analysis

# The code implements the following cleaning steps
# 1. Distance to road categories (e.g., min distance to improved road >50km/h)
# 2. Create (1) years since improved and (2) binary improved variables
# 3. Add grouped, lagged treatment (dummy for 2-5 & 6-10 years before treatment)
# 4. Dependent variable transformations
# 5. Other variable transformations
# 6. Woreda level stats (within woreda + near road; not getting full woreda value)
# 7. Remove variables don't need

# The code is memory intensive. To prevent from crashing, break the datasets into
# different chunks, implementing the code on each chunk, the append together.
# Chunk done by woreda and cell, so that an entire woreda must be contained
# fully within a chunk

#### Parameters
NEAR_CUTOFF <- 5 * 1000
ALL_YEARS_IMPROVED_VAR <- F # add variables indicate 2nd and 3rd year of treatment
CHUNK_SIZE <- 200           # number of woredas in each chunk

# Load Data --------------------------------------------------------------------
data_all <- readRDS(file.path(panel_rsdp_imp_dir, "dmspols_grid_nearroad", "merged_datasets", "panel_data.Rds"))

# Basic Cleaning ---------------------------------------------------------------
# Need to do now as take the median value across whole dataset

data_all <- data_all %>% 
  dplyr::mutate(dmspols_harmon_2  = as.numeric(dmspols_harmon >= 2),
                dmspols_harmon_6  = as.numeric(dmspols_harmon >= 6),
                dmspols_harmon_10 = as.numeric(dmspols_harmon >= 10),
                
                dmspols_harmon_ihs = calc_ihs(dmspols_harmon),
                dmspols_harmon_log = log(dmspols_harmon + 1),
                
                globcover_urban = as.numeric(globcover_urban    > 0),
                globcover_cropland = as.numeric(globcover_cropland > 0),
                
                far_addis = as.numeric(distance_city_addisababa >= 100*1000)) %>%
  dplyr::group_by(cell_id) %>%
  dplyr::mutate(dmspols_harmon_2_1996 = dmspols_harmon_2[year == 1996],
                dmspols_harmon_6_1996 = dmspols_harmon_6[year == 1996],
                dmspols_harmon_10_1996 = dmspols_harmon_10[year == 1996],
                
                dmspols_harmon_1996     = dmspols_harmon[year == 1996],
                dmspols_harmon_ihs_1996 = dmspols_harmon_ihs[year == 1996],
                
                globcover_urban_1996   = globcover_urban[year == 1996],
                globcover_cropland_1996 = globcover_cropland[year == 1996]) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(woreda_id) %>% 
  dplyr::mutate(dmspols_harmon_woreda_1996 = mean(dmspols_harmon[year == 1996], na.rm=T),
                dmspols_harmon_sum2_1996_woreda   = sum(dmspols_harmon_2_1996, na.rm = T),
                dmspols_harmon_sum6_1996_woreda   = sum(dmspols_harmon_6_1996, na.rm = T),
                dmspols_harmon_sum10_1996_woreda  = sum(dmspols_harmon_10_1996, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(distance_city_addisababa))

#### Groupigs
## Median
data_all$ntl_group <- data_all$wor_ntlgroup_2bin

## bin4
data_all$dmspols_harmon_1996_bin4 <- data_all$wor_ntlgroup_4bin

#### Other
data_all$dmspols_harmon_ihs2013 <- data_all$dmspols_harmon_ihs
data_all$dmspols_harmon_ihs2013[data_all$year > 2013] <- NA

# Set Up Loop Over Chunks ------------------------------------------------------

## Delete previous temp files
file.path(panel_rsdp_imp_dir, "dmspols_grid_nearroad", "merged_datasets", "temp_datasets") %>%
  list.files(full.names = T) %>%
  lapply(function(file) file.remove(file))

## Determine chunks
cell_id_df <- data_all %>%
  distinct(cell_id, woreda_id)

woreda_ids <- unique(cell_id_df$woreda_id)

start_ids <- seq(from = 1, to = length(woreda_ids), by=CHUNK_SIZE)

for(start_i in start_ids){
  print(paste(start_i, "-", length(start_ids), "-----------------------------"))
  
  ## Subset Data
  end_i <- min(start_i + CHUNK_SIZE - 1, length(woreda_ids))
  woreda_ids_i <- woreda_ids[start_i:end_i]
  
  data <- data_all[data_all$woreda_id %in% woreda_ids_i,]
  
  # Distance to aggregate road categories --------------------------------------
  # We calculate distance to roads by speed limit. Here we calculate distance
  # to any road, road 50 km/h and above and roads less than 50 km/h
  
  ## Distance improved road
  data$distance_improvedroad <- apply(data[,paste0("distance_improvedroad_speedafter_",c(20,25,30,35,45,50,70,120))], 1, FUN = min_na)
  data$distance_improvedroad_50aboveafter <- apply(data[,paste0("distance_improvedroad_speedafter_",c(50,70,120))], 1, FUN = min_na)
  data$distance_improvedroad_below50after <- apply(data[,paste0("distance_improvedroad_speedafter_",c(20,25,30,35,45))], 1, FUN = min_na)
  
  data$distance_improvedroad_speedafter_20 <- NULL
  data$distance_improvedroad_speedafter_25 <- NULL
  data$distance_improvedroad_speedafter_30 <- NULL
  data$distance_improvedroad_speedafter_35 <- NULL
  data$distance_improvedroad_speedafter_45 <- NULL
  data$distance_improvedroad_speedafter_50 <- NULL
  data$distance_improvedroad_speedafter_70 <- NULL
  data$distance_improvedroad_speedafter_120 <- NULL
  
  ## Distance road
  data$distance_road <- apply(data[,paste0("distance_road_speed_",c(10,15,20,25,30,35,45,50,70,120))], 1, FUN = min_na)
  data$distance_road_50above <- apply(data[,paste0("distance_road_speed_",c(50,70,120))], 1, FUN = min_na)
  data$distance_road_below50 <- apply(data[,paste0("distance_road_speed_",c(10,15,20,25,30,35,45))], 1, FUN = min_na)
  
  data$distance_road_speed_10 <- NULL
  data$distance_road_speed_15 <- NULL
  data$distance_road_speed_20 <- NULL
  data$distance_road_speed_25 <- NULL
  data$distance_road_speed_30 <- NULL
  data$distance_road_speed_35 <- NULL
  data$distance_road_speed_45 <- NULL
  data$distance_road_speed_50 <- NULL
  data$distance_road_speed_70 <- NULL
  data$distance_road_speed_120 <- NULL
  
  # Years Since / Post Improved Variables --------------------------------------
  roadimproved_df <- lapply(c("distance_improvedroad", 
                              "distance_improvedroad_50aboveafter", 
                              "distance_improvedroad_below50after"),
                            generate_road_improved_variables, 
                            data, 
                            ALL_YEARS_IMPROVED_VAR,
                            NEAR_CUTOFF) %>% bind_cols()
  data <- bind_cols(data, roadimproved_df)
  
  #"distance_road", 
  #"distance_road_50above", 
  #"distance_road_below50"
  
  # Remove Stuff Don't Need ----------------------------------------------------
  # Reduces dataset size if grid dataset where need to trim size of dataset
  data$distance_city_popsize_3groups_g1 <- NULL
  data$globcover_urban_log <- NULL
  data$globcover_cropland_log <- NULL
  data$dmspols_log <- NULL
  data$viirs_max <- NULL
  data$viirs_mean <- NULL
  data$viirs_mean_2 <- NULL
  data$viirs_mean_6 <- NULL
  data$viirs_median <- NULL
  data$dmspols_harmon_2 <- NULL
  data$dmspols_harmon_6 <- NULL
  data$dmspols_harmon_10 <- NULL
  data$dmspols_harmon_2_1996 <- NULL
  data$dmspols_harmon_6_1996 <- NULL
  data$dmspols_harmon_10_1996 <- NULL
  data$distance_improvedroad <- NULL
  data$distance_improvedroad_50aboveafter <- NULL
  data$distance_improvedroad_below50after <- NULL
  data$distance_road <- NULL
  data$distance_road_50above <- NULL
  data$distance_road_below50 <- NULL
  data$year_road <- NULL
  data$years_since_road <- NULL
  data$post_road <- NULL
  data$year_road_50above <- NULL
  data$years_since_road_50above <- NULL
  data$post_road_50above <- NULL
  data$year_road_below50 <- NULL
  data$years_since_road_below50 <- NULL
  data$post_road_below50 <- NULL
  
  # Export Tmp Data ------------------------------------------------------------
  saveRDS(data, file.path(panel_rsdp_imp_dir, "dmspols_grid_nearroad", "merged_datasets", "temp_datasets", paste0("grid_data_clean_",start_i,".Rds")))
}

# Append Together --------------------------------------------------------------
rm(data)
rm(data_all)
gc(); gc(); gc()

data_append <- file.path(panel_rsdp_imp_dir, "dmspols_grid_nearroad", "merged_datasets", "temp_datasets") %>%
  list.files(full.names = T) %>%
  lapply(function(fpath){
    print(fpath)
    data_i <- readRDS(fpath) %>% data.table()
    
    return(data_i)
  }) %>%
  bind_rows() %>%
  as.data.frame()

# Add lat/lon centroid ---------------------------------------------------------
grid_sf <- readRDS(file.path(panel_rsdp_imp_dir, "dmspols_grid_nearroad", "individual_datasets", "points.Rds")) %>%
  st_as_sf() 

grid_coords_df <- grid_sf %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  dplyr::rename(longitude = X,
                latitude = Y)
grid_coords_df$cell_id <- grid_sf$cell_id

data_append <- data_append %>%
  left_join(grid_coords_df, by = "cell_id")

# Export -----------------------------------------------------------------------
saveRDS(data_append, file.path(panel_rsdp_imp_dir, "dmspols_grid_nearroad", "merged_datasets", "panel_data_clean.Rds"))

