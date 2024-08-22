# Clean Data
# Kebele

NEAR_CUTOFF <- 5 * 1000
ALL_YEARS_IMPROVED_VAR <- F

# Load Data / Create Dataset Lists ---------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_dir, "kebele", "merged_datasets", "panel_data.Rds"))

# Distance improved road -------------------------------------------------------
names(data) %>% str_subset("distance_improvedroad_speedafter_[:digit:]") %>% sort()
data$distance_improvedroad              <- apply(data[,paste0("distance_improvedroad_speedafter_",c(20,25,30,35,45,50,70,120))], 1, FUN = min_na)
data$distance_improvedroad_50aboveafter <- apply(data[,paste0("distance_improvedroad_speedafter_",c(50,70,120))], 1, FUN = min_na)
data$distance_improvedroad_below50after <- apply(data[,paste0("distance_improvedroad_speedafter_",c(20,25,30,35,45))], 1, FUN = min_na)

names(data) %>% str_subset("distance_improvedroad_speedafter_rand_[:digit:]") %>% sort()
data$distance_improvedroad_rand              <- apply(data[,paste0("distance_improvedroad_speedafter_rand_",c(10,15,20,25,30,35,45,50,70))], 1, FUN = min_na)
data$distance_improvedroad_50aboveafter_rand <- apply(data[,paste0("distance_improvedroad_speedafter_rand_",c(50,70))], 1, FUN = min_na)
data$distance_improvedroad_below50after_rand <- apply(data[,paste0("distance_improvedroad_speedafter_rand_",c(10,15,20,25,30,35,45))], 1, FUN = min_na)

names(data) %>% str_subset("distance_improvedroad_speedafter_randrestrict_[:digit:]") %>% sort()
data$distance_improvedroad_randrestrict              <- apply(data[,paste0("distance_improvedroad_speedafter_randrestrict_",c(10,15,20,25,30,35,45,50,70))], 1, FUN = min_na)
data$distance_improvedroad_50aboveafter_randrestrict <- apply(data[,paste0("distance_improvedroad_speedafter_randrestrict_",c(50,70))], 1, FUN = min_na)
data$distance_improvedroad_below50after_randrestrict <- apply(data[,paste0("distance_improvedroad_speedafter_randrestrict_",c(10,15,20,25,30,35,45))], 1, FUN = min_na)

names(data) %>% str_subset("distance_improvedroad_speedafter_randtreat_[:digit:]") %>% sort()
data$distance_improvedroad_randtreat              <- apply(data[,paste0("distance_improvedroad_speedafter_randtreat_",c(10,15,20,25,30,35,50,70))], 1, FUN = min_na)
data$distance_improvedroad_50aboveafter_randtreat <- apply(data[,paste0("distance_improvedroad_speedafter_randtreat_",c(50,70))], 1, FUN = min_na)
data$distance_improvedroad_below50after_randtreat <- apply(data[,paste0("distance_improvedroad_speedafter_randtreat_",c(10,15,20,25,30,35))], 1, FUN = min_na)

names(data) %>% str_subset("distance_improvedroad_speedafter_p1to3") %>% sort()
data$distance_improvedroad_p1to3              <- apply(data[,paste0("distance_improvedroad_speedafter_p1to3_",c(45,50,70))], 1, FUN = min_na)
data$distance_improvedroad_p1to3_50aboveafter <- apply(data[,paste0("distance_improvedroad_speedafter_p1to3_",c(50,70))], 1, FUN = min_na)
data$distance_improvedroad_p1to3_below50after <- apply(data[,paste0("distance_improvedroad_speedafter_p1to3_",c(45, 45))], 1, FUN = min_na)

names(data) %>% str_subset("distance_improvedroad_speedafter_p4") %>% sort()
data$distance_improvedroad_p4              <- apply(data[,paste0("distance_improvedroad_speedafter_p4_",c(20,25,30,35,45,50,70,120))], 1, FUN = min_na)
data$distance_improvedroad_p4_50aboveafter <- apply(data[,paste0("distance_improvedroad_speedafter_p4_",c(50,70,120))], 1, FUN = min_na)
data$distance_improvedroad_p4_below50after <- apply(data[,paste0("distance_improvedroad_speedafter_p4_",c(20,25,30,35,45))], 1, FUN = min_na)

# Years Since / Post Improved Variables --------------------------------------
data$distance_improvedroad_p4[data$year < 2012] <- NA
data$distance_improvedroad_p4_50aboveafter[data$year < 2012] <- NA
data$distance_improvedroad_p4_below50after[data$year < 2012] <- NA

data$distance_improvedroad_p1to3[data$year >= 2010] <- NA
data$distance_improvedroad_p1to3_50aboveafter[data$year >= 2010] <- NA
data$distance_improvedroad_p1to3_below50after[data$year >= 2010] <- NA

roadimproved_df <- lapply(c("distance_improvedroad", 
                            "distance_improvedroad_50aboveafter", 
                            "distance_improvedroad_below50after",
                            
                            "distance_improvedroad_rand", 
                            "distance_improvedroad_50aboveafter_rand", 
                            "distance_improvedroad_below50after_rand",
                            
                            "distance_improvedroad_randrestrict", 
                            "distance_improvedroad_50aboveafter_randrestrict", 
                            "distance_improvedroad_below50after_randrestrict",
                            
                            "distance_improvedroad_randtreat", 
                            "distance_improvedroad_50aboveafter_randtreat", 
                            "distance_improvedroad_below50after_randtreat",
                            
                            "distance_improvedroad_p1to3",
                            "distance_improvedroad_p1to3_50aboveafter",
                            "distance_improvedroad_p1to3_below50after"),
                          # 
                          # "distance_improvedroad_p4", 
                          # "distance_improvedroad_p4_50aboveafter", 
                          # "distance_improvedroad_p4_below50after"),
                          generate_road_improved_variables, 
                          data, 
                          ALL_YEARS_IMPROVED_VAR,
                          NEAR_CUTOFF,
                          0) %>% 
  bind_cols()
data <- bind_cols(data, roadimproved_df)

# Years Since / Post Improved Variables: Different Road Cut-Offs ---------------
#### 0 to 1km
roadimproved_df <- lapply(c("distance_improvedroad", 
                            "distance_improvedroad_50aboveafter", 
                            "distance_improvedroad_below50after"),
                          generate_road_improved_variables, 
                          data, 
                          ALL_YEARS_IMPROVED_VAR,
                          1000,
                          0) %>% 
  bind_cols()
names(roadimproved_df) <- names(roadimproved_df) %>% paste0("_0to1km")
data <- bind_cols(data, roadimproved_df)

#### 1 to 2km
roadimproved_df <- lapply(c("distance_improvedroad", 
                            "distance_improvedroad_50aboveafter", 
                            "distance_improvedroad_below50after"),
                          generate_road_improved_variables, 
                          data, 
                          ALL_YEARS_IMPROVED_VAR,
                          2000,
                          1000) %>% 
  bind_cols()
names(roadimproved_df) <- names(roadimproved_df) %>% paste0("_1to2km")
data <- bind_cols(data, roadimproved_df)

#### 2 to 3km
roadimproved_df <- lapply(c("distance_improvedroad", 
                            "distance_improvedroad_50aboveafter", 
                            "distance_improvedroad_below50after"),
                          generate_road_improved_variables, 
                          data, 
                          ALL_YEARS_IMPROVED_VAR,
                          3000,
                          2000) %>% 
  bind_cols()
names(roadimproved_df) <- names(roadimproved_df) %>% paste0("_2to3km")
data <- bind_cols(data, roadimproved_df)

#### 3 to 4km
roadimproved_df <- lapply(c("distance_improvedroad", 
                            "distance_improvedroad_50aboveafter", 
                            "distance_improvedroad_below50after"),
                          generate_road_improved_variables, 
                          data, 
                          ALL_YEARS_IMPROVED_VAR,
                          4000,
                          3000) %>% 
  bind_cols()
names(roadimproved_df) <- names(roadimproved_df) %>% paste0("_3to4km")
data <- bind_cols(data, roadimproved_df)

#### 4 to 5km
roadimproved_df <- lapply(c("distance_improvedroad", 
                            "distance_improvedroad_50aboveafter", 
                            "distance_improvedroad_below50after"),
                          generate_road_improved_variables, 
                          data, 
                          ALL_YEARS_IMPROVED_VAR,
                          5000,
                          4000) %>% 
  bind_cols()
names(roadimproved_df) <- names(roadimproved_df) %>% paste0("_4to5km")
data <- bind_cols(data, roadimproved_df)

#### 5 to 10km
roadimproved_df <- lapply(c("distance_improvedroad", 
                            "distance_improvedroad_50aboveafter", 
                            "distance_improvedroad_below50after"),
                          generate_road_improved_variables, 
                          data, 
                          ALL_YEARS_IMPROVED_VAR,
                          5000,
                          0) %>% 
  bind_cols()
names(roadimproved_df) <- names(roadimproved_df) %>% paste0("_0to5km")
data <- bind_cols(data, roadimproved_df)

#### 5 to 10km
roadimproved_df <- lapply(c("distance_improvedroad", 
                            "distance_improvedroad_50aboveafter", 
                            "distance_improvedroad_below50after"),
                          generate_road_improved_variables, 
                          data, 
                          ALL_YEARS_IMPROVED_VAR,
                          10000,
                          5000) %>% 
  bind_cols()
names(roadimproved_df) <- names(roadimproved_df) %>% paste0("_5to10km")
data <- bind_cols(data, roadimproved_df)

#### 10 to 15km
roadimproved_df <- lapply(c("distance_improvedroad", 
                            "distance_improvedroad_50aboveafter", 
                            "distance_improvedroad_below50after"),
                          generate_road_improved_variables, 
                          data, 
                          ALL_YEARS_IMPROVED_VAR,
                          15000,
                          10000) %>% 
  bind_cols()
names(roadimproved_df) <- names(roadimproved_df) %>% paste0("_10to15km")
data <- bind_cols(data, roadimproved_df)

#### 15 to 20km
roadimproved_df <- lapply(c("distance_improvedroad", 
                            "distance_improvedroad_50aboveafter", 
                            "distance_improvedroad_below50after"),
                          generate_road_improved_variables, 
                          data, 
                          ALL_YEARS_IMPROVED_VAR,
                          20000,
                          15000) %>% 
  bind_cols()
names(roadimproved_df) <- names(roadimproved_df) %>% paste0("_15to20km")
data <- bind_cols(data, roadimproved_df)

#### 20 to 25km
roadimproved_df <- lapply(c("distance_improvedroad", 
                            "distance_improvedroad_50aboveafter", 
                            "distance_improvedroad_below50after"),
                          generate_road_improved_variables, 
                          data, 
                          ALL_YEARS_IMPROVED_VAR,
                          25000,
                          20000) %>% 
  bind_cols()
names(roadimproved_df) <- names(roadimproved_df) %>% paste0("_20to25km")
data <- bind_cols(data, roadimproved_df)

#### 250 to 30km
roadimproved_df <- lapply(c("distance_improvedroad", 
                            "distance_improvedroad_50aboveafter", 
                            "distance_improvedroad_below50after"),
                          generate_road_improved_variables, 
                          data, 
                          ALL_YEARS_IMPROVED_VAR,
                          30000,
                          25000) %>% 
  bind_cols()
names(roadimproved_df) <- names(roadimproved_df) %>% paste0("_25to30km")
data <- bind_cols(data, roadimproved_df)


# Road Variables ---------------------------------------------------------------
# Variables for road length above Xkm in each year

#### Remove speed distacne 0 variables
# Speed distnace of 0 means the road didn't exist then, so we remove
data$road_length_0 <- NULL
data$distance_improvedroad_speedbefore_0 <- NULL

#### Road speeds
speeds_vec <- names(data) %>% 
  str_subset("road_length_") %>% 
  str_replace_all("road_length_", "") %>% 
  as.numeric() %>% 
  sort()

#### Create road variables, using speed limits of X and above
for(speed_i in speeds_vec){
  
  ### Road length above speed
  # Length of road with speed limit of X and above
  vars_road_length <- paste0("road_length_", speeds_vec[speeds_vec >= speed_i])
  
  data[[paste0("road_length_",speed_i,"above")]] <- data %>%
    dplyr::select(all_of(vars_road_length)) %>% 
    apply(1, sum_na)
  
  var_label(data[[paste0("road_length_",speed_i,"above")]]) <- 
    paste0("Road length of  ", speed_i, "km/h and above")
  
  ### Minumum distance to road
  # Minimum distance to road with speed limit of X and above
  vars_distance_road <- paste0("distance_road_speed_", speeds_vec[speeds_vec >= speed_i])
  
  data[[paste0("distance_road_speed_",speed_i,"above")]] <- data %>%
    dplyr::select(all_of(vars_distance_road)) %>% 
    apply(1, min, na.rm=T)
  
  var_label(data[[paste0("distance_road_speed_",speed_i,"above")]]) <- 
    paste0("Min distance to road of ", speed_i, "km/h and above")
}

# Compute spatial lags ---------------------------------------------------------
# kebele_sf <- readRDS(file.path(panel_rsdp_imp_dir, "kebele", "individual_datasets", "points.Rds")) %>% 
#   st_as_sf()
# 
# sp_lag_df <- map_df(unique(data$year), function(year_i){
#   print(year_i)
#   
#   data_i <- data[data$year %in% year_i,] %>%
#     dplyr::select(cell_id, 
#                   dmspols_harmon, dmspols_harmon_viirs,
#                   globcover_urban, globcover_urban_sum,
#                   globcover_cropland, globcover_cropland_sum)
#   kebele_data_sf <- kebele_sf %>%
#     left_join(data_i, by = "cell_id")
#   
#   geo <- sf::st_geometry(kebele_data_sf)
#   nb <- st_contiguity(geo)
#   wt <- st_weights(nb, allow_zero = T)
#   
#   kebele_data_sf$dmspols_harmon_splag         <- st_lag(kebele_data_sf$dmspols_harmon,         nb, wt, na_ok = T, allow_zero = T)
#   kebele_data_sf$dmspols_harmon_viirs_splag   <- st_lag(kebele_data_sf$dmspols_harmon_viirs,   nb, wt, na_ok = T, allow_zero = T)
#   kebele_data_sf$globcover_urban_splag        <- st_lag(kebele_data_sf$globcover_urban,        nb, wt, na_ok = T, allow_zero = T)
#   kebele_data_sf$globcover_urban_sum_splag    <- st_lag(kebele_data_sf$globcover_urban_sum,    nb, wt, na_ok = T, allow_zero = T)
#   kebele_data_sf$globcover_cropland_splag     <- st_lag(kebele_data_sf$globcover_cropland,     nb, wt, na_ok = T, allow_zero = T)
#   kebele_data_sf$globcover_cropland_sum_splag <- st_lag(kebele_data_sf$globcover_cropland_sum, nb, wt, na_ok = T, allow_zero = T)
#   kebele_data_sf$year <- year_i
#   
#   kebele_data_df <- kebele_data_sf %>%
#     st_drop_geometry()
#   
#   return(kebele_data_df)
# })
# 
# sp_lag_df <- sp_lag_df %>%
#   dplyr::select(-c(dmspols_harmon,
#                    dmspols_harmon_viirs,
#                    globcover_urban,
#                    globcover_urban_sum,
#                    globcover_cropland,
#                    globcover_cropland_sum))
# 
# data <- data %>%
#   left_join(sp_lag_df, by = c("cell_id", "year"))

# Log Variables ----------------------------------------------------------------
ma_var <- data %>% names() %>% str_subset("^MA_")
for(var in ma_var) data[[paste0(var, "_log")]] <- data[[var]] %>% log()

ntl_var <- data %>% names() %>% str_subset("dmspols|globcover|viirs")
#for(var in ntl_var) data[[paste0(var, "_log")]] <- log(data[[var]] + 1)
for(var in ntl_var) data[[paste0(var, "_ihs")]] <- calc_ihs(data[[var]])
for(var in ntl_var) data[[paste0(var, "_log")]] <- log(data[[var]] + 1)

# Baseline Dependent Variables -------------------------------------------------
data <- data %>%
  dplyr::group_by(cell_id) %>%
  dplyr::mutate(globcover_urban_1996        = globcover_urban[year == 1996],
                globcover_urban_sum_1996    = globcover_urban_sum[year == 1996],
                globcover_urban_sum_ihs_1996    = globcover_urban_sum_ihs[year == 1996],
                
                globcover_cropland_1996     = globcover_cropland[year == 1996],
                globcover_cropland_sum_1996 = globcover_cropland_sum[year == 1996],
                globcover_cropland_sum_ihs_1996    = globcover_cropland_sum_ihs[year == 1996],
                
                dmspols_harmon_1996 = dmspols_harmon[year == 1996],
                dmspols_harmon_ihs_1996 = dmspols_harmon_ihs[year == 1996],
                
                dmspols_harmon_sum2_1996 = dmspols_harmon_sum2[year == 1996],
                dmspols_harmon_sum6_1996 = dmspols_harmon_sum6[year == 1996],
                dmspols_harmon_sum10_1996 = dmspols_harmon_sum10[year == 1996],
                
                
                globcover_urban_2011        = globcover_urban[year == 2011],
                globcover_urban_sum_2011    = globcover_urban_sum[year == 2011],
                globcover_urban_sum_ihs_2011    = globcover_urban_sum_ihs[year == 2011],
                
                globcover_cropland_2011     = globcover_cropland[year == 2011],
                globcover_cropland_sum_2011 = globcover_cropland_sum[year == 2011],
                globcover_cropland_sum_ihs_2011    = globcover_cropland_sum_ihs[year == 2011],
                
                dmspols_harmon_2011 = dmspols_harmon[year == 2011],
                dmspols_harmon_ihs_2011 = dmspols_harmon_ihs[year == 2011]) %>%
  dplyr::ungroup()

# Baseline Variables - MA ------------------------------------------------------
#### 1996
MA_vars <- names(data) %>% str_subset("^MA_")

data_MA_vars <- data[data$year %in% 1996, c("cell_id", MA_vars)]
data_MA_vars <- data_MA_vars %>% rename_at(vars(-cell_id), ~ paste0(., '_1996'))

data <- merge(data, data_MA_vars, by = "cell_id")

#### 2011
MA_vars <- names(data) %>% str_subset("^MA_")

data_MA_vars <- data[data$year %in% 2011, c("cell_id", MA_vars)]
data_MA_vars <- data_MA_vars %>% rename_at(vars(-cell_id), ~ paste0(., '_2011'))

data <- merge(data, data_MA_vars, by = "cell_id")

# NTL Bins/Groups --------------------------------------------------------------

# NTL Bins: 1996
data$dmspols_harmon_1996_bin4 <- data$wor_ntlgroup_4bin

data$dmspols_harmon_1996_bin4_1 <- as.numeric(data$dmspols_harmon_1996_bin4 == 1)
data$dmspols_harmon_1996_bin4_2 <- as.numeric(data$dmspols_harmon_1996_bin4 == 2)
data$dmspols_harmon_1996_bin4_3 <- as.numeric(data$dmspols_harmon_1996_bin4 == 3)
data$dmspols_harmon_1996_bin4_4 <- as.numeric(data$dmspols_harmon_1996_bin4 == 4)

# NTL Bins: 2011
data$dmspols_harmon_2011_bin4 <- data$wor_ntlgroup_2011_4bin

data$dmspols_harmon_2011_bin4_1 <- as.numeric(data$dmspols_harmon_2011_bin4 == 1)
data$dmspols_harmon_2011_bin4_2 <- as.numeric(data$dmspols_harmon_2011_bin4 == 2)
data$dmspols_harmon_2011_bin4_3 <- as.numeric(data$dmspols_harmon_2011_bin4 == 3)
data$dmspols_harmon_2011_bin4_4 <- as.numeric(data$dmspols_harmon_2011_bin4 == 4)

# Baseline NTL quantiles
data$ntl_group      <- data$wor_ntlgroup_2bin
data$ntl_group_2011 <- data$wor_ntlgroup_2011_2bin

# Pretrends Variables ----------------------------------------------------------
data <- data %>%
  dplyr::group_by(cell_id) %>%
  dplyr::mutate(globcover_urban_sum_ihs_pretnd96_92 = globcover_urban_sum_ihs[year == 1996]  - globcover_urban_sum_ihs[year == 1992],
                dmspols_harmon_ihs_pretnd96_92      = dmspols_harmon_ihs[year == 1996] - dmspols_harmon_ihs[year == 1992],
                
                globcover_urban_sum_ihs_pretnd11_07 = globcover_urban_sum_ihs[year == 2011]  - globcover_urban_sum_ihs[year == 2007],
                dmspols_harmon_ihs_pretnd11_07      = dmspols_harmon_ihs[year == 2011] - dmspols_harmon_ihs[year == 2007]) %>%
  dplyr::ungroup()

# Other ------------------------------------------------------------------------
data$far_addis <- as.numeric(data$distance_city_addisababa >= 100*1000)

data$dmspols_harmon_ihs2013 <- data$dmspols_harmon_ihs
data$dmspols_harmon_ihs2013[data$year > 2013] <- NA

# Add lat/lon centroid ---------------------------------------------------------
kebele_sf <- readRDS(file.path(panel_rsdp_imp_dir, "kebele", "individual_datasets", "points.Rds")) %>%
  st_as_sf() 

kebele_coords_df <- kebele_sf %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  dplyr::rename(longitude = X,
                latitude = Y)
kebele_coords_df$cell_id <- kebele_sf$cell_id

data <- data %>%
  left_join(kebele_coords_df, by = "cell_id")

# Export -----------------------------------------------------------------------
saveRDS(data, file.path(panel_rsdp_imp_dir, "kebele", "merged_datasets", "panel_data_clean.Rds"))

