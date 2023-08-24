# Clean Data
# Woreda

NEAR_CUTOFF <- 0 * 1000
ALL_YEARS_IMPROVED_VAR <- F

# Load Data / Create Dataset Lists ---------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_dir, "woreda", "merged_datasets", "panel_data.Rds"))

# Distance improved road -------------------------------------------------------
#data$distance_improvedroad <- apply(data[,paste0("distance_improvedroad_speedafter_",c(20,25,30,35,45,50,70,120))], 1, FUN = min_na)
#data$distance_improvedroad_50aboveafter <- apply(data[,paste0("distance_improvedroad_speedafter_",c(50,70,120))], 1, FUN = min_na)
#data$distance_improvedroad_below50after <- apply(data[,paste0("distance_improvedroad_speedafter_",c(20,25,30,35,45))], 1, FUN = min_na)

# Years Since / Post Improved Variables --------------------------------------
# roadimproved_df <- lapply(c("distance_improvedroad", 
#                             "distance_improvedroad_50aboveafter", 
#                             "distance_improvedroad_below50after"),
#                           generate_road_improved_variables, 
#                           data, 
#                           ALL_YEARS_IMPROVED_VAR,
#                           NEAR_CUTOFF) %>% bind_cols()
# data <- bind_cols(data, roadimproved_df)

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

# Log Variables ----------------------------------------------------------------
ma_var <- data %>% names() %>% str_subset("^MA_")
for(var in ma_var) data[[paste0(var, "_log")]] <- data[[var]] %>% log()

ntl_var <- data %>% names() %>% str_subset("dmspols|globcover|viirs")
for(var in ntl_var) data[[paste0(var, "_log")]] <- log(data[[var]] + 1)
for(var in ntl_var) data[[paste0(var, "_ihs")]] <- calc_ihs(data[[var]])

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

# Export -----------------------------------------------------------------------
saveRDS(data, file.path(panel_rsdp_imp_dir, "woreda", "merged_datasets", "panel_data_clean.Rds"))

