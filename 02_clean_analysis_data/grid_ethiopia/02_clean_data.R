# Clean Data
# Grid - All Ethiopia

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_dir, "dmspols_grid_ethiopia",
                          "merged_datasets", "panel_data.Rds"))

#data <- data[data$cell_id %in% unique(data$cell_id)[1:10000],] ## for testing

# Create Varibles --------------------------------------------------------------

data <- data %>% 
  dplyr::mutate(dmspols_harmon_2  = as.numeric(dmspols_harmon >= 2),
                dmspols_harmon_6  = as.numeric(dmspols_harmon >= 6),
                dmspols_harmon_10 = as.numeric(dmspols_harmon >= 10),
                
                dmspols_harmon_ihs = calc_ihs(dmspols_harmon),
                dmspols_harmon_log = log(dmspols_harmon+1),
                
                globcover_urban = as.numeric(globcover_urban    > 0),
                globcover_cropland = as.numeric(globcover_cropland > 0),
                
                far_addis = as.numeric(distance_city_addisababa >= 100*1000),
                
                near_anyimproved_ever_5km = as.numeric(distance_anyimproved_ever <= 5*1000),
                #near_anyimproved_by2012_5km = as.numeric(distance_anyimproved_by2012 <= 5*1000),
                
                #near_anyroad2012_5km = as.numeric(distance_anyroad2012 <= 5*1000),
                near_anyroad2016_5km = as.numeric(distance_anyroad2016 <= 5*1000),
                
                #near_mst_5km = as.numeric(distance_mst <= 5*1000),
                #near_mst_mindist_5km = as.numeric(distance_mst_mindist <= 5*1000),
                
                endline = as.numeric(year %in% c(2012, 2016))) %>%
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
  dplyr::ungroup() 

#### Groupigs
## Median
# m <- data$dmspols_harmon_woreda_1996[data$dmspols_harmon_woreda_1996 > 0] %>% median(na.rm=T)
# data$ntl_group <- NA
# data$ntl_group[data$dmspols_harmon_woreda_1996 <= m] <- "1"
# data$ntl_group[data$dmspols_harmon_woreda_1996 > m] <- "2"
data$ntl_group <- data$wor_ntlgroup_2bin

## bin4
data$dmspols_harmon_1996_bin4 <- data$wor_ntlgroup_4bin
# data$dmspols_harmon_1996_bin4[data$dmspols_harmon_sum2_1996_woreda %in% 0] <- 1
# data$dmspols_harmon_1996_bin4[data$dmspols_harmon_sum2_1996_woreda > 0]    <- 2
# data$dmspols_harmon_1996_bin4[data$dmspols_harmon_sum6_1996_woreda > 0]    <- 3
# data$dmspols_harmon_1996_bin4[data$dmspols_harmon_sum10_1996_woreda > 0]   <- 4

data$dmspols_harmon_1996_bin4_1 <- as.numeric(data$dmspols_harmon_1996_bin4 == 1)
data$dmspols_harmon_1996_bin4_2 <- as.numeric(data$dmspols_harmon_1996_bin4 == 2)
data$dmspols_harmon_1996_bin4_3 <- as.numeric(data$dmspols_harmon_1996_bin4 == 3)
data$dmspols_harmon_1996_bin4_4 <- as.numeric(data$dmspols_harmon_1996_bin4 == 4)

# Export -----------------------------------------------------------------------
saveRDS(data, file.path(panel_rsdp_imp_dir, "dmspols_grid_ethiopia",
                        "merged_datasets", "panel_data_clean.Rds"))







