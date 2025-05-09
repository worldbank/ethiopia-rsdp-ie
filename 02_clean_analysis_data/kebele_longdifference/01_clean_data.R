# Prep data for long difference
# Kebele

#### Parameters
NEAR_THRESHOLD <- 5*1000

# Dataframe to define baseline/endline combinations. For each combiations,
# makes a different dataframe. 
# - DMSP-OLS until 2012
# - Globcover until 2018
# - Roads until 2016 (for globcover, could use roads in 2016 and globcover in 2018)
base_end_df <- data.frame(baseline = c(1996, 1996, 1996, 1996, 2012, 2013, 2012, 2006, 2006),
                          endline =  c(2012, 2009, 2016, 2018, 2016, 2016, 2018, 2009, 2016))

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_dir, "kebele", "merged_datasets", "panel_data_clean.Rds"))

data <- data[,!grepl("MA_ntl2000_|MA_poplog2000_|MA_gcu2000_|_ic_|_rural33|_rural2|_urban33|_urban2|_urban6|_rural6", names(data))]

# Clean Data -------------------------------------------------------------------
for(i in 1:nrow(base_end_df)){
  print(i)
  
  #### Grab start/end years
  base_year <- base_end_df$baseline[i]
  end_year <- base_end_df$endline[i]
  
  #### First Difference Dataset
  
  ## First difference dataset of time varying variables 
  #a <- data %>%
  #  arrange(desc(year)) %>%
  #  dplyr::filter(year %in% c(base_year, end_year),
  #                cell_id %in% 36)
  #a$MA_pop2000_tt_theta1
  #data_first_diff$MA_pop2000_tt_theta1[data_first_diff$cell_id %in% 36]
  
  data <- data %>%
    group_by(cell_id) %>%
    dplyr::mutate(year_improvedroad_first_imp = min(year[post_improvedroad %in% 1 ])) %>%
    ungroup()
  
  data$year_improvedroad_first_imp[data$year_improvedroad_first_imp %in% Inf] <- NA
  
  data_first_diff <- data %>%
    arrange(year) %>%
    dplyr::filter(year %in% c(base_year, end_year)) %>%
    
    # Assume NA is 0 (for first difference... eg, road length)
    mutate_if(is.numeric, replace_na, replace = 0) %>%
    
    # First difference
    group_by(cell_id) %>%
    summarize_at(names(data) %>% 
                   str_subset("MA|road_length|dmspols|viirs|globcover|viirs|temp|precipitation|ndvi|distance_road_") %>%
                   str_remove_vec(rx = "_1996") %>%
                   str_remove_vec(rx = "_2011") %>%
                   str_remove_vec(rx = "distance_rsdp123") %>%
                   str_remove_vec(rx = "_pretnd96_92") %>%
                   str_remove_vec(rx = "_pretnd11_07"), 
                 diff) 
  
  ## Grab time invariant variables
  data_time_invar <- data %>%
    filter(year %in% base_year) %>%
    dplyr::select(c(contains("_1996"),
                    contains("_2011"),
                    contains("distance_rsdp123"),
                    ends_with("_pretnd96_92"),
                    ends_with("_pretnd11_07"),
                    cell_id, woreda_id, R_CODE, Z_CODE, W_CODE, # Pop2007
                    area_polygon, distance_city_addisababa,
                    distance_elec_trans,
                    latitude, longitude,
                    year_improvedroad_first_imp,
                    wor_ntlgroup_2bin)) 
  
  ## Merge
  data_clean <- merge(data_first_diff, data_time_invar, by = "cell_id")
  
  #### Export
  file_name <- paste0("longdiff_data_clean_base",base_year,"_end",end_year)
  
  saveRDS(data_clean, file.path(panel_rsdp_imp_dir, "kebele", "merged_datasets", 
                                paste0(file_name, ".Rds")))
}

