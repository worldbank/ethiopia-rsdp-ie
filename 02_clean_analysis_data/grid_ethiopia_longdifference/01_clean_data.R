# Clean Data - Long Difference
# Full Ethiopia Grid

#### Parameters
# 0 meters, meaning woreda must intersect
NEAR_THRESHOLD <- 0 

# Dataframe to define baseline/endline combinations. For each combiations,
# makes a different dataframe. 
# - DMSP-OLS until 2012
# - Globcover until 2018
# - Roads until 2016 (for globcover, could use roads in 2016 and globcover in 2018)
base_end_df <- data.frame(baseline = c(1996, 1996, 1996),
                          endline =  c(2012, 2009, 2016))

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_dir, "dmspols_grid_ethiopia",
                          "merged_datasets", "panel_data_clean.Rds"))

data <- data[!is.na(data$woreda_id),]

#data <- data[data$cell_id %in% unique(data$cell_id)[1:5000],] ## for testing

# First Difference -------------------------------------------------------------
for(i in 1:nrow(base_end_df)){
  print(i)
  
  #### Grab start/end years
  base_year <- base_end_df$baseline[i]
  end_year <- base_end_df$endline[i]
  
  #### First Difference Dataset
  ## First difference dataset of time varying variables 
  data_first_diff <- data %>%
    arrange(year) %>%
    filter(year %in% c(base_year, end_year)) %>%
    
    # First difference
    group_by(cell_id) %>%
    summarize_at(names(data) %>% 
                   str_subset("dmspols|globcover|ndvi") %>%
                   str_remove_vec(rx = "_1996"), 
                 diff)
  
  ## Grab time invariant variables
  data_time_invar <- data %>%
    
    filter(year %in% base_year) %>%
    dplyr::select(c(contains("_1996"),
                    contains("distance_rsdp123"),
                    contains("distance_anyimproved"),
                    contains("distance_anyroad"),
                    cell_id, woreda_id, R_CODE, Z_CODE, distance_city_addisababa))
  
  ## Merge
  data_clean <- merge(data_first_diff, data_time_invar, by = "cell_id")
  
  #### Export
  file_name <- paste0("longdiff_data_clean_base",base_year,"_end",end_year)
  
  saveRDS(data_clean, file.path(panel_rsdp_imp_dir, "dmspols_grid_ethiopia", 
                                "merged_datasets", paste0(file_name, ".Rds")))
}







