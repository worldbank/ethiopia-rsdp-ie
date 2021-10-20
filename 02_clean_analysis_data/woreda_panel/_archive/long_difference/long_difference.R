# Number of Projects Near

control_vars <- "+ temp_avg + precipitation"

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data_clean_all.Rds"))

#### Add variable
data$road_length_X_speed <- data$road_length_10 * 10 + 
  data$road_length_15 * 15 + 
  data$road_length_20 * 20 + 
  data$road_length_25 * 25 +
  data$road_length_30 * 30 +
  data$road_length_35 * 35 +
  data$road_length_45 * 45 + 
  data$road_length_50 * 50 + 
  data$road_length_70 * 70 + 
  data$road_length_120 * 120 

data_dmspols <- data %>%
  filter(year %in% c(1996, 2012)) %>%
  mutate(endline = as.numeric(year %in% 2012)) 

data_viirs <- data %>%
  filter(year %in% c(2013, 2016)) %>%
  mutate(endline = as.numeric(year %in% 2016)) 

data_full <- data %>%
  filter(year %in% c(1996, 2016)) %>%
  mutate(endline = as.numeric(year %in% 2016)) 

results_df <- data.frame(NULL)

counter <- 1
for(dv in c("viirs_mean_2",
            "viirs_mean_6",
            "viirs_mean_ihs",
            "dmspols_zhang_ihs",
            "dmspols_zhang_2",
            "dmspols_zhang_6",
            "globcover_urban", 
            "globcover_cropland",
            "ndvi", 
            "ndvi_cropland")){
  for(iv in c("road_length_10over", 
              "road_length_15over", 
              "road_length_20over", 
              "road_length_25over", 
              "road_length_30over", 
              "road_length_35over", 
              "road_length_45over", 
              "road_length_50over", 
              "road_length_70over", 
              "road_length_120over", 
              "road_length_X_speed")){
    for(iv_suffix in c("", "_area", "_neigh", "_neigh_area", "_neigh_withi", "_neigh_withi_area")){
      for(addis_dist in c("All", "Far")){
        
        ## Grab dataset
        if (grepl("dmsp", dv)){
          data_temp <- data_dmspols
        } else if (grepl("viirs", dv)){
          data_temp <- data_viirs
        } else {
          data_temp <- data_full
        }
        
        ## Subset to Far from Addis
        if(addis_dist %in% "Far"){
          data_temp <- data_temp[data_temp$far_addis %in% 1,] 
        }
        
        ## _area doesn't exist so add
        if(iv_suffix %in% "_area"){
          data_temp[[paste0(iv, "_area")]] <- data_temp[[iv]] / data_temp$Area
        }
        
        data_temp$road_var     <- log(data_temp[[paste0(iv, iv_suffix)]]+1)
        data_temp$road_var_end <- data_temp$road_var * data_temp$endline
        
        IVs <- "road_var + road_var_end"
        IVs_base <- "road_var + road_var_end + road_var_end*dmspols_1996_group_woreda - dmspols_1996_group_woreda"
        
        #### Level
        f <- as.formula(paste(dv, " ~ ", IVs, control_vars, "| uid + year | 0 | uid"))
        lm <- felm(f, data=data_temp) %>%
          lm_post_confint_tidy %>%
          filter(variable != "temp_avg",
                 variable != "precipitation") %>%
          mutate(dv = dv,
                 iv = iv,
                 addis_dist = addis_dist,
                 iv_suffix = iv_suffix,
                 lm_type = "level")
        
        #### Base
        f_base <- as.formula(paste(dv, " ~ ", IVs_base, control_vars, "| uid + year | 0 | uid"))
        lm_base <- felm(f_base, data=data_temp) %>%
          lm_post_confint_tidy %>%
          filter(variable != "temp_avg",
                 variable != "precipitation") %>%
          mutate(dv = dv,
                 iv = iv,
                 addis_dist = addis_dist,
                 iv_suffix = iv_suffix,
                 lm_type = "ntl_base")
        
        results_df <- bind_rows(results_df, lm)
        results_df <- bind_rows(results_df, lm_base)
        
        if((counter %% 10) == 0) print(counter)
        counter <- counter + 1
        
        
      }
    }
  }
}

saveRDS(results_df, file.path(finaldata_file_path, DATASET_TYPE, "results", "long_diff.Rds"))
