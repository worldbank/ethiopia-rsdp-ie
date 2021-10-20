# Number of Projects Near

MA_var_i <- "MA_pop2000_theta1_log"
time_period <- "All"
addis_distance <- "All"

control_vars <- "+ temp_avg + precipitation"

for(MA_var_i in c("MA_pop2000_theta1_log", "MA_pop2000_theta5_log", "MA_pop2000_theta8_log")){
  for(time_period in c("All", "DMSPOLS", "VIIRS")){
    for(addis_distance in c("All", "Far")){
      
      print(paste(MA_var_i, time_period, addis_distance, "-------------------"))
      
      #### Load Data
      data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data_clean_all.Rds"))
    
      #### Define Independent Variables
      data$MA_var <- data[[MA_var_i]]
      
      # Initial value - demeaned
      if(time_period %in% "VIIRS"){
        data <- data %>%
          group_by(uid) %>%
          mutate(MA_var_uidmean = mean(MA_var, na.rm=T)) %>%
          mutate(MA_var_demean = MA_var - MA_var_uidmean) %>%
          mutate(MA_var_demean_base = MA_var_demean[year == 2012]) %>%
          ungroup() 
      } else{
        data <- data %>%
          group_by(uid) %>%
          mutate(MA_var_uidmean = mean(MA_var, na.rm=T)) %>%
          mutate(MA_var_demean = MA_var - MA_var_uidmean) %>%
          mutate(MA_var_demean_base = MA_var_demean[year == 1996]) %>%
          ungroup() 
      }
      
      data$MA_X_MA_base <- data$MA_var * data$MA_var_demean_base

      vars_keep <- c("MA_var", "MA_X_MA_base")
      vars_name <- c("log(MA)", "log(MA) X Baseline log(MA)")
      
      MA_level <- "MA_var"
      MA_level_base <- "MA_var + MA_X_MA_base"
      
      #### Define NTL Variables
      if(time_period %in% c("All", "DMSPOLS")){
        data$NTL <- data$dmspols_zhang_ihs
        data$NTL_2 <- data$dmspols_zhang_2
        
        NTL_name <- "DMSPOLS (IHS)"
        NTL_2_name <- "DMSPOLS $>$ 2"
        
      } else if (time_period %in% "VIIRS"){
        data$NTL <- data$viirs_mean_ihs
        data$NTL_2 <- data$viirs_mean_2
        
        NTL_name <- "VIIRS (IHS)"
        NTL_2_name <- "VIIRS $>$ 2"
      }
      
      #### Define Data Sample
      if(time_period %in% "DMSPOLS") data <- data[data$year <= 2013,]
      if(time_period %in% "VIIRS")   data <- data[data$year >= 2013,]
      
      if(addis_distance %in% "Far")   data <- data[data$far_addis %in% 1,]
      
      
      #### Models - Levels
      f <- as.formula(paste("NTL ~ ", MA_level, control_vars, "| uid + year | 0 | uid"))
      lm_ntl <- felm(f, data=data)
      
      f <- as.formula(paste("NTL_2 ~ ", MA_level, control_vars, "| uid + year | 0 | uid"))
      lm_ntl_2 <- felm(f, data=data)
      
      f <- as.formula(paste("globcover_urban ~ ", MA_level, control_vars, "| uid + year | 0 | uid"))
      lm_urban <- felm(f, data=data)
      
      f <- as.formula(paste("globcover_cropland ~ ", MA_level, control_vars, "| uid + year | 0 | uid"))
      lm_crop <- felm(f, data=data)
      
      f <- as.formula(paste("ndvi ~ ", MA_level, control_vars, "| uid + year | 0 | uid"))
      lm_ndvi <- felm(f, data=data)
      
      f <- as.formula(paste("ndvi_cropland ~ ", MA_level, control_vars, "| uid + year | 0 | uid"))
      lm_ndvi_cropland <- felm(f, data=data)
      
      #### Models - Baseline Interact
      f <- as.formula(paste("NTL ~ ", MA_level_base, control_vars, "| uid + year | 0 | uid"))
      lm_ntl_base <- felm(f, data=data)
      
      f <- as.formula(paste("NTL_2 ~ ", MA_level_base, control_vars, "| uid + year | 0 | uid"))
      lm_ntl_2_base <- felm(f, data=data)
      
      f <- as.formula(paste("globcover_urban ~ ", MA_level_base, control_vars, "| uid + year | 0 | uid"))
      lm_urban_base <- felm(f, data=data)
      
      f <- as.formula(paste("globcover_cropland ~ ", MA_level_base, control_vars, "| uid + year | 0 | uid"))
      lm_crop_base <- felm(f, data=data)
      
      f <- as.formula(paste("ndvi ~ ", MA_level_base, control_vars, "| uid + year | 0 | uid"))
      lm_ndvi_base <- felm(f, data=data)
      
      f <- as.formula(paste("ndvi_cropland ~ ", MA_level_base, control_vars, "| uid + year | 0 | uid"))
      lm_ndvi_cropland_base <- felm(f, data=data)
      
      #### Stargazer
      stargazer(lm_ntl, lm_ntl_base,
                lm_ntl_2, lm_ntl_2_base,
                lm_urban, lm_urban_base,
                lm_crop, lm_crop_base,
                lm_ndvi, lm_ndvi_base,
                lm_ndvi_cropland, lm_ndvi_cropland_base,
                dep.var.labels.include = T,
                dep.var.caption = "",
                #dep.var.labels = c("NTL","NTL",
                #                   "NTL $>$ 2","NTL $>$ 2",
                #                   "Urban","Urban",
                #                   "Crop","Crop",
                #                   "NDVI","NDVI",
                #                   "NDVI Crop", "NDVI Crop"),
                dep.var.labels = c(NTL_name,
                                   NTL_2_name,
                                   "Urban",
                                   "Crop",
                                   "NDVI",
                                   "NDVI Crop"),
                keep = vars_keep,
                covariate.labels = vars_name,
                omit.stat = c("f","ser"),
                align=TRUE,
                no.space=TRUE,
                float=FALSE,
                column.sep.width="-15pt",
                digits=2,
                add.lines = list(
                  c("Cell FE", rep("Y", 12)),
                  c("Year FE", rep("Y", 12))),
                out = file.path(tables_file_path, paste0("results_MA_MAvar",MA_var_i,
                                                         "_time_period", time_period,
                                                         "_addis",addis_distance,
                                                         ".tex")))
      
      print(paste0("results_MA_MAvar",MA_var_i,
                   "_time_period", time_period,
                   "_addis",addis_distance,
                   ".tex"))
      
      
    }
  }
}



