# Number of Projects Near

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

data$road_length_X_speed_area <- data$road_length_X_speed / data$Area
data$road_length_35over_area <- data$road_length_35over / data$Area
data$road_length_50over_area <- data$road_length_50over / data$Area

#### Log variables
road_density_vars <- names(data)[grepl("road_length", names(data))]
for(var in road_density_vars){
  data[[paste0(var, "_log")]] <- log(data[[var]]+1)
}

control_vars <- "+ temp_avg + precipitation"

data$far_addis <- as.numeric(data$distance_city_addisababa >= 100*1000)

# Ward Density -----------------------------------------------------------------
for(variable in c("road_length_35over_log", "road_length_35over_area_log",
                  "road_length_50over_log", "road_length_50over_area_log",
                  "road_length_X_speed_log", "road_length_X_speed_area_log",
                  
                  "road_length_35over_neigh_withi_log", "road_length_35over_neigh_withi_area_log",
                  "road_length_50over_neigh_withi_log", "road_length_50over_neigh_withi_area_log",
                  "road_length_X_speed_neigh_withi_log", "road_length_X_speed_neigh_withi_area_log",
                  
                  "road_length_35over_neigh_log", "road_length_35over_neigh_area_log",
                  "road_length_50over_neigh_log", "road_length_50over_neigh_area_log",
                  "road_length_X_speed_neigh_log", "road_length_X_speed_neigh_area_log")){
  for(addis_distance in c("All", "Far")){ # "All", "Far"
    for(base_ntl in c(123)){ # 1,2,3,123
      
      data_temp <- data
      
      if(addis_distance %in% "Far"){
        data_temp <- data_temp[data_temp$far_addis %in% 1,]
      }
      
      if(!(base_ntl %in% 123)){
        data_temp <- data_temp[data_temp$dmspols_1996_group_woreda %in% base_ntl,]
      }
      
      ### Main
      density_dmsp <- felm(as.formula(as.formula(paste("dmspols_zhang_ihs ~ ",
                                                       variable,
                                                       control_vars,
                                                       "| uid + year | 0 | uid"))), data=data_temp)
      
      density_urban <- felm(as.formula(as.formula(paste("globcover_urban ~ ",
                                                        variable,
                                                        control_vars,
                                                        "| uid + year | 0 | uid"))), data=data_temp)
      
      density_crop <- felm(as.formula(as.formula(paste("globcover_cropland ~ ",
                                                       variable,
                                                       control_vars,
                                                       "| uid + year | 0 | uid"))), data=data_temp)
      
      density_ndvicrop <- felm(as.formula(as.formula(paste("ndvi_cropland ~ ",
                                                           variable,
                                                           control_vars,
                                                           "| uid + year | 0 | uid"))), data=data_temp)
      
      density_viirs <- felm(as.formula(as.formula(paste("viirs_mean ~ ",
                                                        variable,
                                                        control_vars,
                                                        "| uid + year | 0 | uid"))), data=data_temp)
      
      ### Interact Base NTL
      density_dmsp_baseNTL <- felm(as.formula(as.formula(paste("dmspols_zhang_ihs ~ ",
                                                               variable, " * dmspols_1996_group_woreda",
                                                               " - dmspols_1996_group_woreda",
                                                               control_vars,
                                                               "| uid + year | 0 | uid"))), data=data_temp)
      
      density_urban_baseNTL <- felm(as.formula(as.formula(paste("globcover_urban ~ ",
                                                                variable, " * dmspols_1996_group_woreda",
                                                                " - dmspols_1996_group_woreda",
                                                                control_vars,
                                                                "| uid + year | 0 | uid"))), data=data_temp)
      
      density_crop_baseNTL <- felm(as.formula(as.formula(paste("globcover_cropland ~ ",
                                                               variable, " * dmspols_1996_group_woreda",
                                                               " - dmspols_1996_group_woreda",
                                                               control_vars,
                                                               "| uid + year | 0 | uid"))), data=data_temp)
      
      density_ndvicrop_baseNTL <- felm(as.formula(as.formula(paste("ndvi_cropland ~ ",
                                                                   variable, " * dmspols_1996_group_woreda",
                                                                   " - dmspols_1996_group_woreda",
                                                                   control_vars,
                                                                   "| uid + year | 0 | uid"))), data=data_temp)
      
      density_viirs_baseNTL <- felm(as.formula(as.formula(paste("viirs_mean ~ ",
                                                                variable, " * dmspols_1996_group_woreda",
                                                                " - dmspols_1996_group_woreda",
                                                                control_vars,
                                                                "| uid + year | 0 | uid"))), data=data_temp)
      
      stargazer(density_dmsp, density_dmsp_baseNTL, 
                density_urban, density_urban_baseNTL, 
                density_crop, density_crop_baseNTL, 
                density_ndvicrop, density_ndvicrop_baseNTL, 
                density_viirs, density_viirs_baseNTL,
                dep.var.labels.include = T,
                dep.var.labels = c("DMSP", "DMSP",
                                   "Urban", "Urban",
                                   "Crop", "Crop",
                                   "NDVI-Crop", "NDVI-Crop",
                                   "VIIRS", "VIIRS"),
                keep = c(variable,
                         paste0(variable,":dmspols_1996_group_woreda")),
                covariate.labels = c("Density",
                                     "Denstiy X DMSP Low",
                                     "Denstiy X DMSP High"),
                omit.stat = c("f","ser"),
                align=TRUE,
                no.space=TRUE,
                float=FALSE,
                column.sep.width="-15pt",
                digits=2,
                add.lines = list(
                  c("Cell FE", rep("Y", 10)),
                  c("Year FE", rep("Y", 10))),
                out = file.path(tables_file_path, paste0("results_did_density_",variable,"_addisdistance",addis_distance,"_basentl",base_ntl,".tex")))
      
      print(paste0("results_did_density_",variable,"_addisdistance",addis_distance,"_basentl",base_ntl,".tex"))
      
    }
  }
}

