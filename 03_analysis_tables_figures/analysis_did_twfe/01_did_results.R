# Analysis: Coefficient Each Year - Results

#### Parameters
OVERWRITE_FILES <- F

# Loop through datasets, variables & subsets -----------------------------------
for(dataset in c("kebele", "dmspols_grid_nearroad")){ 
  
  # Define Dependent Variables -------------------------------------------------
  if(dataset %in% "kebele"){
    dep_var_vec <- c("globcover_urban_sum_ihs", "globcover_cropland_sum_ihs", "dmspols_harmon_ihs")
  }           
  
  if(dataset %in% "dmspols_grid_nearroad"){
    dep_var_vec <- c("globcover_urban", "globcover_cropland", "dmspols_harmon_ihs")
  } 
  
  for(dep_var in dep_var_vec){
    for(indep_var in c("year_improvedroad",
                       "year_improvedroad_50aboveafter",
                       "year_improvedroad_below50after")){
      for(addis_distance in c("All", "Far")){
        for(ntl_num_groups in c(2,4)){
          for(controls in c("none", "temp_precip")){ 
            
            if(ntl_num_groups %in% 2) ntl_group_vec <- c("all", "0", "1")
            if(ntl_num_groups %in% 4) ntl_group_vec <- c("all", "1", "2", "3", "4")
            
            for(ntl_group in ntl_group_vec){
              
              # Skip certain subsets ---------------------------------------------
              # For road type, only calculate for all groups
              if((indep_var %in% c("year_improvedroad_50aboveafter", "year_improvedroad_below50after")) &
                 (ntl_group != "all")){
                next
              }
            
              if((dataset == "dmspols_grid_nearroad") & (addis_distance == "Far")){
                next
              }
              
              if((dataset == "dmspols_grid_nearroad") & (ntl_num_groups == 2)){
                next
              }
              
              # Check if model already estimated ---------------------------------
              OUT_PATH_SUFFIX <- paste0(dataset, "_", 
                                        dep_var, "_", 
                                        indep_var, "_", 
                                        controls, "_",
                                        addis_distance, "_numgroups", 
                                        ntl_num_groups, "_group", 
                                        ntl_group)
              
              file_to_check <- file.path(panel_rsdp_imp_dir,
                                         "all_units",
                                         "results_datasets",
                                         "individual_datasets",
                                         paste0("group_did_attgt_",OUT_PATH_SUFFIX, ".Rds"))
              if(OVERWRITE_FILES | !file.exists(file_to_check)){
                
                print(OUT_PATH_SUFFIX)
                
                # Load/Prep Data ---------------------------------------------------
                data <- readRDS(file.path(panel_rsdp_imp_dir, dataset, "merged_datasets", "panel_data_clean.Rds"))
                
                if(ntl_num_groups %in% 2){
                  data$ntl_group <- data$wor_ntlgroup_2bin
                } else{
                  data$ntl_group <- data$wor_ntlgroup_4bin
                }
                
                cluster_var <- "woreda_id"
                
                data$dep_var   <- data[[dep_var]]
                data$indep_var <- data[[indep_var]]
                
                ## Addis subset
                if(addis_distance %in% "Far") data <- data[data$far_addis %in% 1,]
                
                ## If by ntl_group, subset by group
                if(ntl_group %in% "0") data <- data[data$ntl_group %in% 0,]
                if(ntl_group %in% "1") data <- data[data$ntl_group %in% 1,]
                if(ntl_group %in% "2") data <- data[data$ntl_group %in% 2,]
                if(ntl_group %in% "3") data <- data[data$ntl_group %in% 3,]
                if(ntl_group %in% "4") data <- data[data$ntl_group %in% 4,]
                
                ## Subset
                data = data %>%
                  ungroup() %>%
                  dplyr::filter(year >= 1992,
                                year <= 2018,
                                !is.na(indep_var))
                
                # This way of selecting specific variables is robust to some names (ie, woreda_id)
                # not being in all the datasets
                if(controls %in% "none"){
                  data <- data[,names(data) %in% c("dep_var", "indep_var", "cell_id", "year", "woreda_id")]
                  
                } else if (controls %in% "temp_precip"){
                  data <- data[,names(data) %in% c("dep_var", "indep_var", "cell_id", "year", "woreda_id", 
                                                   "temp_avg", "precipitation")]
                  
                  data <- data %>%
                    dplyr::filter(!is.na(temp_avg),
                                  !is.na(precipitation))
                }
                
                # Estimate Model -----------------------------------------------
                if(controls %in% "none"){
                  example_attgt <- att_gt(yname = "dep_var",
                                          tname = "year",
                                          idname = "cell_id",
                                          gname = "indep_var",
                                          xformla = ~1,
                                          data = data,
                                          control_group = "notyettreated",
                                          clustervars = cluster_var,
                                          print_details = T
                  )
                } else if(controls %in% "temp_precip"){
                  example_attgt <- att_gt(yname = "dep_var",
                                          tname = "year",
                                          idname = "cell_id",
                                          gname = "indep_var",
                                          xformla = ~temp_avg+precipitation,
                                          data = data,
                                          control_group = "notyettreated",
                                          clustervars = cluster_var,
                                          print_details = T
                                          
                  )
                }
                
                ## Aggregate ATTs
                agg.simple.dynamic <- aggte(example_attgt, type = "dynamic", na.rm = TRUE)
                p_dynamic <- ggdid(agg.simple.dynamic)
                
                #agg.simple.group <- aggte(example_attgt, type = "group", na.rm = TRUE)
                #p_group <- ggdid(agg.simple.group)
                
                # Save Results -------------------------------------------------
                #### Dynamic
                dynamic_df <- data.frame(time               = agg.simple.dynamic$egt,
                                         att                = agg.simple.dynamic$att.egt,
                                         se                 = agg.simple.dynamic$se.egt,
                                         critical_value_95p = as.numeric(agg.simple.dynamic$crit.val.egt)) %>%
                  mutate(dataset = dataset,
                         dep_var = dep_var,
                         indep_var = indep_var,
                         ntl_group = ntl_group,
                         ntl_num_groups = ntl_num_groups,
                         addis_distance = addis_distance,
                         controls = controls)
                
                saveRDS(dynamic_df, 
                        file.path(panel_rsdp_imp_dir,
                                  "all_units",
                                  "results_datasets",
                                  "individual_datasets",
                                  paste0("dynamic_did_attgt_",OUT_PATH_SUFFIX, ".Rds")))
                
                #### Group
                # group_df <- data.frame(group              = agg.simple.group$egt,
                #                        att                = agg.simple.group$att.egt,
                #                        se                 = agg.simple.group$se.egt,
                #                        critical_value_95p = as.numeric(agg.simple.group$crit.val.egt)) %>%
                #   mutate(dataset = dataset,
                #          dep_var = dep_var,
                #          indep_var = indep_var,
                #          ntl_group = ntl_group,
                #          ntl_num_groups = ntl_num_groups,
                #          addis_distance = addis_distance,
                #          controls = controls)
                # 
                # saveRDS(group_df, 
                #         file.path(panel_rsdp_imp_dir,
                #                   "all_units",
                #                   "results_datasets",
                #                   "individual_datasets",
                #                   paste0("group_did_attgt_",OUT_PATH_SUFFIX, ".Rds")))
                
                ## Cleanup
                rm(example_attgt)
                gc()
              }
            }
          }
        }
      }
    }
  }
}
