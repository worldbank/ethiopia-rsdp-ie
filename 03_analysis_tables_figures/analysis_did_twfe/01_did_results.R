# Analysis: Coefficient Each Year - Results

#### Parameters
OVERWRITE_FILES <- F

dataset = "kebele"
dep_var = "globcover_urban_sum_ihs"
indep_var = "year_improvedroad"
ntl_group = "all"
ntl_num_groups = 4
addis_distance = "All"
controls = "none"
ntl_group = "4"

buff_suffix <- c("_0to5km", "_5to10km", "_10to15km", "_15to20km", "_20to25km", "_25to30km")
buff_suffix_rx <- buff_suffix %>% paste(collapse = "|")

# to_delete <- file.path(panel_rsdp_imp_dir,
#                        "all_units",
#                        "results_datasets",
#                        "individual_datasets") %>%
#   list.files(full.names = T) %>%
#   str_subset("dynamic_did") %>%
#   str_subset("to") %>%
#   str_subset("km")
# for(i in to_delete){
#   file.remove(i)
# }

# file.path(panel_rsdp_imp_dir,
#           "all_units",
#           "results_datasets",
#           "individual_datasets") %>%
#   list.files() %>%
#   str_subset("")

# to_delete <- c(
#   "dynamic_did_attgt_kebele_globcover_urban_sum_ihs_year_improvedroad_none_All_numgroups4_groupall.Rds",
#   "dynamic_did_attgt_kebele_globcover_urban_sum_ihs_year_improvedroad_50aboveafter_none_All_numgroups4_groupall.Rds",
#   "dynamic_did_attgt_kebele_globcover_urban_sum_ihs_year_improvedroad_below50after_none_All_numgroups4_groupall.Rds"
# )
# to_delete <- file.path(panel_rsdp_imp_dir,
#                        "all_units",
#                        "results_datasets",
#                        "individual_datasets",
#                        to_delete)
# for(i in to_delete){
#   file.remove(i)
# }

# Loop through datasets, variables & subsets -----------------------------------
for(dataset in c("kebele")){ # dmspols_grid_nearroad 
  
  # Define Dependent Variables -------------------------------------------------
  if(dataset %in% "kebele"){
    dep_var_vec <- c("globcover_urban_sum_ihs", "globcover_cropland_sum_ihs", "dmspols_harmon_ihs", "dmspols_harmon_viirs_ihs",
                     "globcover_urban_sum_log", "globcover_cropland_sum_log", "dmspols_harmon_log", "dmspols_harmon_viirs_log") # "viirs_bm_ihs", "viirs_bm_log"
  }           
  
  if(dataset %in% "dmspols_grid_nearroad"){
    dep_var_vec <- c("globcover_urban", "globcover_cropland", "dmspols_harmon_ihs", "dmspols_harmon_log")
  } 
  
  for(dep_var in dep_var_vec){
    for(indep_var in c("year_improvedroad",
                       "year_improvedroad_50aboveafter",
                       "year_improvedroad_below50after",
                       
                       paste0("year_improvedroad", buff_suffix),
                       paste0("year_improvedroad_50aboveafter", buff_suffix),
                       paste0("year_improvedroad_below50after", buff_suffix),
                       
                       "year_improvedroad_p1to3",
                       "year_improvedroad_p1to3_50aboveafter",
                       "year_improvedroad_p1to3_below50after",
                       
                       "year_improvedroad_rand", 
                       "year_improvedroad_50aboveafter_rand",
                       "year_improvedroad_below50after_rand",
                       
                       "year_improvedroad_randtreat", 
                       "year_improvedroad_50aboveafter_randtreat",
                       "year_improvedroad_below50after_randtreat",
                       
                       "year_improvedroad_randrestrict",
                       "year_improvedroad_50aboveafter_randrestrict",
                       "year_improvedroad_below50after_randrestrict")){
      for(addis_distance in rev(c("All", "Far"))){ # "All", "Far"
        for(ntl_num_groups in c(2,4)){ # 
          for(controls in c("none")){ # temp_precip, precip, "splag"
            
            if(ntl_num_groups %in% 2) ntl_group_vec <- c("all", "0", "1")
            if(ntl_num_groups %in% 4) ntl_group_vec <- c("all", "1", "2", "3", "4")
            
            for(ntl_group in ntl_group_vec){
              
              # Skip certain subsets ---------------------------------------------
              # For road type, only calculate for all groups
              if( str_detect(indep_var, buff_suffix_rx) & (addis_distance == "Far")  ){
                next
              }
              
              if( str_detect(indep_var, buff_suffix_rx) & (ntl_group != "all")  ){
                next
              }
              
              if( str_detect(indep_var, buff_suffix_rx) & (ntl_num_groups == 2)  ){
                next
              }
              
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
              
              if((dataset == "dmspols_grid_nearroad") & (controls == "splag")){
                next
              }
              
              if((indep_var %>% str_detect("year_improvedroad_p1to3")) & (dep_var %>% str_detect("viirs_bm")) ){
                next
              }
              
              if((indep_var %in% c("year_improvedroad",
                                   "year_improvedroad_50aboveafter",
                                   "year_improvedroad_below50after")) & (dep_var %>% str_detect("viirs_bm")) ){
                next
              }
              
              if( (controls == "splag") & (dep_var == "viirs_bm_ihs") ){
                next
              }
              
              if( (controls == "splag") & (str_detect(indep_var, "_rand")) ){
                next
              }
              
              if( (controls == "splag") & (str_detect(dep_var, "_log")) ){
                next
              }
              
              ### Rand
              if( (str_detect(dep_var, "_log")) & (str_detect(indep_var, "_rand")) ){
                next
              }
              
              if( (addis_distance == "Far") & (str_detect(indep_var, "_rand")) ){
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
                                         paste0("dynamic_did_attgt_",OUT_PATH_SUFFIX, ".Rds"))
              
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
                # dep_var_vec <- c("globcover_urban_sum_ihs", "globcover_cropland_sum_ihs", "dmspols_harmon_ihs", "dmspols_harmon_viirs_ihs", "viirs_bm_ihs",
                #                  "globcover_urban_sum_log", "globcover_cropland_sum_log", "dmspols_harmon_log", "dmspols_harmon_viirs_log", "viirs_bm_log"
                #           
                #                  
                
                if(dep_var %>% str_detect("globcover")) end_year_i <- 2018
                if(dep_var %>% str_detect("dmspols_harmon")) end_year_i <- 2018 # could update to 2021
                if(dep_var %>% str_detect("viirs_bm")) end_year_i <- 2022 # could update to 2021
                
                data = data %>%
                  ungroup() %>%
                  dplyr::filter(year >= 1992,
                                year <= end_year_i,
                                !is.na(indep_var))
                
                # if(indep_var %>% str_detect("year_improvedroad_p1to3")){
                #   data <- data %>%
                #     dplyr::filter(year <= 2009)
                # }
                # 
                # if(indep_var %>% str_detect("year_improvedroad_p4")){
                #   data <- data %>%
                #     dplyr::filter(year >= 2012)
                # }
                
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
                } else if (controls %in% "precip"){
                  data <- data[,names(data) %in% c("dep_var", "indep_var", "cell_id", "year", "woreda_id", 
                                                   "precipitation")]
                  
                  data <- data %>%
                    dplyr::filter(!is.na(precipitation))
                  
                } else if (controls %in% "splag"){
                  
                  dep_var_splag <- dep_var %>%
                    str_replace_all("_ihs", "_splag_ihs") %>%
                    str_replace_all("_log", "_splag_log")
                  
                  data$dep_var_splag <- data[[dep_var_splag]]
                  
                  data <- data[,names(data) %in% c("dep_var", "indep_var", "cell_id", "year", "woreda_id", 
                                                   "dep_var_splag")]
                  
                  data <- data %>%
                    dplyr::filter(!is.na(dep_var_splag))
                  
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
                } else if(controls %in% "precip"){
                  example_attgt <- att_gt(yname = "dep_var",
                                          tname = "year",
                                          idname = "cell_id",
                                          gname = "indep_var",
                                          xformla = ~precipitation,
                                          data = data,
                                          control_group = "notyettreated",
                                          clustervars = cluster_var,
                                          print_details = T
                                          
                  )
                } else if(controls %in% "splag"){
                  example_attgt <- att_gt(yname = "dep_var",
                                          tname = "year",
                                          idname = "cell_id",
                                          gname = "indep_var",
                                          xformla = ~dep_var_splag,
                                          data = data,
                                          control_group = "notyettreated",
                                          clustervars = cluster_var,
                                          print_details = T
                                          
                  )
                }
                
                ## Aggregate ATTs
                agg.simple.dynamic <- aggte(example_attgt, type = "dynamic", na.rm = TRUE)
                
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
                
                #### Wald Test p-value
                # wald_df <- data.frame(wald_pvalue = wald_pvalue) %>%
                #   mutate(dataset = dataset,
                #          dep_var = dep_var,
                #          indep_var = indep_var,
                #          ntl_group = ntl_group,
                #          ntl_num_groups = ntl_num_groups,
                #          addis_distance = addis_distance,
                #          controls = controls)
                # 
                # saveRDS(wald_df, 
                #         file.path(panel_rsdp_imp_dir,
                #                   "all_units",
                #                   "results_datasets",
                #                   "individual_datasets",
                #                   paste0("dynamic_did_attgt_wald_test_pvalue_",OUT_PATH_SUFFIX, ".Rds")))
                
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
                #rm(wald_pvalue)
                #rm(wald_df)
                gc()
              }
            }
          }
        }
      }
    }
  }
}
