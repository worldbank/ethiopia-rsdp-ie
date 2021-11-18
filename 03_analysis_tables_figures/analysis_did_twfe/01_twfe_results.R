# Analysis: Coefficient Each Year - Results

# Exports dataframe of results, to be used to make figures

#### Parameters
OVERWRITE_FILES <- F

# Loop Over Datasets -----------------------------------------------------------
for(dataset in c("kebele", "dmspols_grid_nearroad")){
  
  # Define Dependent Variables -------------------------------------------------
  if(dataset %in% "kebele"){
    dep_var_vec <- c("globcover_urban_sum_ihs", "globcover_cropland_sum_ihs", "dmspols_harmon_ihs")
  }           
  
  if(dataset %in% "dmspols_grid_nearroad"){
    dep_var_vec <- c("globcover_urban", "globcover_cropland", "dmspols_harmon_ihs")
  } 
  
  # Load Data ------------------------------------------------------------------
  data <- readRDS(file.path(panel_rsdp_imp_dir, dataset, "merged_datasets", "panel_data_clean.Rds"))
  
  for(dep_var in dep_var_vec){
    for(indep_var in c("years_since_improvedroad", "years_since_improvedroad_50aboveafter", "years_since_improvedroad_below50after")){
      for(controls in c("")){ # "+temp_avg+precipitation"
        for(addis_distance in c("All", "Far")){
          for(ntl_num_groups in c(2,4)){
            
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
              
              ## Check if model already estimated ------------------------------  
              filename <- paste0("twowayFE_", 
                                 dataset, "-", 
                                 dep_var, "-", 
                                 indep_var, "-", 
                                 controls, "-", 
                                 addis_distance, "-numgroups",
                                 ntl_num_groups, "-",
                                 ntl_group, ".Rds")
              file <- file.path(panel_rsdp_imp_dir,
                                "all_units", "results_datasets",
                                "individual_datasets",
                                filename)
              
              print(filename)
              
              if(!file.exists(file) | OVERWRITE_FILES){
                
                # Prep Data ----------------------------------------------------
                data_temp <- data
                
                if(ntl_num_groups %in% 2){
                  data_temp$ntl_group <- data_temp$wor_ntlgroup_2bin
                } else{
                  data_temp$ntl_group <- data_temp$wor_ntlgroup_4bin
                }
                
                if(addis_distance %in% "Far") data_temp <- data_temp[data_temp$far_addis %in% 1,]
                if(ntl_group %in% "1")        data_temp <- data_temp[data_temp$ntl_group %in% 1,]
                if(ntl_group %in% "2")        data_temp <- data_temp[data_temp$ntl_group %in% 2,]
                if(ntl_group %in% "3")        data_temp <- data_temp[data_temp$ntl_group %in% 3,]
                if(ntl_group %in% "4")        data_temp <- data_temp[data_temp$ntl_group %in% 4,]
                
                # Run model ----------------------------------------------------
                results_df_temp <- tryCatch({     
                  
                  paste(dep_var, "~", indep_var, controls, "| year + cell_id | 0 | woreda_id") %>%
                    as.formula() %>%
                    felm(data = data_temp) %>%
                    lm_confint_tidy(indep_var)%>%
                    mutate(addis_distance = addis_distance,
                           dep_var = dep_var,
                           ntl_group = ntl_group,
                           ntl_num_groups = ntl_num_groups,
                           controls = controls,
                           indep_var = indep_var,
                           dataset = dataset)
                  
                }, error=function(e) data.frame(NULL))
                
                saveRDS(results_df_temp, file)
              }
              
              
            }
          }
        }
      }
    }
  }
}




