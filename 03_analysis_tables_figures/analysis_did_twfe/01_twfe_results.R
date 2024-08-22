# Analysis: Coefficient Each Year - Results

# Exports dataframe of results, to be used to make figures

#### Parameters
OVERWRITE_FILES <- F

dataset = "kebele"
se_type = "conley"
dep_var = "dmspols_harmon_ihs"
indep_var = "years_since_improvedroad"
controls = ""
addis_distance = "All"
ntl_num_groups = 2
ntl_group = "all"

if(OVERWRITE_FILES %in% T){
  to_delete <- file.path(panel_rsdp_imp_dir,
            "all_units", "results_datasets",
            "individual_datasets") %>%
    list.files(pattern = "twowayFE",
               full.names = T)
  for(file_delete_i in to_delete){
    file.remove(file_delete_i)
  }
}

# Loop Over Datasets -----------------------------------------------------------
for(dataset in rev(c("kebele", "dmspols_grid_nearroad"))){ # dmspols_grid_nearroad
  for(se_type in rev(c("conley", "cluster"))){
    
    if(se_type == "cluster") se_type_suffix <- ""
    if(se_type == "conley")  se_type_suffix <- "conley"
    
    if( (dataset == "dmspols_grid_nearroad") & (se_type == "conley") ){
      next
    }
    
    # if( (dataset == "kebele") & (se_type == "cluster") ){
    #   next
    # }
    
    # Define Dependent Variables -------------------------------------------------
    if(dataset %in% "kebele"){
      dep_var_vec <- c("globcover_urban_sum_ihs", "globcover_cropland_sum_ihs", "dmspols_harmon_ihs") # "viirs_bm_ihs"
    }           
    
    if(dataset %in% "dmspols_grid_nearroad"){
      dep_var_vec <- c("globcover_urban", "globcover_cropland", "dmspols_harmon_ihs")
    } 
    
    # Load Data ------------------------------------------------------------------
    data <- readRDS(file.path(panel_rsdp_imp_dir, dataset, "merged_datasets", "panel_data_clean.Rds"))
    
    for(dep_var in dep_var_vec){
      for(indep_var in rev(c("years_since_improvedroad", 
                         "years_since_improvedroad_50aboveafter",
                         "years_since_improvedroad_below50after",
                         
                         "years_since_improvedroad_p1to3", 
                         "years_since_improvedroad_p1to3_50aboveafter",
                         "years_since_improvedroad_p1to3_below50after",
                         
                         "years_since_improvedroad_rand", 
                         "years_since_improvedroad_50aboveafter_rand",
                         "years_since_improvedroad_below50after_rand",
                         
                         "years_since_improvedroad_randrestrict", 
                         "years_since_improvedroad_50aboveafter_randrestrict",
                         "years_since_improvedroad_below50after_randrestrict",
                         
                         "years_since_improvedroad_randtreat", 
                         "years_since_improvedroad_50aboveafter_randtreat",
                         "years_since_improvedroad_below50after_randtreat"))){
        for(controls in c("")){ # "+temp_avg+precipitation"
          for(addis_distance in c("All")){ # "Far"
            for(ntl_num_groups in c(4)){ #2,4
              
              if(ntl_num_groups %in% 2) ntl_group_vec <- c("all", "0", "1")
              if(ntl_num_groups %in% 4) ntl_group_vec <- c("all", "1", "2", "3", "4")
              
              for(ntl_group in ntl_group_vec){
              
                # Skip certain subsets ---------------------------------------------
                if( (dataset == "dmspols_grid_nearroad") & (se_type == "conley") ){
                  next
                }
                
                if( (dataset == "kebele") & (se_type == "cluster") ){
                  next
                }
                
                if( (dataset == "dmspols_grid_nearroad") & str_detect(indep_var, "rand") ){
                  next
                }
                
                # if( (dataset == "dmspols_grid_nearroad") & ntl_num_groups == 2 ){
                #   next
                # }
                
                # For road type, only calculate for all groups
                if((indep_var %in% c("year_improvedroad_50aboveafter", "year_improvedroad_below50after")) &
                   (ntl_group != "all")){
                  next
                }
                
                if((dataset == "dmspols_grid_nearroad") & (addis_distance == "Far")){
                  next
                }
                
                if((indep_var %>% str_detect("distance_improvedroad_p1to3")) & (dep_var %>% str_detect("viirs")) ){
                  next
                }
                
                if((indep_var %in% c("year_improvedroad",
                                     "year_improvedroad_50aboveafter",
                                     "year_improvedroad_below50after")) & (dep_var %>% str_detect("viirs")) ){
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
                                   ntl_group, "-",
                                   se_type_suffix, ".Rds")
                file <- file.path(panel_rsdp_imp_dir,
                                  "all_units", "results_datasets",
                                  "individual_datasets",
                                  filename)
                
                if(!file.exists(file) | OVERWRITE_FILES){
                  
                  print(filename)
                  
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
                  
                  data_temp$dep_var   <- data_temp[[dep_var]]
                  data_temp$indep_var <- data_temp[[indep_var]]
                  
                  # Prep Conley ------------------------------------------------
                  if(se_type == "conley"){
                    
                    results_df_temp <- tryCatch({     
                      
                      feols(dep_var ~ indep_var | year + cell_id, data = data_temp, conley(50)) %>%
                        lm_confint_tidy("indep_var") %>%
                        mutate(addis_distance = addis_distance,
                               dep_var = dep_var,
                               ntl_group = ntl_group,
                               ntl_num_groups = ntl_num_groups,
                               controls = controls,
                               indep_var = indep_var,
                               dataset = dataset,
                               se_type = se_type)
                      
                    }, error=function(e) data.frame(NULL))
                  }
                  
                  # Run model ----------------------------------------------------
                  results_df_temp <- tryCatch({     
                    
                    feols(dep_var ~ indep_var | year + cell_id, data = data_temp, vcov = ~woreda_id) %>%
                      lm_confint_tidy("indep_var") %>%
                      mutate(addis_distance = addis_distance,
                             dep_var = dep_var,
                             ntl_group = ntl_group,
                             ntl_num_groups = ntl_num_groups,
                             controls = controls,
                             indep_var = indep_var,
                             dataset = dataset,
                             se_type = se_type)

                    
                  }, error=function(e) data.frame(NULL))
                  
                  print(nrow(results_df_temp))
                  saveRDS(results_df_temp, file)
                }
                
                
              }
            }
          }
        }
      }
    }
  }
}



