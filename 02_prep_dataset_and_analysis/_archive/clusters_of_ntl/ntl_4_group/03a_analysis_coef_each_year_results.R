# Analysis: Coefficient Each Year - Results

# Exports dataframe of results, to be used to make figures

#### Parameters
OVERWRITE_FILES <- T

#### Default
dep_var <- "globcover_urban"
indep_var <- "years_since_improvedroad"
controls <- ""
addis_distance <- "All"
ntl_group <- "All"

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "merged_datasets", "panel_data_clean.Rds"))

# Estimate Model ---------------------------------------------------------------
results_df <- data.frame(NULL)

for(dep_var in c("dmspols_zhang_ihs",
                 "dmspols_zhang_sum2_ihs",
                 "dmspols_zhang_sum6_ihs",
                 "globcover_urban_sum_ihs",
                 "dmspols_zhang_sum0greater_bin")){
  for(indep_var in c("years_since_improvedroad", "years_since_improvedroad_50aboveafter", "years_since_improvedroad_below50after")){
    
    for(controls in c("", "+temp_avg+precipitation")){
    #for(controls in c("")){
      
      #for(addis_distance in c("All", "Far")){
      for(addis_distance in c("All", "Far")){
        
        #for(ntl_group in c("All", "1", "2")){
        for(ntl_group in c("All", "1", "2", "3", "4")){
          
          ## Check if exists          
          filename <- paste0(dep_var, "-", indep_var, "-", controls, "-", addis_distance, "-", ntl_group, ".Rds")
          file <- file.path(panel_rsdp_imp_data_file_path, 
                            "clusters_of_ntl", "results_datasets", 
                            "individual_datasets4",
                            filename)
          
          ## Update
          print(filename)
          
          if(!file.exists(file) | OVERWRITE_FILES){
            
            ### Prep Data
            data_temp <- data
            if(addis_distance %in% "Far") data_temp <- data_temp[data_temp$far_addis %in% 1,]
            if(ntl_group %in% "1")        data_temp <- data_temp[data_temp$dmspols_1996_bin4 %in% 1,]
            if(ntl_group %in% "2")        data_temp <- data_temp[data_temp$dmspols_1996_bin4 %in% 2,]
            if(ntl_group %in% "3")        data_temp <- data_temp[data_temp$dmspols_1996_bin4 %in% 3,]
            if(ntl_group %in% "4")        data_temp <- data_temp[data_temp$dmspols_1996_bin4 %in% 4,]
            
            ### Run model
            results_df_temp <- tryCatch({     
              
              paste(dep_var, "~", indep_var, controls, "| year + cell_id | 0 | woreda_id") %>%
                as.formula() %>%
                felm(data = data_temp) %>%
                lm_confint_tidy(indep_var)%>%
                mutate(addis_distance = addis_distance,
                       dep_var = dep_var,
                       ntl_group = ntl_group,
                       controls = controls,
                       indep_var = indep_var)
              
            }, error=function(e) data.frame(NULL))
            
            results_df <- bind_rows(results_df,
                                    results_df_temp)
            
            saveRDS(results_df, file)
          }
          
          
        }
      }
    }
  }
}




