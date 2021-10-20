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
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "kebele", "merged_datasets", "panel_data_clean.Rds"))

data$dmspols_harmon_ihs2013 <- data$dmspols_harmon_ihs
data$dmspols_harmon_ihs2013[data$year >= 2013] <- NA

data <- data %>% group_by(cell_id) %>% mutate(dmspols_1996sum = dmspols_sum[year == 1996])

m <- data$dmspols_1996sum[data$dmspols_1996sum > 0] %>% median(na.rm=T)
data$ntl_group <- NA
data$ntl_group[data$dmspols_1996sum <= m] <- "1"
data$ntl_group[data$dmspols_1996sum > m] <- "2"

# data$ntl_group <- NA
# data$ntl_group[data$dmspols_sum6_1996 == 0] <- "1"
# data$ntl_group[data$dmspols_sum6_1996 > 0] <- "2"

# Estimate Model ---------------------------------------------------------------
results_df <- data.frame(NULL)

for(dep_var in c("dmspols_harmon_ihs",
                 "dmspols_harmon_ihs2013",
                 #"dmspols_zhang_sum2_ihs",
                 #"dmspols_zhang_sum6_ihs",
                 "globcover_cropland_sum_ihs",
                 "globcover_urban_sum_ihs")){
  for(indep_var in c("years_since_improvedroad", "years_since_improvedroad_50aboveafter", "years_since_improvedroad_below50after")){
    
    for(controls in c("+temp_avg+precipitation")){
    #for(controls in c("")){
      
      #for(addis_distance in c("All", "Far")){
      for(addis_distance in c("All", "Far")){
        
        #for(ntl_group in c("All", "1", "2")){
        for(ntl_group in c("All", "1", "2")){
          
          ## Check if exists          
          filename <- paste0(dep_var, "-", indep_var, "-", controls, "-", addis_distance, "-", ntl_group, ".Rds")
          file <- file.path(panel_rsdp_imp_data_file_path, 
                            "kebele", "results_datasets", 
                            "individual_datasets",
                            filename)
          
          ## Update
          print(filename)
          
          if(!file.exists(file) | OVERWRITE_FILES){
            
            ### Prep Data
            data_temp <- data
            if(addis_distance %in% "Far") data_temp <- data_temp[data_temp$far_addis %in% 1,]
            if(ntl_group %in% "1")        data_temp <- data_temp[data_temp$ntl_group %in% "1",]
            if(ntl_group %in% "2")        data_temp <- data_temp[data_temp$ntl_group %in% "2",]

            ### Run model
            results_df_temp <- tryCatch({     
              
              paste(dep_var, "~", indep_var, controls, "| year + cell_id | 0 | W_CODE") %>%
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




