# Subset Main Dataset to Time Periods

for(road_years_group in c("all",
                          "dmspols",
                          "viirs",
                          "phase1",
                          "phase2",
                          "phase3",
                          "phase4")){
  
  print(paste(road_years_group, "--------------------------------------------"))
  
  data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data_clean.Rds"))
  
  #### Prep Road variables
  # Restrict to roads improved in time period
  # 1. Start with list of all year
  # 2. Vector of years to remove
  # 3. str_replace_all to remove vector of years to remove
  # 4. Remove semicolons that separate years
  # 5. Grab first four characters; this is the first year and year we use
  road_years_i <- road_year[[road_years_group]]
  all_years <- 1996:2016
  road_years_remove <- all_years[!(all_years %in% road_years_i)] %>%
    paste(collapse="|")
  
  # when "all", this is empty and doesnt work in str_replace_all
  if(nchar(road_years_remove) %in% 0) road_years_remove <- "DUMMYTEXT"
  
  #### Years Since Improved & Post Improved
  ## All Roads
  data$improvedroad_year <- data$near_improvedroad_all_years %>%
    str_replace_all(road_years_remove, "") %>%
    str_replace_all(";", "") %>%
    substring(1,4) %>%
    as.numeric()
  data$near_improvedroad_all_years <- NULL
  data$years_since_improvedroad <- data$year - data$improvedroad_year
  data$post_improvedroad <- as.numeric(data$years_since_improvedroad %in% 0:100) # use 0:100, not >0, so NA-->0
  
  data$years_since_improvedroad[data$years_since_improvedroad > 10] <- 10
  data$years_since_improvedroad[data$years_since_improvedroad < -10] <- -10
  
  # Restrict to observations where road improved
  data <- data[!is.na(data$years_since_improvedroad),]
  
  ## <50 Roads
  data$improvedroad_below50after_year <- data$near_improvedroad_below50after_all_years %>%
    str_replace_all(road_years_remove, "") %>%
    str_replace_all(";", "") %>%
    substring(1,4) %>%
    as.numeric()
  data$near_improvedroad_below50after_all_years <- NULL
  data$years_since_improvedroad_below50after <- data$year - data$improvedroad_below50after_year
  data$post_improvedroad_below50after <- as.numeric(data$years_since_improvedroad_below50after %in% 0:100)
  
  data$years_since_improvedroad_below50after[data$years_since_improvedroad_below50after > 10] <- 10
  data$years_since_improvedroad_below50after[data$years_since_improvedroad_below50after < -10] <- -10
  
  
  ## >=50 Roads
  data$improvedroad_50aboveafter_year <- data$near_improvedroad_50aboveafter_all_years %>%
    str_replace_all(road_years_remove, "") %>%
    str_replace_all(";", "") %>%
    substring(1,4) %>%
    as.numeric()
  data$near_improvedroad_50aboveafter_all_years <- NULL
  data$years_since_improvedroad_50aboveafter <- data$year - data$improvedroad_50aboveafter_year
  data$post_improvedroad_50aboveafter <- as.numeric(data$years_since_improvedroad_50aboveafter %in% 0:100)
  
  data$years_since_improvedroad_50aboveafter[data$years_since_improvedroad_50aboveafter > 10] <- 10
  data$years_since_improvedroad_50aboveafter[data$years_since_improvedroad_50aboveafter < -10] <- -10
  
  #### Lagged treatment
  data$pre_improvedroad_neg2_5 <- as.numeric(data$years_since_improvedroad %in% -2:-5) %>% as.numeric()
  data$pre_improvedroad_neg6_10 <- as.numeric(data$years_since_improvedroad %in% -6:-10) %>% as.numeric()
  
  data$pre_improvedroad_50aboveafter_neg2_5 <- as.numeric(data$years_since_improvedroad_50aboveafter %in% -2:-5) %>% as.numeric()
  data$pre_improvedroad_50aboveafter_neg6_10 <- as.numeric(data$years_since_improvedroad_50aboveafter %in% -6:-10) %>% as.numeric()
  
  data$pre_improvedroad_below50after_neg2_5 <- as.numeric(data$years_since_improvedroad_below50after %in% -2:-5) %>% as.numeric()
  data$pre_improvedroad_below50after_neg6_10 <- as.numeric(data$years_since_improvedroad_below50after %in% -6:-10) %>% as.numeric()
  
  #### Years Since a Factor
  # Needs to come after lagged treatment variable construction as that relies
  # on the variable being numeric
  data$years_since_improvedroad <- data$years_since_improvedroad %>% as.factor() %>% relevel("-1")
  data$years_since_improvedroad_below50after <- data$years_since_improvedroad_below50after %>% as.factor() %>% relevel("-1")
  data$years_since_improvedroad_50aboveafter <- data$years_since_improvedroad_50aboveafter %>% as.factor() %>% relevel("-1")
  
  #### Restrict Years of Analysis
  if(road_years_group %in% "viirs"){
    data <- data[data$year %in% 2012:2019,]
  }
  
  if(road_years_group %in% "dmspols"){
    data <- data[data$year %in% 1992:2012,]
  }
  
  #### Remove Variables Not Needed
  # Clean up dataframe so it's smaller to avoid memory issues.
  
  #### Remove individual variables
  data$temp_max <- NULL
  data$temp_min <- NULL
  data$dmspols_1996_ihs <- NULL
  data$dmspols_zhang_1996_ihs <- NULL
  data$dmspols_1996_group <- NULL
  data$dmspols_zhang_1996_group <- NULL
  improvedroad_year <- NULL
  improvedroad_below50after_year <- NULL
  improvedroad_50aboveafter_year <- NULL
  
  #### Remove categories of variables
  vars_delete <- names(data)[grepl("^year_improved|^near_improvedroad|^distance_improved", names(data))]
  for(var in vars_delete){
    data[[var]] <- NULL
  }
  
  #### Export
  saveRDS(data, file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", paste0("grid_data_clean_",road_years_group,".Rds")))
}
