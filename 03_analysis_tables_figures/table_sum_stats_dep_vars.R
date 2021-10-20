# Summary Statistics of Dependent Variables

# Define Helper Functions ------------------------------------------------------

if_na_return <- function(x, 
                         value = "N/A"){
  # If "x" is na, then return "value"
  
  if(is.na(x)){
    x <- value
  }
  
  return(x)
}

if_zero_return <- function(x, 
                           value = "N/A"){
  # If "x" is na, then return "value"
  
  if(x %in% 0){
    x <- value
  }
  
  return(x)
}

# Function for Sum Stats -------------------------------------------------------
make_sum_stats <- function(var,
                           data_full,
                           data_near,
                           data_far,
                           fun,
                           dataset_type,
                           ROUND_NUM_NTL = 2,
                           ROUND_NUM_URBAN = 2){
  # Summary statistics of variable
  # ARGS
  # -- data: Dataset
  # -- variables: variable to summarize
  # -- fun: "mean" or "N_g0" (N_g0 = number greater than zero)
  # -- tex_endline: If should have \\ at the end, not &
  # -- dataset_type: grid or kebele
  
  for(tex_position in c("left", "middle", "right")){
    
    if(tex_position %in% "left")   data <- data_full
    if(tex_position %in% "middle") data <- data_near
    if(tex_position %in% "right")  data <- data_far
    
    if(grepl("dmsp", var) & fun %in% "mean" & dataset_type %in% "dmspols_grid_ethiopia") var_name <- "Avg. NTL"
    if(grepl("dmsp", var) & fun %in% "mean" & dataset_type %in% "kebele") var_name <- "Avg. NTL"
    if(grepl("dmsp", var) & fun %in% "N_g0") var_name <- "NTL"
    
    if(grepl("urban", var) & (fun %in% "mean") & dataset_type %in% "dmspols_grid_ethiopia") var_name <- "Prop. Urban"
    if(grepl("crop", var)  & (fun %in% "mean") & dataset_type %in% "dmspols_grid_ethiopia") var_name <- "Prop. Cropland"
    if(grepl("urban", var) & (fun %in% "mean") & dataset_type %in% "kebele") var_name <- "Avg. Urban Area (km$^2$)"
    if(grepl("crop", var)  & (fun %in% "mean") & dataset_type %in% "kebele") var_name <- "Avg. Cropland Area (km$^2$)"
    
    if(grepl("urban", var) & (fun %in% "N_g0")) var_name <- "Urban"
    if(grepl("crop", var)  & (fun %in% "N_g0")) var_name <- "Cropland"
    
    if(var %in% c("globcover_urban", "globcover_cropland")){
      ROUND_NUM <- ROUND_NUM_URBAN
    } else{
      ROUND_NUM <- ROUND_NUM_NTL
    }
    
    if(fun %in% "mean"){
      value_1996 <- data[[var]][data$year %in% 1996] %>% mean(na.rm = T) %>% round(ROUND_NUM)
      value_2013 <- data[[var]][data$year %in% 2013] %>% mean(na.rm = T) %>% round(ROUND_NUM)
      value_2016 <- data[[var]][data$year %in% 2016] %>% mean(na.rm = T) %>% round(ROUND_NUM) %>% if_na_return()
    }
    
    if(fun %in% "N_g0"){
      value_1996 <- sum(data[[var]][data$year %in% 1996] > 0, na.rm = T) 
      value_2013 <- sum(data[[var]][data$year %in% 2013] > 0, na.rm = T) 
      value_2016 <- sum(data[[var]][data$year %in% 2016] > 0, na.rm = T) %>% if_zero_return()
    }
    
    if(dataset_type %in% "kebele" & grepl("globcover", var) & fun %in% "mean"){
      value_1996 <- (300*value_1996) / 1000
      value_2013 <- (300*value_2013) / 1000
      value_2016 <- (300*value_2016) / 1000
    }
    
    if(tex_position %in% c("right"))          tex_end_txt <- " \\\\ \n"
    if(tex_position %in% c("left", "middle")) tex_end_txt <- " & "
    
    if(tex_position %in% "left"){
      cat(var_name, " & ",
          value_1996 %>% prettyNum(big.mark=",",scientific=FALSE), " & ",
          value_2013 %>% prettyNum(big.mark=",",scientific=FALSE), " & ",
          value_2016 %>% prettyNum(big.mark=",",scientific=FALSE), tex_end_txt)
    } else{
      cat(value_1996 %>% prettyNum(big.mark=",",scientific=FALSE), " & ",
          value_2013 %>% prettyNum(big.mark=",",scientific=FALSE), " & ",
          value_2016 %>% prettyNum(big.mark=",",scientific=FALSE), tex_end_txt)
    }
    
  }
}

# Make Table -------------------------------------------------------------------
for(dataset_type in c("kebele")){
  
  data_full <- readRDS(file.path(panel_rsdp_imp_dir, dataset_type, "merged_datasets", "panel_data_clean.Rds"))
  
  data_near_rd <- data_full[data_full$distance_anyimproved_ever <= 5000,]
  data_far_rd  <- data_full[data_full$distance_anyimproved_ever > 5000,]
  
  if(dataset_type %in% "kebele"){
    data_name <- "Kebeles"
    vars_to_summarise <- c("dmspols_harmon",
                           "globcover_urban_sum",
                           "globcover_cropland_sum")

  } else{
    data_name <- "1x1km Grids"
    vars_to_summarise <- c("dmspols_harmon",
                           "globcover_urban",
                           "globcover_cropland")
  }
  
  sink(file.path(paper_tables,
                 paste0("depvar_sumstats_",dataset_type,".tex")))
  
  cat("\\begin{tabular}{l ccc | ccc | ccc} \n")
  cat("\\hline \n")
  cat(" Variable 
    & \\multicolumn{3}{c|}{All} 
    & \\multicolumn{3}{c|}{Treated Areas}
    & \\multicolumn{3}{c}{Control Areas} \\\\ \n")
  cat("  
    & \\multicolumn{3}{c|}{ } 
    & \\multicolumn{3}{c|}{(Within 5km of Improved Road)}
    & \\multicolumn{3}{c}{($>$5km of Improved Road)} \\\\ \n")
  cat(" \\hline ")
  cat(" & 1996 & 2013 & 2016        
      & 1996 & 2013 & 2016   
      & 1996 & 2013 & 2016 \\\\ \n")
  cat(" \\hline \n")
  
  cat("\\multicolumn{3}{l}{\\textbf{\\textit{Average across ",data_name,"}}} & & & & & & & \\\\ \n ")
  for(var in vars_to_summarise){
    make_sum_stats(var, data_full, data_near_rd, data_far_rd, "mean", dataset_type, ROUND_NUM_URBAN = 4)
  }
  
  cat("\\multicolumn{3}{l}{\\textbf{\\textit{N ",data_name," with Positive Value}}} & & & & & & & \\\\ \n ")
  for(var in vars_to_summarise){
    make_sum_stats(var, data_full, data_near_rd, data_far_rd, "N_g0", dataset_type, ROUND_NUM_URBAN = 4)
  }
  
  cat(" \\hline ")
  cat(" N & ", 
      nrow(data_full[data_full$year %in% 1996,]) %>% prettyNum(big.mark=",",scientific=FALSE), " & & & ",
      nrow(data_near_rd[data_near_rd$year %in% 1996,]) %>% prettyNum(big.mark=",",scientific=FALSE), " & & & ",
      nrow(data_far_rd[data_far_rd$year %in% 1996,]) %>% prettyNum(big.mark=",",scientific=FALSE), " & & \\\\ \n")
  
  cat("\\hline \n")
  cat("\\end{tabular} ")
  
  sink()
  
}

