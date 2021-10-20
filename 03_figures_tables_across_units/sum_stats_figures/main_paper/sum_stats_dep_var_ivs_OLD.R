# Summary Statistics


#### PARAMETERS
NEAR_TARGETTED_LOCATION <- 5000
RM_DISTANE_ADDIS <- 0 # (100km scale; so 1 = 100km)
NEAR_ROAD <- 5000

rsdp_type <- "rsdp123"
dataset_type <- "kebele"

create_table_df <- function(dataset_type, rsdp_type){
  
  if(rsdp_type %in% "rsdp123")  rsdp_end_year <- 2009
  if(rsdp_type %in% "rsdp1234") rsdp_end_year <- 2016
  
  df <- readRDS(file.path(panel_rsdp_imp_data_file_path, dataset_type, "merged_datasets", "panel_data_clean.Rds"))
  
  # Define rsdp/iv variables
  df$far_targettedlocs        <- df[[paste0("distance_",rsdp_type,"_targettedlocs")]]   > NEAR_TARGETTED_LOCATION
  df$near_rsdp                <- df[[paste0("distance_", rsdp_type)]]                  <= NEAR_ROAD
  df$near_rsdp_mst_euc        <- df[[paste0("distance_",rsdp_type,"_mst_euc")]]        <= NEAR_ROAD
  df$near_rsdp_mst_euc_region <- df[[paste0("distance_",rsdp_type,"_mst_euc_region")]] <= NEAR_ROAD
  df$near_rsdp_mst_lc         <- df[[paste0("distance_",rsdp_type,"_mst_lc")]]         <= NEAR_ROAD
  df$near_rsdp_mst_lc_region  <- df[[paste0("distance_",rsdp_type,"_mst_lc_region")]]  <= NEAR_ROAD
  df$baseline <- df$year %in% 1996
  if(rsdp_type %in% "rsdp123")  df$endline <- df$year %in% 2009
  if(rsdp_type %in% "rsdp1234") df$endline <- df$year %in% 2016
  
  if(dataset_type %in% "kebele"){
    df$globcover_urban    <- df$globcover_urban_sum
    df$globcover_cropland <- df$globcover_cropland_sum
  }
  
  # Prep data
  df <- df %>%
    dplyr::filter(baseline %in% T | endline %in% T) %>%
    dplyr::filter(far_targettedlocs %in% T) %>%
    arrange(year) %>%
    dplyr::group_by(cell_id) %>%
    dplyr::mutate(dmspols_harmon_diff     = c(NA, diff(dmspols_harmon)),
                  globcover_urban_diff    = c(NA, diff(globcover_urban)),
                  globcover_cropland_diff = c(NA, diff(globcover_cropland)),
                  dmspols_harmon_1996     = c(NA, dmspols_harmon[baseline %in% T]),
                  globcover_urban_1996    = c(NA, globcover_urban[baseline %in% T]),
                  globcover_cropland_1996 = c(NA, globcover_cropland[baseline %in% T])) %>%
    ungroup() 
  
  # Summarize
  sum_by_type <- function(group_var, df, dataset_type, rsdp_type){
    
    df$group_var <- df[[group_var]]
    
    df_sum <- df %>%
      dplyr::filter(endline %in% T) %>%
      group_by(group_var) %>%
      dplyr::summarise(dmspols_harmon_1996 = mean(dmspols_harmon_1996, na.rm = T),
                       globcover_urban_1996 = mean(globcover_urban_1996, na.rm = T),
                       globcover_cropland_1996 = mean(globcover_cropland_1996, na.rm = T),
                       dmspols_harmon_diff = mean(dmspols_harmon_diff, na.rm = T),
                       globcover_urban_diff = mean(globcover_urban_diff, na.rm = T),
                       globcover_cropland_diff = mean(globcover_cropland_diff, na.rm = T),
                       N = n()) %>%
      pivot_longer(cols = -group_var) %>%
      mutate(group_var = ifelse(group_var %in% T, "treat", "control")) %>%
      pivot_wider(names_from = group_var,
                  values_from = value) %>%
      rename_at(vars(-name), ~ paste0(., '_', group_var))
    
    if(dataset_type %in% "kebele" & rsdp_type %in% "rsdp123"){
      df_sum <- df_sum %>%
        mutate(name = case_when(
          name == "dmspols_harmon_diff" ~ "Change Avg NTL [1996 - 2009]",
          name == "globcover_urban_diff" ~ "Change N Urban Pixels [1996 - 2009]",
          name == "globcover_cropland_diff" ~ "Change N Cropland Pixels [1996 - 2009]",
          name == "dmspols_harmon_1996" ~ "Avg NTL, 1996",
          name == "globcover_urban_1996" ~ "N Urban Pixels, 1996",
          name == "globcover_cropland_1996" ~ "N Cropland Pixels, 1996",
          TRUE ~ name
        ))  
    }

    if(dataset_type %in% "kebele" & rsdp_type %in% "rsdp1234"){
      df_sum <- df_sum %>%
        mutate(name = case_when(
          name == "dmspols_harmon_diff" ~ "Change Avg NTL [1996 - 2016]",
          name == "globcover_urban_diff" ~ "Change N Urban Pixels [1996 - 2016]",
          name == "globcover_cropland_diff" ~ "Change N Cropland Pixels [1996 - 2016]",
          name == "dmspols_harmon_1996" ~ "Avg NTL, 1996",
          name == "globcover_urban_1996" ~ "N Urban Pixels, 1996",
          name == "globcover_cropland_1996" ~ "N Cropland Pixels, 1996",
          TRUE ~ name
        ))  
    }
    
    if(dataset_type %in% "dmspols_grid_ethiopia" & rsdp_type %in% "rsdp123"){
      df_sum <- df_sum %>%
        mutate(name = case_when(
          name == "dmspols_harmon_diff" ~ "Change Avg NTL [1996 - 2009]",
          name == "globcover_urban_diff" ~ "Change Urban [1996 - 2009]",
          name == "globcover_cropland_diff" ~ "Change Cropland [1996 - 2009]",
          name == "dmspols_harmon_1996" ~ "Avg NTL, 1996",
          name == "globcover_urban_1996" ~ "Urban Pixel, 1996",
          name == "globcover_cropland_1996" ~ "Cropland Pixel, 1996",
          TRUE ~ name
        ))  
    }
    
    if(dataset_type %in% "dmspols_grid_ethiopia" & rsdp_type %in% "rsdp1234"){
      df_sum <- df_sum %>%
        mutate(name = case_when(
          name == "dmspols_harmon_diff" ~ "Change Avg NTL [1996 - 2016]",
          name == "globcover_urban_diff" ~ "Change Urban [1996 - 2016]",
          name == "globcover_cropland_diff" ~ "Change Cropland [1996 - 2016]",
          name == "dmspols_harmon_1996" ~ "Avg NTL, 1996",
          name == "globcover_urban_1996" ~ "Urban Pixel, 1996",
          name == "globcover_cropland_1996" ~ "Cropland Pixel, 1996",
          TRUE ~ name
        ))  
    }
    
    return(df_sum)
  }
  
  df_sum <- lapply(c("near_rsdp", "near_rsdp_mst_euc", "near_rsdp_mst_lc"), 
                   sum_by_type, df, dataset_type, rsdp_type) %>% 
    reduce(left_join, by = "name") %>%
    mutate(tex = paste(name, "&",
                       treat_near_rsdp %>% round(4) %>% prettyNum(big.mark=",",scientific=FALSE), "&",
                       control_near_rsdp %>% round(4) %>% prettyNum(big.mark=",",scientific=FALSE), "&",
                       treat_near_rsdp_mst_euc %>% round(4) %>% prettyNum(big.mark=",",scientific=FALSE), "&",
                       control_near_rsdp_mst_euc %>% round(4) %>% prettyNum(big.mark=",",scientific=FALSE), "&",
                       treat_near_rsdp_mst_lc %>% round(4) %>% prettyNum(big.mark=",",scientific=FALSE), "&",
                       control_near_rsdp_mst_lc %>% round(4) %>% prettyNum(big.mark=",",scientific=FALSE), " \\\\ \n"))
  
  return(df_sum)
}

# keb_123 <- create_table_df("kebele", "rsdp123")
# keb_1324 <- create_table_df("kebele", "rsdp1234")
# 
# grid_123 <- create_table_df("dmspols_grid_ethiopia", "rsdp123")
# grid_1234 <- create_table_df("dmspols_grid_ethiopia", "rsdp1234")

for(dataset_type in c("kebele", "dmspols_grid_ethiopia")){
  for(rsdp_type in c("rsdp123", "rsdp1234")){
    print(dataset_type)
    print(rsdp_type)
    
    df_tex <- create_table_df(dataset_type, rsdp_type)
    
    sink(file.path(paper_tables,
                   paste0("depvar_sumstats_iv_",dataset_type,"_",rsdp_type, ".tex")))
    
    cat(" \\begin{tabular}{ l ll | ll | ll} \n ")
    cat(" \\hline \n")
    cat("Variable & \\multicolumn{2}{c|}{RSDP} & \\multicolumn{2}{c|}{Lease Distance MST} & \\multicolumn{2}{c}{Lease Cost MST} \\\\ \n")
    #cat(" \\hline \n")
    cat(" & Avg & Avg & Avg & Avg & Avg & Avg \\\\ \n")
    cat(" & $<$5km & $>$5km & $<$5km & $>$5km & $<$5km & $>$5km \\\\ \n")
    cat(" \\hline \n")
    
    for(i in 1:6) cat(df_tex$tex[i])
    cat(" \\hline \n")
    cat(df_tex$tex[7]) # N
    cat(" \\hline \n")
    cat(" \\end{tabular} ")
    
    sink()
  }
}

# TODO: Cleaning numbers. function -- if less than, round by X, if more than, round by Y
# TODO Add t-stat
# prettyNum(12345.678,big.mark=",",scientific=FALSE)
