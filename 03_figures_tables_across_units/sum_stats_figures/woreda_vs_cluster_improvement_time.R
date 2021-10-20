# Woreda Summary Trends

# Woreda Data ------------------------------------------------------------------
woreda_data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "woreda", "merged_datasets", "panel_data_clean.Rds"))
ntl_data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "merged_datasets", "panel_data_clean.Rds"))
glob_data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_globcover_urban", "merged_datasets", "panel_data_clean.Rds"))

# First year improved and number of times treted
calc_year_improved <- function(df, DIST_THRESH_M = 5000){
  
  # Create variable that indicates if near improved road of any speed
  df$near_improvedroad_speedafter_any <- 
    ((df$distance_improvedroad_speedafter_20 <= DIST_THRESH_M) %in% T) |
    ((df$distance_improvedroad_speedafter_25 <= DIST_THRESH_M) %in% T) |
    ((df$distance_improvedroad_speedafter_30 <= DIST_THRESH_M) %in% T) |
    ((df$distance_improvedroad_speedafter_35 <= DIST_THRESH_M) %in% T) |
    ((df$distance_improvedroad_speedafter_45 <= DIST_THRESH_M) %in% T) |
    ((df$distance_improvedroad_speedafter_50 <= DIST_THRESH_M) %in% T) |
    ((df$distance_improvedroad_speedafter_70 <= DIST_THRESH_M) %in% T) |
    ((df$distance_improvedroad_speedafter_120 <= DIST_THRESH_M) %in% T)
  
  df$near_improvedroad_speedafter_any <- df$near_improvedroad_speedafter_any %>% as.numeric()
  
  df_col <- df %>%
    mutate(nearrdXyear = near_improvedroad_speedafter_any * year) %>%
    group_by(cell_id) %>%
    dplyr::summarise(first_yr_improved = min(nearrdXyear[nearrdXyear > 0], na.rm = T),
                     N_year_treated = sum(nearrdXyear > 0, na.rm=T))
  
  return(df_col)
}

woreda_data_clean <- calc_year_improved(woreda_data, 0)
ntl_data_clean    <- calc_year_improved(ntl_data)
glob_data_clean   <- calc_year_improved(glob_data)

# First Year Treated
woreda_data_clean$first_yr_improved %>% table
ntl_data_clean$first_yr_improved %>% table
glob_data_clean$first_yr_improved %>% table

# First Year Treated (Prop)
round(woreda_data_clean$first_yr_improved %>% table / nrow(woreda_data_clean),3)
round(ntl_data_clean$first_yr_improved %>% table    / nrow(ntl_data_clean),3)
round(glob_data_clean$first_yr_improved %>% table   / nrow(glob_data_clean),3)

# Number of Times Treated
mean(woreda_data_clean$N_year_treated >= 3)
mean(ntl_data_clean$N_year_treated >= 3)
mean(glob_data_clean$N_year_treated >= 3)

# Number of Times Treated
mean(woreda_data_clean$N_year_treated >= 5)
mean(ntl_data_clean$N_year_treated >= 5)
mean(glob_data_clean$N_year_treated >= 5)

# Percent Near Treated Road
mean(woreda_data_clean$N_year_treated >= 1)
mean(ntl_data_clean$N_year_treated >= 1)
mean(glob_data_clean$N_year_treated >= 1)



grid_eth <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_ethiopia",
                              "merged_datasets", "panel_data_clean.Rds"))

grid_table <- grid_eth %>%
  filter(year %in% 2012) %>%
  filter(dmspols > 0) %>%
  group_by(near_anyimproved_ever_5km) %>%
  dplyr::summarise(N = n())

grid_table

