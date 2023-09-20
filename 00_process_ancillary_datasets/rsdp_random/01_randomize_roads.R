# Alternate RSDP: Randomize Roads

set.seed(42)

# Load data --------------------------------------------------------------------
rsdp_sp <- readRDS(file.path(rsdp_dir, "RawData", "RoadNetworkPanelData_1996_2016.Rds"))

# Randomize year ---------------------------------------------------------------

#### Random Year
rsdp_rand_sp <- rsdp_sp
rsdp_rand_sp$Complete_G_rand <- sample(rsdp_rand_sp$Complete_G)

#### Year Restricted
year_df <- as.data.frame(table(rsdp_sp$Complete_G) / nrow(rsdp_sp)) %>%
  dplyr::rename(year = Var1,
                prob = Freq)

rsdp_yr_rt_sp <- rsdp_sp

rsdp_yr_rt_sp$Complete_G_rand <- lapply(1:nrow(rsdp_yr_rt_sp), function(i){
  rsdp_sp_i <- rsdp_yr_rt_sp[i,]
  
  year_df_i <- year_df[year_df$year != rsdp_sp_i$Complete_G,]
  
  sample(x = year_df_i$year, 
         size = 1,
         prob = year_df_i$prob) %>%
    as.character() %>%
    as.numeric()
}) %>%
  unlist()

# Adjust speeds ----------------------------------------------------------------

df_i <- rsdp_rand_sp[rsdp_rand_sp$Speed2016 == 120,]
df_i$Speed1996

adjust_speed <- function(df){
  
  lapply(1:nrow(df), function(i){
    
    df_i <- df[i,]
    
    year_c  <- df_i$Complete_G_rand
    speed_1 <- df_i$Speed1996
    speed_2 <- df_i$Speed2016
    
    if(year_c > 1996) speed_1 <- 0 # To force an improvement
    
    for(year_i in 1996:2016){
      if(year_i < year_c)  df_i[[paste0("Speed", year_i)]] <- speed_1
      if(year_i >= year_c) df_i[[paste0("Speed", year_i)]] <- speed_2
    }
    
    return(df_i)
  }) %>%
    do.call(what = "rbind")
  
}

rsdp_rand_sp  <- adjust_speed(rsdp_rand_sp)
rsdp_yr_rt_sp <- adjust_speed(rsdp_yr_rt_sp)

# Cleanup ----------------------------------------------------------------------
rsdp_rand_sp@data <- rsdp_rand_sp@data %>%
  dplyr::rename(Complete_G_orig = Complete_G) %>%
  dplyr::rename(Complete_G      = Complete_G_rand)

rsdp_yr_rt_sp@data <- rsdp_yr_rt_sp@data %>%
  dplyr::rename(Complete_G_orig = Complete_G) %>%
  dplyr::rename(Complete_G      = Complete_G_rand)

# Export -----------------------------------------------------------------------
saveRDS(rsdp_rand_sp,  file.path(rsdp_dir, "FinalData", "RoadNetworkPanelData_1996_2016_rand_year.Rds"))
saveRDS(rsdp_yr_rt_sp, file.path(rsdp_dir, "FinalData", "RoadNetworkPanelData_1996_2016_rand_year_restrict.Rds"))


# diff <- (rsdp_rand_sp$Complete_G - rsdp_rand_sp$Complete_G_orig)
# diff <- (rsdp_yr_rt_sp$Complete_G - rsdp_yr_rt_sp$Complete_G_orig)
# 
# diff %>% table %>% View()

# rsdp_sp$Speed2016       %>% table()
# rsdp_rand_sp$Speed2016  %>% table()
# rsdp_yr_rt_sp$Speed2016 %>% table()
# 
# a <- rsdp_rand_sp[rsdp_rand_sp$Speed2016 == 120,]
# a$Speed1996
# 
# table(rsdp_sp$Speed2016 > rsdp_sp$Speed1996)
# table(rsdp_rand_sp$Speed2016 > rsdp_rand_sp$Speed1996)
# table(rsdp_yr_rt_sp$Speed2016 > rsdp_yr_rt_sp$Speed1996)
# 
