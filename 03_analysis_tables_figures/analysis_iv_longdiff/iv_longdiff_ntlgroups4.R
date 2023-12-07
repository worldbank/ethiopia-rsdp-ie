# Long-Difference IV Estimation

# Grab names of objects before running code. At end of code, delete new objects
# created; code is memory intensive cleaning up avoids memory issues for
# subsequent scripts.
OBJECTS_BEFORE_CODE <- ls()

#### Parameters
NEAR_TARGETTED_LOCATION <- 5000
RM_DISTANE_ADDIS <- 0
NEAR_ROAD <- 5000
ROUND_NUM <- 1 # number of digits to round numbers

# For naming files
rsdp_type <- "rsdp123"

# Prep Data Function -----------------------------------------------------------

# Function that preps data for a specific dataset
prep_data <- function(DATASET_TYPE){
  
  # Load Data ----------------------------------------------------------------
  df <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "merged_datasets", 
                          paste0("longdiff_data_clean_base", 
                                 1996,
                                 "_end",
                                 2009,
                                 ".Rds")))
  
  # Parameters and Variables -------------------------------------------------
  #### Parameters
  df$cluster_var <- df$woreda_id
  
  #### Dep Vars
  if(DATASET_TYPE %in% "dmspols_grid_ethiopia"){
    df <- df %>%
      dplyr::rename(dv_dmspols = dmspols_harmon_ihs,
                    dv_gcurban = globcover_urban,
                    dv_gccrop  = globcover_cropland,
                    
                    dv_dmspols_1996 = dmspols_harmon_ihs_1996,
                    dv_gcurban_1996 = globcover_urban_1996,
                    dv_gccrop_1996 = globcover_cropland_1996)
  }
  
  if(DATASET_TYPE %in% "kebele"){
    df <- df %>%
      dplyr::rename(dv_dmspols = dmspols_harmon_ihs,
                    dv_gcurban = globcover_urban_sum_ihs,
                    dv_gccrop  = globcover_cropland_sum_ihs,
                    
                    dv_dmspols_1996 = dmspols_harmon_ihs_1996,
                    dv_gcurban_1996 = globcover_urban_sum_ihs_1996,
                    dv_gccrop_1996 = globcover_cropland_sum_ihs_1996)
  }
  
  #### Distance Addis/Targetted Locs
  df$distance_city_addisababa           <- df$distance_city_addisababa / 1000 / 100 # 100km scale
  df$distance_rsdp123_targettedlocs_log <- log(df$distance_rsdp123_targettedlocs + 1)
  
  #### Prep variables, mainly interactions
  df <- df %>%
    
    dplyr::filter(distance_rsdp123_targettedlocs > NEAR_TARGETTED_LOCATION,
                  distance_city_addisababa       > RM_DISTANE_ADDIS) %>%
    
    dplyr::mutate(near_rsdp123         = as.numeric(distance_rsdp123 <= NEAR_ROAD),
                  near_mst_euc         = as.numeric(distance_rsdp123_mst_euc        <= NEAR_ROAD),
                  near_mst_euc_region  = as.numeric(distance_rsdp123_mst_euc_region <= NEAR_ROAD),
                  near_mst_lc          = as.numeric(distance_rsdp123_mst_lc        <= NEAR_ROAD),
                  near_mst_lc_region   = as.numeric(distance_rsdp123_mst_lc_region <= NEAR_ROAD),
                  
                  near_rsdp123Xdmspols_1996_bin4_1      = near_rsdp123 * dmspols_harmon_1996_bin4_1,
                  near_rsdp123Xdmspols_1996_bin4_2      = near_rsdp123 * dmspols_harmon_1996_bin4_2,
                  near_rsdp123Xdmspols_1996_bin4_3      = near_rsdp123 * dmspols_harmon_1996_bin4_3,
                  near_rsdp123Xdmspols_1996_bin4_4      = near_rsdp123 * dmspols_harmon_1996_bin4_4,
                  near_rsdp123Xdistance_city_addisababa = near_rsdp123 * distance_city_addisababa,
                  
                  near_mst_eucXdmspols_1996_bin4_1      = near_mst_euc * dmspols_harmon_1996_bin4_1,
                  near_mst_eucXdmspols_1996_bin4_2      = near_mst_euc * dmspols_harmon_1996_bin4_2,
                  near_mst_eucXdmspols_1996_bin4_3      = near_mst_euc * dmspols_harmon_1996_bin4_3,
                  near_mst_eucXdmspols_1996_bin4_4      = near_mst_euc * dmspols_harmon_1996_bin4_4,
                  near_mst_eucXdistance_city_addisababa = near_mst_euc * distance_city_addisababa,
                  
                  near_mst_euc_regionXdmspols_1996_bin4_1      = near_mst_euc_region * dmspols_harmon_1996_bin4_1,
                  near_mst_euc_regionXdmspols_1996_bin4_2      = near_mst_euc_region * dmspols_harmon_1996_bin4_2,
                  near_mst_euc_regionXdmspols_1996_bin4_3      = near_mst_euc_region * dmspols_harmon_1996_bin4_3,
                  near_mst_euc_regionXdmspols_1996_bin4_4      = near_mst_euc_region * dmspols_harmon_1996_bin4_4,
                  near_mst_euc_regionXdistance_city_addisababa = near_mst_euc_region * distance_city_addisababa,
                  
                  near_mst_lcXdmspols_1996_bin4_1      = near_mst_lc * dmspols_harmon_1996_bin4_1,
                  near_mst_lcXdmspols_1996_bin4_2      = near_mst_lc * dmspols_harmon_1996_bin4_2,
                  near_mst_lcXdmspols_1996_bin4_3      = near_mst_lc * dmspols_harmon_1996_bin4_3,
                  near_mst_lcXdmspols_1996_bin4_4      = near_mst_lc * dmspols_harmon_1996_bin4_4,
                  near_mst_lcXdistance_city_addisababa = near_mst_lc * distance_city_addisababa,
                  
                  near_mst_lc_regionXdmspols_1996_bin4_1      = near_mst_lc_region * dmspols_harmon_1996_bin4_1,
                  near_mst_lc_regionXdmspols_1996_bin4_2      = near_mst_lc_region * dmspols_harmon_1996_bin4_2,
                  near_mst_lc_regionXdmspols_1996_bin4_3      = near_mst_lc_region * dmspols_harmon_1996_bin4_3,
                  near_mst_lc_regionXdmspols_1996_bin4_4      = near_mst_lc_region * dmspols_harmon_1996_bin4_4,
                  near_mst_lc_regionXdistance_city_addisababa = near_mst_lc_region * distance_city_addisababa)
  
  return(df)
}

# Prep Data --------------------------------------------------------------------------
df_grid   <- prep_data("dmspols_grid_ethiopia")
df_kebele <- prep_data("kebele")

# OLS --------------------------------------------------------------------------
# Need to do as.formula %>% felm(), as if do felm(as.formula), then dep var heading 
# in stargazer is the full formula. Piping formula in solves.

## Grid
lm_dmspols_ihs_g     <- feols(dv_dmspols ~ near_rsdp123 + distance_rsdp123_targettedlocs_log, vcov = conley(20), data = df_grid)
lm_globcover_urban_g <- feols(dv_gcurban ~ near_rsdp123 + distance_rsdp123_targettedlocs_log, vcov = conley(20), data = df_grid)
lm_globcover_crop_g  <- feols(dv_gccrop ~ near_rsdp123 + distance_rsdp123_targettedlocs_log, vcov = conley(20), data = df_grid)

lm_dmspols_ihs_basentl_g     <- feols(dv_dmspols ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4, data = df_grid, vcov = conley(20)) 
lm_globcover_urban_basentl_g <- feols(dv_gcurban ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4, data = df_grid, vcov = conley(20)) 
lm_globcover_crop_basentl_g  <- feols(dv_gccrop ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4, data = df_grid, vcov = conley(20)) 

## Kebele
lm_dmspols_ihs_k     <- feols(dv_dmspols ~ near_rsdp123 + distance_rsdp123_targettedlocs_log, vcov = conley(20), data = df_kebele)
lm_globcover_urban_k <- feols(dv_gcurban ~ near_rsdp123 + distance_rsdp123_targettedlocs_log, vcov = conley(20), data = df_kebele)
lm_globcover_crop_k  <- feols(dv_gccrop ~ near_rsdp123 + distance_rsdp123_targettedlocs_log, vcov = conley(20), data = df_kebele)

lm_dmspols_ihs_basentl_k     <- feols(dv_dmspols ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4, data = df_kebele, vcov = conley(20)) 
lm_globcover_urban_basentl_k <- feols(dv_gcurban ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4, data = df_kebele, vcov = conley(20)) 
lm_globcover_crop_basentl_k  <- feols(dv_gccrop ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4, data = df_kebele, vcov = conley(20)) 

# MST - Cost Distance --------------------------------------------------------
## Grid
iv_cd_dmspols_ihs_g     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_lc, vcov = conley(20), data = df_grid)
iv_cd_globcover_urban_g <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_lc, vcov = conley(20), data = df_grid)
iv_cd_globcover_crop_g  <- feols(dv_gccrop ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_lc, vcov = conley(20), data = df_grid)

iv_cd_dmspols_ihs_basentl_g     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc + near_mst_lcXdmspols_1996_bin4_2 + near_mst_lcXdmspols_1996_bin4_3 + near_mst_lcXdmspols_1996_bin4_4, vcov = conley(20), data = df_grid)
iv_cd_globcover_urban_basentl_g <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc + near_mst_lcXdmspols_1996_bin4_2 + near_mst_lcXdmspols_1996_bin4_3 + near_mst_lcXdmspols_1996_bin4_4, vcov = conley(20), data = df_grid)
iv_cd_globcover_crop_basentl_g  <- feols(dv_gccrop ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc + near_mst_lcXdmspols_1996_bin4_2 + near_mst_lcXdmspols_1996_bin4_3 + near_mst_lcXdmspols_1996_bin4_4, vcov = conley(20), data = df_grid)

## Kebele
iv_cd_dmspols_ihs_k     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_lc, vcov = conley(20), data = df_kebele)
iv_cd_globcover_urban_k <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_lc, vcov = conley(20), data = df_kebele)
iv_cd_globcover_crop_k  <- feols(dv_gccrop ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_lc, vcov = conley(20), data = df_kebele)

iv_cd_dmspols_ihs_basentl_k     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc + near_mst_lcXdmspols_1996_bin4_2 + near_mst_lcXdmspols_1996_bin4_3 + near_mst_lcXdmspols_1996_bin4_4, vcov = conley(20), data = df_kebele)
iv_cd_globcover_urban_basentl_k <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc + near_mst_lcXdmspols_1996_bin4_2 + near_mst_lcXdmspols_1996_bin4_3 + near_mst_lcXdmspols_1996_bin4_4, vcov = conley(20), data = df_kebele)
iv_cd_globcover_crop_basentl_k  <- feols(dv_gccrop ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc + near_mst_lcXdmspols_1996_bin4_2 + near_mst_lcXdmspols_1996_bin4_3 + near_mst_lcXdmspols_1996_bin4_4, vcov = conley(20), data = df_kebele)

# MST - Least Distance -------------------------------------------------------
## Grid
iv_ld_dmspols_ihs_g     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_euc, vcov = conley(20), data = df_grid) 
iv_ld_globcover_urban_g <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_euc, vcov = conley(20), data = df_grid) 
iv_ld_globcover_crop_g  <- feols(dv_gccrop ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_euc, vcov = conley(20), data = df_grid) 

iv_ld_dmspols_ihs_basentl_g     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc + near_mst_eucXdmspols_1996_bin4_2 + near_mst_eucXdmspols_1996_bin4_3 + near_mst_eucXdmspols_1996_bin4_4, vcov = conley(20), data = df_grid)
iv_ld_globcover_urban_basentl_g <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc + near_mst_eucXdmspols_1996_bin4_2 + near_mst_eucXdmspols_1996_bin4_3 + near_mst_eucXdmspols_1996_bin4_4, vcov = conley(20), data = df_grid)
iv_ld_globcover_crop_basentl_g  <- feols(dv_gccrop ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc + near_mst_eucXdmspols_1996_bin4_2 + near_mst_eucXdmspols_1996_bin4_3 + near_mst_eucXdmspols_1996_bin4_4, vcov = conley(20), data = df_grid)

## Kebele
iv_ld_dmspols_ihs_k     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_euc, vcov = conley(20), data = df_kebele) 
iv_ld_globcover_urban_k <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_euc, vcov = conley(20), data = df_kebele) 
iv_ld_globcover_crop_k  <- feols(dv_gccrop ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_euc, vcov = conley(20), data = df_kebele) 

iv_ld_dmspols_ihs_basentl_k     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc + near_mst_eucXdmspols_1996_bin4_2 + near_mst_eucXdmspols_1996_bin4_3 + near_mst_eucXdmspols_1996_bin4_4, vcov = conley(20), data = df_kebele)
iv_ld_globcover_urban_basentl_k <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc + near_mst_eucXdmspols_1996_bin4_2 + near_mst_eucXdmspols_1996_bin4_3 + near_mst_eucXdmspols_1996_bin4_4, vcov = conley(20), data = df_kebele)
iv_ld_globcover_crop_basentl_k  <- feols(dv_gccrop ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc + near_mst_eucXdmspols_1996_bin4_2 + near_mst_eucXdmspols_1996_bin4_3 + near_mst_eucXdmspols_1996_bin4_4, vcov = conley(20), data = df_kebele)

# MST Regions - Cost Distance ------------------------------------------------
## Grid
iv_cd_dmspols_ihs_regions_g     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_lc_region, vcov = conley(20), data = df_grid) 
iv_cd_globcover_urban_regions_g <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_lc_region, vcov = conley(20), data = df_grid) 
iv_cd_globcover_crop_regions_g  <- feols(dv_gccrop ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_lc_region, vcov = conley(20), data = df_grid) 

iv_cd_dmspols_ihs_basentl_regions_g     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc_region + near_mst_lc_regionXdmspols_1996_bin4_2 + near_mst_lc_regionXdmspols_1996_bin4_3 + near_mst_lc_regionXdmspols_1996_bin4_4, vcov = conley(20), data = df_grid) 
iv_cd_globcover_urban_basentl_regions_g <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc_region + near_mst_lc_regionXdmspols_1996_bin4_2 + near_mst_lc_regionXdmspols_1996_bin4_3 + near_mst_lc_regionXdmspols_1996_bin4_4, vcov = conley(20), data = df_grid) 
iv_cd_globcover_crop_basentl_regions_g  <- feols(dv_gccrop ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc_region + near_mst_lc_regionXdmspols_1996_bin4_2 + near_mst_lc_regionXdmspols_1996_bin4_3 + near_mst_lc_regionXdmspols_1996_bin4_4, vcov = conley(20), data = df_grid) 

## Kebele
iv_cd_dmspols_ihs_regions_k     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_lc_region, vcov = conley(20), data = df_kebele) 
iv_cd_globcover_urban_regions_k <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_lc_region, vcov = conley(20), data = df_kebele) 
iv_cd_globcover_crop_regions_k  <- feols(dv_gccrop ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_lc_region, vcov = conley(20), data = df_kebele) 

iv_cd_dmspols_ihs_basentl_regions_k     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc_region + near_mst_lc_regionXdmspols_1996_bin4_2 + near_mst_lc_regionXdmspols_1996_bin4_3 + near_mst_lc_regionXdmspols_1996_bin4_4, vcov = conley(20), data = df_kebele) 
iv_cd_globcover_urban_basentl_regions_k <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc_region + near_mst_lc_regionXdmspols_1996_bin4_2 + near_mst_lc_regionXdmspols_1996_bin4_3 + near_mst_lc_regionXdmspols_1996_bin4_4, vcov = conley(20), data = df_kebele) 
iv_cd_globcover_crop_basentl_regions_k  <- feols(dv_gccrop ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc_region + near_mst_lc_regionXdmspols_1996_bin4_2 + near_mst_lc_regionXdmspols_1996_bin4_3 + near_mst_lc_regionXdmspols_1996_bin4_4, vcov = conley(20), data = df_kebele) 

# MST Regions - Least Distance -------------------------------------------------------
## Grid
iv_ld_dmspols_ihs_regions_g     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_euc_region, vcov = conley(20), data = df_grid) 
iv_ld_globcover_urban_regions_g <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_euc_region, vcov = conley(20), data = df_grid) 
iv_ld_globcover_crop_regions_g  <- feols(dv_gccrop ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_euc_region, vcov = conley(20), data = df_grid) 

iv_ld_dmspols_ihs_basentl_regions_g     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc_region + near_mst_euc_regionXdmspols_1996_bin4_2 + near_mst_euc_regionXdmspols_1996_bin4_3 + near_mst_euc_regionXdmspols_1996_bin4_4, vcov = conley(20), data = df_grid) 
iv_ld_globcover_urban_basentl_regions_g <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc_region + near_mst_euc_regionXdmspols_1996_bin4_2 + near_mst_euc_regionXdmspols_1996_bin4_3 + near_mst_euc_regionXdmspols_1996_bin4_4, vcov = conley(20), data = df_grid) 
iv_ld_globcover_crop_basentl_regions_g  <- feols(dv_gccrop ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc_region + near_mst_euc_regionXdmspols_1996_bin4_2 + near_mst_euc_regionXdmspols_1996_bin4_3 + near_mst_euc_regionXdmspols_1996_bin4_4, vcov = conley(20), data = df_grid) 

## Kebele
iv_ld_dmspols_ihs_regions_k     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_euc_region, vcov = conley(20), data = df_kebele) 
iv_ld_globcover_urban_regions_k <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_euc_region, vcov = conley(20), data = df_kebele) 
iv_ld_globcover_crop_regions_k  <- feols(dv_gccrop ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_euc_region, vcov = conley(20), data = df_kebele) 

iv_ld_dmspols_ihs_basentl_regions_k     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc_region + near_mst_euc_regionXdmspols_1996_bin4_2 + near_mst_euc_regionXdmspols_1996_bin4_3 + near_mst_euc_regionXdmspols_1996_bin4_4, vcov = conley(20), data = df_kebele) 
iv_ld_globcover_urban_basentl_regions_k <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc_region + near_mst_euc_regionXdmspols_1996_bin4_2 + near_mst_euc_regionXdmspols_1996_bin4_3 + near_mst_euc_regionXdmspols_1996_bin4_4, vcov = conley(20), data = df_kebele) 
iv_ld_globcover_crop_basentl_regions_k  <- feols(dv_gccrop ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc_region + near_mst_euc_regionXdmspols_1996_bin4_2 + near_mst_euc_regionXdmspols_1996_bin4_3 + near_mst_euc_regionXdmspols_1996_bin4_4, vcov = conley(20), data = df_kebele) 

# MAIN RESULTS TABLES ==========================================================
round_char <- function(x){
  x %>% round(2) %>% as.character()
}

modelsummary_tab(list("NTL" = lm_dmspols_ihs_k, 
                  "NTL" = lm_dmspols_ihs_basentl_k, 
                  "Urban" = lm_globcover_urban_k,
                  "Urban" = lm_globcover_urban_basentl_k, 
                  "Cropland" = lm_globcover_crop_k, 
                  "Cropland" = lm_globcover_crop_basentl_k,
                  
                  "NTL" = iv_cd_dmspols_ihs_regions_k, 
                  "NTL" = iv_cd_dmspols_ihs_basentl_regions_k, 
                  "Urban" = iv_cd_globcover_urban_regions_k, 
                  "Urban" = iv_cd_globcover_urban_basentl_regions_k, 
                  "Cropland" = iv_cd_globcover_crop_regions_k, 
                  "Cropland" = iv_cd_globcover_crop_basentl_regions_k),
             stars = c('*' = .05, '**' = .01, "***" = 0.001),
             coef_map = c("near_rsdp123" = "Imp Rd",
                          "near_rsdp123Xdmspols_1996_bin4_2" = "Imp Rd.$\\times NTL_{96}$ Low",
                          "near_rsdp123Xdmspols_1996_bin4_3" = "Imp Rd.$\\times NTL_{96}$ Medium",
                          "near_rsdp123Xdmspols_1996_bin4_4" = "Imp Rd.$\\times NTL_{96}$ High",
                          "fit_near_rsdp123" = "Imp Rd",
                          "fit_near_rsdp123Xdmspols_1996_bin4_2" = "Imp Rd.$\\times NTL_{96}$ Low",
                          "fit_near_rsdp123Xdmspols_1996_bin4_3" = "Imp Rd.$\\times NTL_{96}$ Medium",
                          "fit_near_rsdp123Xdmspols_1996_bin4_4" = "Imp Rd.$\\times NTL_{96}$ High"),
             gof_map = c("nobs", "adj.r.squared"),
             escape = FALSE,
             add_rows = tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6, ~V7, ~V8, ~V9, ~V10, ~V11, ~V12,
                                'Type', "OLS","OLS","OLS","OLS","OLS","OLS","IV","IV","IV","IV","IV","IV",
                                '1st Stage F-Stat',"NA","NA","NA","NA","NA","NA",
                                fitstat(iv_cd_dmspols_ihs_regions_k, type = "ivf", simplify = T)$stat %>% round_char(),
                                fitstat(iv_cd_dmspols_ihs_basentl_regions_k, type = "ivf", simplify = T)$stat %>% round_char(),
                                fitstat(iv_cd_globcover_urban_regions_k, type = "ivf", simplify = T)$stat %>% round_char(),
                                fitstat(iv_cd_globcover_urban_basentl_regions_k, type = "ivf", simplify = T)$stat %>% round_char(),
                                fitstat(iv_cd_globcover_crop_regions_k, type = "ivf", simplify = T)$stat %>% round_char(),
                                fitstat(iv_cd_globcover_crop_basentl_regions_k, type = "ivf", simplify = T)$stat %>% round_char()),
             output = file.path(paper_tables, 
                                paste0("ols_iv_near_mst_cost_distance_5km_","kebele","_results_",rsdp_type,"_regions.tex")))

# stargazer(lm_dmspols_ihs_k, 
#           lm_dmspols_ihs_basentl_k, 
#           lm_globcover_urban_k,
#           lm_globcover_urban_basentl_k, 
#           lm_globcover_crop_k, 
#           lm_globcover_crop_basentl_k,
#           
#           iv_cd_dmspols_ihs_regions_k %>% update_iv_coef_name(), 
#           iv_cd_dmspols_ihs_basentl_regions_k %>% update_iv_coef_name(), 
#           iv_cd_globcover_urban_regions_k %>% update_iv_coef_name(), 
#           iv_cd_globcover_urban_basentl_regions_k %>% update_iv_coef_name(), 
#           iv_cd_globcover_crop_regions_k %>% update_iv_coef_name(), 
#           iv_cd_globcover_crop_basentl_regions_k %>% update_iv_coef_name(),
#           
#           dep.var.labels.include = T,
#           dep.var.labels = c("NTL", "Urban", "Cropland",
#                              "NTL", "Urban", "Cropland"), #  "NTL $\\geq$ 2", "NTL $\\geq$ 6",
#           dep.var.caption = "",
#           omit = c("temp_avg", "precipitation", "distance_rsdp123_targettedlocs_log"),
#           covariate.labels = c("Imp Rd.",
#                                "Imp Rd.$\\times NTL_{96}$ Low",
#                                "Imp Rd.$\\times NTL_{96}$ Med",
#                                "Imp Rd.$\\times NTL_{96}$ High"),
#           omit.stat = c("f","ser", "rsq"),
#           align=TRUE,
#           no.space=TRUE,
#           float=FALSE,
#           column.sep.width="-15pt",
#           digits=2,
#           omit.table.layout = "n",
#           
#           add.lines =         add_lines <- list(
#             c("Type", rep("OLS", 6), rep("IV", 6)),
#             c("1st Stage F-Stat",
#               rep("N/A", 6),
#               
#               iv_cd_dmspols_ihs_regions_k$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
#               iv_cd_dmspols_ihs_basentl_regions_k$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
#               iv_cd_globcover_urban_regions_k$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
#               iv_cd_globcover_urban_basentl_regions_k$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
#               iv_cd_globcover_crop_regions_k$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
#               iv_cd_globcover_crop_basentl_regions_k$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM))
#           ),
#           out = file.path(paper_tables, 
#                           paste0("ols_iv_near_mst_cost_distance_5km_","kebele","_results_",rsdp_type,"_regions.tex"))
# )

modelsummary_tab(list("NTL" = iv_cd_dmspols_ihs_regions_k, 
                  "NTL" = iv_cd_dmspols_ihs_basentl_regions_k, 
                  "Urban" = iv_cd_globcover_urban_regions_k, 
                  "Urban" = iv_cd_globcover_urban_basentl_regions_k, 
                  "Cropland" = iv_cd_globcover_crop_regions_k, 
                  "Cropland" = iv_cd_globcover_crop_basentl_regions_k),
             stars = c('*' = .05, '**' = .01, "***" = 0.001),
             coef_map = c("near_rsdp123" = "Imp Rd",
                          "near_rsdp123Xdmspols_1996_bin4_2" = "Imp Rd.$\\times NTL_{96}$ Low",
                          "near_rsdp123Xdmspols_1996_bin4_3" = "Imp Rd.$\\times NTL_{96}$ Medium",
                          "near_rsdp123Xdmspols_1996_bin4_4" = "Imp Rd.$\\times NTL_{96}$ High",
                          "fit_near_rsdp123" = "Imp Rd",
                          "fit_near_rsdp123Xdmspols_1996_bin4_2" = "Imp Rd.$\\times NTL_{96}$ Low",
                          "fit_near_rsdp123Xdmspols_1996_bin4_3" = "Imp Rd.$\\times NTL_{96}$ Medium",
                          "fit_near_rsdp123Xdmspols_1996_bin4_4" = "Imp Rd.$\\times NTL_{96}$ High"),
             gof_map = c("nobs", "adj.r.squared"),
             escape = FALSE,
             add_rows = tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6,
                                'Type', "IV","IV","IV","IV","IV","IV",
                                '1st Stage F-Stat',
                                fitstat(iv_cd_dmspols_ihs_regions_k, type = "ivf", simplify = T)$stat %>% round_char(),
                                fitstat(iv_cd_dmspols_ihs_basentl_regions_k, type = "ivf", simplify = T)$stat %>% round_char(),
                                fitstat(iv_cd_globcover_urban_regions_k, type = "ivf", simplify = T)$stat %>% round_char(),
                                fitstat(iv_cd_globcover_urban_basentl_regions_k, type = "ivf", simplify = T)$stat %>% round_char(),
                                fitstat(iv_cd_globcover_crop_regions_k, type = "ivf", simplify = T)$stat %>% round_char(),
                                fitstat(iv_cd_globcover_crop_basentl_regions_k, type = "ivf", simplify = T)$stat %>% round_char()),
             output = file.path(paper_tables, 
                                paste0("ols_iv_near_mst_cost_distance_5km_","kebele","_results_",rsdp_type,"_regions_IVonly.tex")))

# stargazer(iv_cd_dmspols_ihs_regions_k %>% update_iv_coef_name(), 
#           iv_cd_dmspols_ihs_basentl_regions_k %>% update_iv_coef_name(), 
#           iv_cd_globcover_urban_regions_k %>% update_iv_coef_name(), 
#           iv_cd_globcover_urban_basentl_regions_k %>% update_iv_coef_name(), 
#           iv_cd_globcover_crop_regions_k %>% update_iv_coef_name(), 
#           iv_cd_globcover_crop_basentl_regions_k %>% update_iv_coef_name(),
#           
#           dep.var.labels.include = T,
#           dep.var.labels = c("NTL", "Urban", "Cropland",
#                              "NTL", "Urban", "Cropland"), #  "NTL $\\geq$ 2", "NTL $\\geq$ 6",
#           dep.var.caption = "",
#           omit = c("temp_avg", "precipitation", "distance_rsdp123_targettedlocs_log"),
#           covariate.labels = c("Imp Rd.",
#                                "Imp Rd.$\\times NTL_{96}$ Low",
#                                "Imp Rd.$\\times NTL_{96}$ Med",
#                                "Imp Rd.$\\times NTL_{96}$ High"),
#           omit.stat = c("f","ser", "rsq"),
#           align=TRUE,
#           no.space=TRUE,
#           float=FALSE,
#           column.sep.width="8pt",
#           digits=2,
#           omit.table.layout = "n",
#           
#           add.lines =         add_lines <- list(
#             c("Type", rep("IV", 6)),
#             c("1st Stage F-Stat",
#               #rep("N/A", 6),
#               
#               iv_cd_dmspols_ihs_regions_k$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
#               iv_cd_dmspols_ihs_basentl_regions_k$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
#               iv_cd_globcover_urban_regions_k$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
#               iv_cd_globcover_urban_basentl_regions_k$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
#               iv_cd_globcover_crop_regions_k$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
#               iv_cd_globcover_crop_basentl_regions_k$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM))
#           ),
#           out = file.path(paper_tables, 
#                           paste0("ols_iv_near_mst_cost_distance_5km_","kebele","_results_",rsdp_type,"_regions_IVonly.tex"))
# )

# Log (not IHS) ================================================================
# https://gist.github.com/alexeyknorre/b0780836f4cec04d41a863a683f91b53

## OLS
lm_dmspols_k_lg     <- feols(dmspols_harmon_log ~ near_rsdp123 + distance_rsdp123_targettedlocs_log, vcov = conley(20), data = df_kebele)
lm_globcover_urban_k_lg <- feols(globcover_urban_sum_log ~ near_rsdp123 + distance_rsdp123_targettedlocs_log, vcov = conley(20), data = df_kebele) 
lm_globcover_crop_k_lg  <- feols(globcover_cropland_sum_log  ~ near_rsdp123 + distance_rsdp123_targettedlocs_log, vcov = conley(20), data = df_kebele) 

lm_dmspols_basentl_k_lg     <- feols(dmspols_harmon_log ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4, vcov = conley(20), data = df_kebele) 
lm_globcover_urban_basentl_k_lg <- feols(globcover_urban_sum_log ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4, vcov = conley(20), data = df_kebele) 
lm_globcover_crop_basentl_k_lg  <- feols(globcover_cropland_sum_log  ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4, vcov = conley(20), data = df_kebele) 

## IV
iv_cd_dmspols_regions_k_lg     <- feols(dmspols_harmon_log ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_lc_region , vcov = conley(20), data = df_kebele) 
iv_cd_globcover_urban_regions_k_lg <- feols(globcover_urban_sum_log ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_lc_region , vcov = conley(20), data = df_kebele) 
iv_cd_globcover_crop_regions_k_lg  <- feols(globcover_cropland_sum_log  ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_lc_region , vcov = conley(20), data = df_kebele) 

iv_cd_dmspols_basentl_regions_k_lg     <- feols(dmspols_harmon_log ~ distance_rsdp123_targettedlocs_log |              near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc_region + near_mst_lc_regionXdmspols_1996_bin4_2 + near_mst_lc_regionXdmspols_1996_bin4_3 + near_mst_lc_regionXdmspols_1996_bin4_4 , vcov = conley(20), data = df_kebele) 
iv_cd_globcover_urban_basentl_regions_k_lg <- feols(globcover_urban_sum_log ~ distance_rsdp123_targettedlocs_log |     near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc_region + near_mst_lc_regionXdmspols_1996_bin4_2 + near_mst_lc_regionXdmspols_1996_bin4_3 + near_mst_lc_regionXdmspols_1996_bin4_4, vcov = conley(20), data = df_kebele) 
iv_cd_globcover_crop_basentl_regions_k_lg  <- feols(globcover_cropland_sum_log  ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc_region + near_mst_lc_regionXdmspols_1996_bin4_2 + near_mst_lc_regionXdmspols_1996_bin4_3 + near_mst_lc_regionXdmspols_1996_bin4_4 , vcov = conley(20), data = df_kebele) 

modelsummary_tab(list("NTL" = lm_dmspols_k_lg, 
                  "NTL" = lm_dmspols_basentl_k_lg, 
                  "Urban" = lm_globcover_urban_k_lg,
                  "Urban" = lm_globcover_urban_basentl_k_lg, 
                  "Cropland" = lm_globcover_crop_k_lg, 
                  "Cropland" = lm_globcover_crop_basentl_k_lg,
                  
                  "NTL" = iv_cd_dmspols_regions_k_lg, 
                  "NTL" = iv_cd_dmspols_basentl_regions_k_lg, 
                  "Urban" = iv_cd_globcover_urban_regions_k_lg, 
                  "Urban" = iv_cd_globcover_urban_basentl_regions_k_lg, 
                  "Cropland" = iv_cd_globcover_crop_regions_k_lg, 
                  "Cropland" = iv_cd_globcover_crop_basentl_regions_k_lg),
             stars = c('*' = .05, '**' = .01, "***" = 0.001),
             coef_map = c("near_rsdp123" = "Imp Rd",
                          "near_rsdp123Xdmspols_1996_bin4_2" = "Imp Rd.$\\times NTL_{96}$ Low",
                          "near_rsdp123Xdmspols_1996_bin4_3" = "Imp Rd.$\\times NTL_{96}$ Medium",
                          "near_rsdp123Xdmspols_1996_bin4_4" = "Imp Rd.$\\times NTL_{96}$ High",
                          "fit_near_rsdp123" = "Imp Rd",
                          "fit_near_rsdp123Xdmspols_1996_bin4_2" = "Imp Rd.$\\times NTL_{96}$ Low",
                          "fit_near_rsdp123Xdmspols_1996_bin4_3" = "Imp Rd.$\\times NTL_{96}$ Medium",
                          "fit_near_rsdp123Xdmspols_1996_bin4_4" = "Imp Rd.$\\times NTL_{96}$ High"),
             gof_map = c("nobs", "adj.r.squared"),
             escape = FALSE,
             add_rows = tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6, ~V7, ~V8, ~V9, ~V10, ~V11, ~V12,
                                'Type', "OLS","OLS","OLS","OLS","OLS","OLS","IV","IV","IV","IV","IV","IV",
                                '1st Stage F-Stat',"NA","NA","NA","NA","NA","NA",
                                fitstat(iv_cd_dmspols_regions_k_lg, type = "ivf", simplify = T)$stat %>% round_char(),
                                fitstat(iv_cd_dmspols_basentl_regions_k_lg, type = "ivf", simplify = T)$stat %>% round_char(),
                                fitstat(iv_cd_globcover_urban_regions_k_lg, type = "ivf", simplify = T)$stat %>% round_char(),
                                fitstat(iv_cd_globcover_urban_basentl_regions_k_lg, type = "ivf", simplify = T)$stat %>% round_char(),
                                fitstat(iv_cd_globcover_crop_regions_k_lg, type = "ivf", simplify = T)$stat %>% round_char(),
                                fitstat(iv_cd_globcover_crop_basentl_regions_k_lg, type = "ivf", simplify = T)$stat %>% round_char()),
             output = file.path(paper_tables, 
                                paste0("ols_iv_near_mst_cost_distance_5km_","kebele","_results_",rsdp_type,"_regions_log.tex")))

# stargazer(lm_dmspols_k_lg, 
#           lm_dmspols_basentl_k_lg, 
#           lm_globcover_urban_k_lg,
#           lm_globcover_urban_basentl_k_lg, 
#           lm_globcover_crop_k_lg, 
#           lm_globcover_crop_basentl_k_lg,
#           
#           iv_cd_dmspols_regions_k_lg %>% update_iv_coef_name(), 
#           iv_cd_dmspols_basentl_regions_k_lg %>% update_iv_coef_name(), 
#           iv_cd_globcover_urban_regions_k_lg %>% update_iv_coef_name(), 
#           iv_cd_globcover_urban_basentl_regions_k_lg %>% update_iv_coef_name(), 
#           iv_cd_globcover_crop_regions_k_lg %>% update_iv_coef_name(), 
#           iv_cd_globcover_crop_basentl_regions_k_lg %>% update_iv_coef_name(),
#           
#           dep.var.labels.include = T,
#           dep.var.labels = c("NTL", "Urban", "Cropland",
#                              "NTL", "Urban", "Cropland"), #  "NTL $\\geq$ 2", "NTL $\\geq$ 6",
#           dep.var.caption = "",
#           omit = c("temp_avg", "precipitation", "distance_rsdp123_targettedlocs_log"),
#           covariate.labels = c("Imp Rd.",
#                                "Imp Rd.$\\times NTL_{96}$ Low",
#                                "Imp Rd.$\\times NTL_{96}$ Med",
#                                "Imp Rd.$\\times NTL_{96}$ High"),
#           omit.stat = c("f","ser", "rsq"),
#           align=TRUE,
#           no.space=TRUE,
#           float=FALSE,
#           column.sep.width="-15pt",
#           digits=2,
#           omit.table.layout = "n",
#           
#           add.lines =         add_lines <- list(
#             c("Type", rep("OLS", 6), rep("IV", 6)),
#             c("1st Stage F-Stat",
#               rep("N/A", 6),
#               
#               iv_cd_dmspols_regions_k_lg$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
#               iv_cd_dmspols_basentl_regions_k_lg$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
#               iv_cd_globcover_urban_regions_k_lg$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
#               iv_cd_globcover_urban_basentl_regions_k_lg$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
#               iv_cd_globcover_crop_regions_k_lg$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
#               iv_cd_globcover_crop_basentl_regions_k_lg$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM))
#           ),
#           out = file.path(paper_tables, 
#                           paste0("ols_iv_near_mst_cost_distance_5km_","kebele","_results_",rsdp_type,"_regions_log.tex"))
# )

# Remove Elect Trans Lines =====================================================
# https://gist.github.com/alexeyknorre/b0780836f4cec04d41a863a683f91b53

df_kebele_rm_elec <- df_kebele %>%
  dplyr::filter(distance_elec_trans >= 5000)

## OLS
lm_dmspols_k_el     <- feols(dv_dmspols ~ near_rsdp123 + distance_rsdp123_targettedlocs_log , vcov = conley(20), data = df_kebele_rm_elec)
lm_globcover_urban_k_el <- feols(dv_gcurban ~ near_rsdp123 + distance_rsdp123_targettedlocs_log , vcov = conley(20), data = df_kebele_rm_elec)
lm_globcover_crop_k_el  <- feols(dv_gccrop  ~ near_rsdp123 + distance_rsdp123_targettedlocs_log , vcov = conley(20), data = df_kebele_rm_elec)

lm_dmspols_basentl_k_el     <- feols(dv_dmspols ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4  , vcov = conley(20), data = df_kebele_rm_elec)
lm_globcover_urban_basentl_k_el <- feols(dv_gcurban ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4  , vcov = conley(20), data = df_kebele_rm_elec)
lm_globcover_crop_basentl_k_el  <- feols(dv_gccrop  ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4  , vcov = conley(20), data = df_kebele_rm_elec)

## IV
iv_cd_dmspols_regions_k_el     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_lc_region , vcov = conley(20), data = df_kebele_rm_elec)
iv_cd_globcover_urban_regions_k_el <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_lc_region , vcov = conley(20), data = df_kebele_rm_elec)
iv_cd_globcover_crop_regions_k_el  <- feols(dv_gccrop  ~ distance_rsdp123_targettedlocs_log | near_rsdp123 ~ near_mst_lc_region , vcov = conley(20), data = df_kebele_rm_elec)

iv_cd_dmspols_basentl_regions_k_el     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log |     near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc_region + near_mst_lc_regionXdmspols_1996_bin4_2 + near_mst_lc_regionXdmspols_1996_bin4_3 + near_mst_lc_regionXdmspols_1996_bin4_4 , vcov = conley(20), data = df_kebele_rm_elec)
iv_cd_globcover_urban_basentl_regions_k_el <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc_region + near_mst_lc_regionXdmspols_1996_bin4_2 + near_mst_lc_regionXdmspols_1996_bin4_3 + near_mst_lc_regionXdmspols_1996_bin4_4 , vcov = conley(20), data = df_kebele_rm_elec)
iv_cd_globcover_crop_basentl_regions_k_el  <- feols(dv_gccrop  ~ distance_rsdp123_targettedlocs_log | near_rsdp123 + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc_region + near_mst_lc_regionXdmspols_1996_bin4_2 + near_mst_lc_regionXdmspols_1996_bin4_3 + near_mst_lc_regionXdmspols_1996_bin4_4 , vcov = conley(20), data = df_kebele_rm_elec)

modelsummary_tab(list("NTL" = lm_dmspols_k_el, 
                  "NTL" = lm_dmspols_basentl_k_el, 
                  "Urban" = lm_globcover_urban_k_el,
                  "Urban" = lm_globcover_urban_basentl_k_el, 
                  "Cropland" = lm_globcover_crop_k_el, 
                  "Cropland" = lm_globcover_crop_basentl_k_el,
                  
                  "NTL" = iv_cd_dmspols_regions_k_el, 
                  "NTL" = iv_cd_dmspols_basentl_regions_k_el, 
                  "Urban" = iv_cd_globcover_urban_regions_k_el, 
                  "Urban" = iv_cd_globcover_urban_basentl_regions_k_el, 
                  "Cropland" = iv_cd_globcover_crop_regions_k_el, 
                  "Cropland" = iv_cd_globcover_crop_basentl_regions_k_el),
             stars = c('*' = .05, '**' = .01, "***" = 0.001),
             coef_map = c("near_rsdp123" = "Imp Rd",
                          "near_rsdp123Xdmspols_1996_bin4_2" = "Imp Rd.$\\times NTL_{96}$ Low",
                          "near_rsdp123Xdmspols_1996_bin4_3" = "Imp Rd.$\\times NTL_{96}$ Medium",
                          "near_rsdp123Xdmspols_1996_bin4_4" = "Imp Rd.$\\times NTL_{96}$ High",
                          "fit_near_rsdp123" = "Imp Rd",
                          "fit_near_rsdp123Xdmspols_1996_bin4_2" = "Imp Rd.$\\times NTL_{96}$ Low",
                          "fit_near_rsdp123Xdmspols_1996_bin4_3" = "Imp Rd.$\\times NTL_{96}$ Medium",
                          "fit_near_rsdp123Xdmspols_1996_bin4_4" = "Imp Rd.$\\times NTL_{96}$ High"),
             gof_map = c("nobs", "adj.r.squared"),
             escape = FALSE,
             add_rows = tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6, ~V7, ~V8, ~V9, ~V10, ~V11, ~V12,
                                'Type', "OLS","OLS","OLS","OLS","OLS","OLS","IV","IV","IV","IV","IV","IV",
                                '1st Stage F-Stat',"NA","NA","NA","NA","NA","NA",
                                fitstat(iv_cd_dmspols_regions_k_el, type = "ivf", simplify = T)$stat %>% round_char(),
                                fitstat(iv_cd_dmspols_basentl_regions_k_el, type = "ivf", simplify = T)$stat %>% round_char(),
                                fitstat(iv_cd_globcover_urban_regions_k_el, type = "ivf", simplify = T)$stat %>% round_char(),
                                fitstat(iv_cd_globcover_urban_basentl_regions_k_el, type = "ivf", simplify = T)$stat %>% round_char(),
                                fitstat(iv_cd_globcover_crop_regions_k_el, type = "ivf", simplify = T)$stat %>% round_char(),
                                fitstat(iv_cd_globcover_crop_basentl_regions_k_el, type = "ivf", simplify = T)$stat %>% round_char()),
             output = file.path(paper_tables, 
                                paste0("ols_iv_near_mst_cost_distance_5km_","kebele","_results_",rsdp_type,"_regions_rm_elec.tex")))

# stargazer(lm_dmspols_k_el, 
#           lm_dmspols_basentl_k_el, 
#           lm_globcover_urban_k_el,
#           lm_globcover_urban_basentl_k_el, 
#           lm_globcover_crop_k_el, 
#           lm_globcover_crop_basentl_k_el,
#           
#           iv_cd_dmspols_regions_k_el %>% update_iv_coef_name(), 
#           iv_cd_dmspols_basentl_regions_k_el %>% update_iv_coef_name(), 
#           iv_cd_globcover_urban_regions_k_el %>% update_iv_coef_name(), 
#           iv_cd_globcover_urban_basentl_regions_k_el %>% update_iv_coef_name(), 
#           iv_cd_globcover_crop_regions_k_el %>% update_iv_coef_name(), 
#           iv_cd_globcover_crop_basentl_regions_k_el %>% update_iv_coef_name(),
#           
#           dep.var.labels.include = T,
#           dep.var.labels = c("NTL", "Urban", "Cropland",
#                              "NTL", "Urban", "Cropland"), #  "NTL $\\geq$ 2", "NTL $\\geq$ 6",
#           dep.var.caption = "",
#           omit = c("temp_avg", "precipitation", "distance_rsdp123_targettedlocs_log"),
#           covariate.labels = c("Imp Rd.",
#                                "Imp Rd.$\\times NTL_{96}$ Low",
#                                "Imp Rd.$\\times NTL_{96}$ Med",
#                                "Imp Rd.$\\times NTL_{96}$ High"),
#           omit.stat = c("f","ser", "rsq"),
#           align=TRUE,
#           no.space=TRUE,
#           float=FALSE,
#           column.sep.width="-15pt",
#           digits=2,
#           omit.table.layout = "n",
#           
#           add.lines =         add_lines <- list(
#             c("Type", rep("OLS", 6), rep("IV", 6)),
#             c("1st Stage F-Stat",
#               rep("N/A", 6),
#               
#               iv_cd_dmspols_regions_k_el$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
#               iv_cd_dmspols_basentl_regions_k_el$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
#               iv_cd_globcover_urban_regions_k_el$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
#               iv_cd_globcover_urban_basentl_regions_k_el$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
#               iv_cd_globcover_crop_regions_k_el$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
#               iv_cd_globcover_crop_basentl_regions_k_el$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM))
#           ),
#           out = file.path(paper_tables, 
#                           paste0("ols_iv_near_mst_cost_distance_5km_","kebele","_results_",rsdp_type,"_regions_rm_elec.tex"))
# )


# APPENDIX RESULTS TABLES ==================================================

# Make stargazer Function --------------------------------------------------
make_stargzer <- function(lm_dmspols_ihs_g,
                          lm_dmspols_ihs_basentl_g,
                          lm_globcover_urban_g,
                          lm_globcover_urban_basentl_g,
                          lm_globcover_crop_g,
                          lm_globcover_crop_basentl_g,
                          
                          lm_dmspols_ihs_k,
                          lm_dmspols_ihs_basentl_k,
                          lm_globcover_urban_k,
                          lm_globcover_urban_basentl_k,
                          lm_globcover_crop_k,
                          lm_globcover_crop_basentl_k,
                          iv_model,
                          file_name){
  
  ROUND_NUM = 2
  
  if(iv_model %in% T){
    #add_lines <- list(
    #   c("Unit", rep("1km", 6), rep("Keb.", 6)),
    #   c("1st Stage F-Stat",
    #     lm_dmspols_ihs_g$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
    #     lm_dmspols_ihs_basentl_g$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
    #     lm_globcover_urban_g$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
    #     lm_globcover_urban_basentl_g$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
    #     lm_globcover_crop_g$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
    #     lm_globcover_crop_basentl_g$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
    #     
    #     lm_dmspols_ihs_k$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
    #     lm_dmspols_ihs_basentl_k$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
    #     lm_globcover_urban_k$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
    #     lm_globcover_urban_basentl_k$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
    #     lm_globcover_crop_k$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
    #     lm_globcover_crop_basentl_k$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM))
    # )
    
    add_lines <- tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6, ~V7, ~V8, ~V9, ~V10, ~V11, ~V12,
                         'Unit', "1km","1km","1km","1km","1km","1km","Keb.","Keb.","Keb.","Keb.","Keb.","Keb.",
                         '1st Stage F-Stat',
                         fitstat(lm_dmspols_ihs_g, type = "ivf", simplify = T)$stat %>% round_char(),
                         fitstat(lm_dmspols_ihs_basentl_g, type = "ivf", simplify = T)$stat %>% round_char(),
                         fitstat(lm_globcover_urban_g, type = "ivf", simplify = T)$stat %>% round_char(),
                         fitstat(lm_globcover_urban_basentl_g, type = "ivf", simplify = T)$stat %>% round_char(),
                         fitstat(lm_globcover_crop_g, type = "ivf", simplify = T)$stat %>% round_char(),
                         fitstat(lm_globcover_crop_basentl_g, type = "ivf", simplify = T)$stat %>% round_char(),
                         
                         fitstat(lm_dmspols_ihs_k, type = "ivf", simplify = T)$stat %>% round_char(),
                         fitstat(lm_dmspols_ihs_basentl_k, type = "ivf", simplify = T)$stat %>% round_char(),
                         fitstat(lm_globcover_urban_k, type = "ivf", simplify = T)$stat %>% round_char(),
                         fitstat(lm_globcover_urban_basentl_k, type = "ivf", simplify = T)$stat %>% round_char(),
                         fitstat(lm_globcover_crop_k, type = "ivf", simplify = T)$stat %>% round_char(),
                         fitstat(lm_globcover_crop_basentl_k, type = "ivf", simplify = T)$stat %>% round_char())
    
  } else{
    # add_lines <- list(
    #   c("Unit", rep("1km", 6), rep("Keb.", 6))
    # )
    
    add_lines <- tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6, ~V7, ~V8, ~V9, ~V10, ~V11, ~V12,
                         'Unit', "1km","1km","1km","1km","1km","1km","Keb.","Keb.","Keb.","Keb.","Keb.","Keb.")
  }
  
  modelsummary_tab(list("NTL" = lm_dmspols_ihs_g, 
                    "NTL" = lm_dmspols_ihs_basentl_g, 
                    "Urban" = lm_globcover_urban_g,
                    "Urban" = lm_globcover_urban_basentl_g, 
                    "Cropland" = lm_globcover_crop_g, 
                    "Cropland" = lm_globcover_crop_basentl_g,
                    
                    "NTL" = lm_dmspols_ihs_k, 
                    "NTL" = lm_dmspols_ihs_basentl_k, 
                    "Urban" = lm_globcover_urban_k, 
                    "Urban" = lm_globcover_urban_basentl_k, 
                    "Cropland" = lm_globcover_crop_k, 
                    "Cropland" = lm_globcover_crop_basentl_k),
               stars = c('*' = .05, '**' = .01, "***" = 0.001),
               coef_map = c("near_rsdp123" = "Imp Rd",
                            "near_rsdp123Xdmspols_1996_bin4_2" = "Imp Rd.$\\times NTL_{96}$ Low",
                            "near_rsdp123Xdmspols_1996_bin4_3" = "Imp Rd.$\\times NTL_{96}$ Medium",
                            "near_rsdp123Xdmspols_1996_bin4_4" = "Imp Rd.$\\times NTL_{96}$ High",
                            "fit_near_rsdp123" = "Imp Rd",
                            "fit_near_rsdp123Xdmspols_1996_bin4_2" = "Imp Rd.$\\times NTL_{96}$ Low",
                            "fit_near_rsdp123Xdmspols_1996_bin4_3" = "Imp Rd.$\\times NTL_{96}$ Medium",
                            "fit_near_rsdp123Xdmspols_1996_bin4_4" = "Imp Rd.$\\times NTL_{96}$ High"),
               gof_map = c("nobs", "adj.r.squared"),
               escape = FALSE,
               add_rows = add_lines,
               output = file.path(paper_tables, file_name))
  
  # stargazer(lm_dmspols_ihs_g,
  #           lm_dmspols_ihs_basentl_g,
  #           lm_globcover_urban_g,
  #           lm_globcover_urban_basentl_g,
  #           lm_globcover_crop_g,
  #           lm_globcover_crop_basentl_g,
  #           
  #           lm_dmspols_ihs_k,
  #           lm_dmspols_ihs_basentl_k,
  #           lm_globcover_urban_k,
  #           lm_globcover_urban_basentl_k,
  #           lm_globcover_crop_k,
  #           lm_globcover_crop_basentl_k,
  #           
  #           dep.var.labels.include = T,
  #           dep.var.labels = c("NTL", "Urban", "Cropland",
  #                              "NTL", "Urban", "Cropland"), #  "NTL $\\geq$ 2", "NTL $\\geq$ 6",
  #           dep.var.caption = "",
  #           omit = c("temp_avg", "precipitation", "distance_rsdp123_targettedlocs_log"),
  #           covariate.labels = c("Imp Rd.",
  #                                "Imp Rd.$\\times NTL_{96}$ Low",
  #                                "Imp Rd.$\\times NTL_{96}$ Med",
  #                                "Imp Rd.$\\times NTL_{96}$ High"),
  #           omit.stat = c("f","ser", "rsq"),
  #           align=TRUE,
  #           no.space=TRUE,
  #           float=FALSE,
  #           column.sep.width="-15pt",
  #           digits=2,
  #           omit.table.layout = "n",
  #           add.lines = add_lines,
  #           out = file.path(paper_tables, 
  #                           file_name))
  
}

# OLS ------------------------------------------------------------------------
make_stargzer(lm_dmspols_ihs_g,
              lm_dmspols_ihs_basentl_g,
              lm_globcover_urban_g,
              lm_globcover_urban_basentl_g,
              lm_globcover_crop_g,
              lm_globcover_crop_basentl_g,
              
              lm_dmspols_ihs_k,
              lm_dmspols_ihs_basentl_k,
              lm_globcover_urban_k,
              lm_globcover_urban_basentl_k,
              lm_globcover_crop_k,
              lm_globcover_crop_basentl_k,
              F,
              paste0("ols_near_road_5km_","1kmkebele","_results_",rsdp_type,".tex"))

# MST - Cost Distance - Stargazer ----------------------------------------------
make_stargzer(iv_cd_dmspols_ihs_g,
              iv_cd_dmspols_ihs_basentl_g,
              iv_cd_globcover_urban_g,
              iv_cd_globcover_urban_basentl_g,
              iv_cd_globcover_crop_g,
              iv_cd_globcover_crop_basentl_g,
              
              iv_cd_dmspols_ihs_k,
              iv_cd_dmspols_ihs_basentl_k,
              iv_cd_globcover_urban_k,
              iv_cd_globcover_urban_basentl_k,
              iv_cd_globcover_crop_k,
              iv_cd_globcover_crop_basentl_k,
              T,
              paste0("iv_near_mst_cost_distance_5km_","1kmkebele","_results_",rsdp_type,".tex"))

# MST - Least Distance - Stargazer ---------------------------------------------
make_stargzer(iv_ld_dmspols_ihs_g,
              iv_ld_dmspols_ihs_basentl_g,
              iv_ld_globcover_urban_g,
              iv_ld_globcover_urban_basentl_g,
              iv_ld_globcover_crop_g,
              iv_ld_globcover_crop_basentl_g,
              
              iv_ld_dmspols_ihs_k,
              iv_ld_dmspols_ihs_basentl_k,
              iv_ld_globcover_urban_k,
              iv_ld_globcover_urban_basentl_k,
              iv_ld_globcover_crop_k,
              iv_ld_globcover_crop_basentl_k,
              T,
              paste0("iv_near_mst_least_distance_5km_","1kmkebele","_results_",rsdp_type,".tex"))

# MST Regions - Cost Distance - Stargazer ----------------------------------------------
make_stargzer(iv_cd_dmspols_ihs_regions_g,
              iv_cd_dmspols_ihs_basentl_regions_g,
              iv_cd_globcover_urban_regions_g,
              iv_cd_globcover_urban_basentl_regions_g,
              iv_cd_globcover_crop_regions_g,
              iv_cd_globcover_crop_basentl_regions_g,
              
              iv_cd_dmspols_ihs_regions_k,
              iv_cd_dmspols_ihs_basentl_regions_k,
              iv_cd_globcover_urban_regions_k,
              iv_cd_globcover_urban_basentl_regions_k,
              iv_cd_globcover_crop_regions_k,
              iv_cd_globcover_crop_basentl_regions_k,
              T,
              paste0("iv_near_mst_cost_distance_5km_","1kmkebele","_results_",rsdp_type,"_regions.tex"))

# MST Regions - Least Distance - Stargazer -------------------------------------
make_stargzer(iv_ld_dmspols_ihs_regions_g,
              iv_ld_dmspols_ihs_basentl_regions_g,
              iv_ld_globcover_urban_regions_g,
              iv_ld_globcover_urban_basentl_regions_g,
              iv_ld_globcover_crop_regions_g,
              iv_ld_globcover_crop_basentl_regions_g,
              
              iv_ld_dmspols_ihs_regions_k,
              iv_ld_dmspols_ihs_basentl_regions_k,
              iv_ld_globcover_urban_regions_k,
              iv_ld_globcover_urban_basentl_regions_k,
              iv_ld_globcover_crop_regions_k,
              iv_ld_globcover_crop_basentl_regions_k,
              T,
              paste0("iv_near_mst_least_distance_5km_","1kmkebele","_results_",rsdp_type,"_regions.tex"))

# First Stage - Stargazer ------------------------------------------------------
if(F){
  stargazer(iv_cd_dmspols_ihs_g$stage1,
            iv_ld_dmspols_ihs_g$stage1,
            iv_cd_dmspols_ihs_regions_g$stage1,
            iv_ld_dmspols_ihs_regions_g$stage1,
            
            iv_cd_dmspols_ihs_k$stage1,
            iv_ld_dmspols_ihs_k$stage1,
            iv_cd_dmspols_ihs_regions_k$stage1,
            iv_ld_dmspols_ihs_regions_k$stage1,
            dep.var.labels.include = T,
            dep.var.labels = c("Near Improved Road"),
            dep.var.caption = "",
            covariate.labels = c("Near Least Cost MST",
                                 "Near Min. Distance MST",
                                 "Near Least Cost MST: Regional",
                                 "Near Min. Distance MST: Regional"),
            omit = "distance_rsdp123_targettedlocs_log",
            omit.stat = c("f","ser", "rsq"),
            align=TRUE,
            no.space=TRUE,
            float=FALSE,
            column.sep.width="-15pt",
            digits=2,
            omit.table.layout = "n",
            add.lines = list(
              c("Unit", rep("1km", 4), rep("Keb.", 4)),
              c("1st Stage F-Stat", 
                lfe::waldtest(iv_cd_dmspols_ihs_g$stage1,         ~near_mst_lc,         lhs=iv_cd_dmspols_ihs_g$stage1$lhs)[5] %>% round(ROUND_NUM),
                lfe::waldtest(iv_ld_dmspols_ihs_g$stage1,         ~near_mst_euc,        lhs=iv_ld_dmspols_ihs_g$stage1$lhs)[5] %>% round(ROUND_NUM),
                lfe::waldtest(iv_cd_dmspols_ihs_regions_g$stage1, ~near_mst_lc_region,  lhs=iv_cd_dmspols_ihs_regions_g$stage1$lhs)[5] %>% round(ROUND_NUM),
                lfe::waldtest(iv_ld_dmspols_ihs_regions_g$stage1, ~near_mst_euc_region, lhs=iv_ld_dmspols_ihs_regions_g$stage1$lhs)[5] %>% round(ROUND_NUM),
                
                lfe::waldtest(iv_cd_dmspols_ihs_k$stage1,         ~near_mst_lc,         lhs=iv_cd_dmspols_ihs_k$stage1$lhs)[5] %>% round(ROUND_NUM),
                lfe::waldtest(iv_ld_dmspols_ihs_k$stage1,         ~near_mst_euc,        lhs=iv_ld_dmspols_ihs_k$stage1$lhs)[5] %>% round(ROUND_NUM),
                lfe::waldtest(iv_cd_dmspols_ihs_regions_k$stage1, ~near_mst_lc_region,  lhs=iv_cd_dmspols_ihs_regions_k$stage1$lhs)[5] %>% round(ROUND_NUM),
                lfe::waldtest(iv_ld_dmspols_ihs_regions_k$stage1, ~near_mst_euc_region, lhs=iv_ld_dmspols_ihs_regions_k$stage1$lhs)[5] %>% round(ROUND_NUM)
              )
            ),
            out = file.path(paper_tables, 
                            paste0("iv_near_mst_5km_1ststage_","1kmkebele","_results_",rsdp_type,"_regions.tex")))
}
# Cleanup ----------------------------------------------------------------------
OBJECTS_AFTER_CODE <- ls()
OBJECTS_TO_DELETE <- setdiff(OBJECTS_AFTER_CODE, OBJECTS_BEFORE_CODE)
rm(list = OBJECTS_TO_DELETE)

gc(); gc(); gc()

