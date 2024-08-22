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

to_delete <- paper_tables %>%
  list.files() %>%
  str_subset("^iv_near_|^ols_iv_near")
for(file_i in to_delete){
  file.remove(file_i)
}

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
  
  #### Shorten variable names to simplify code for regressions & standardize names
  # across grid & kebele data
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
    
    dplyr::mutate(near_rsdp123         = as.numeric(distance_rsdp123                <= NEAR_ROAD),
                  near_mst_euc         = as.numeric(distance_rsdp123_mst_euc        <= NEAR_ROAD),
                  near_mst_euc_region  = as.numeric(distance_rsdp123_mst_euc_region <= NEAR_ROAD),
                  near_mst_lc          = as.numeric(distance_rsdp123_mst_lc         <= NEAR_ROAD),
                  near_mst_lc_region   = as.numeric(distance_rsdp123_mst_lc_region  <= NEAR_ROAD),
                  
                  near_rsdp123Xwor_ntlgroup_2bin        = near_rsdp123 * wor_ntlgroup_2bin,
                  near_rsdp123Xdistance_city_addisababa = near_rsdp123 * distance_city_addisababa,
                  
                  near_mst_eucXwor_ntlgroup_2bin        = near_mst_euc * wor_ntlgroup_2bin,
                  near_mst_eucXdistance_city_addisababa = near_mst_euc * distance_city_addisababa,
                  
                  near_mst_euc_regionXwor_ntlgroup_2bin        = near_mst_euc_region * wor_ntlgroup_2bin,
                  near_mst_euc_regionXdistance_city_addisababa = near_mst_euc_region * distance_city_addisababa,
                  
                  near_mst_lcXwor_ntlgroup_2bin        = near_mst_lc * wor_ntlgroup_2bin,
                  near_mst_lcXdistance_city_addisababa = near_mst_lc * distance_city_addisababa,
                  
                  near_mst_lc_regionXwor_ntlgroup_2bin        = near_mst_lc_region * wor_ntlgroup_2bin,
                  near_mst_lc_regionXdistance_city_addisababa = near_mst_lc_region * distance_city_addisababa)
  
  return(df)
}

# Prep Data --------------------------------------------------------------------
df_grid   <- prep_data("dmspols_grid_ethiopia")
df_kebele <- prep_data("kebele")

# OLS --------------------------------------------------------------------------
# vcov = conley(50)

## Grid
lm_dmspols_ihs_g     <- feols(dv_dmspols ~ near_rsdp123 + distance_rsdp123_targettedlocs_log , vcov = conley(50), data = df_grid)
lm_globcover_urban_g <- feols(dv_gcurban ~ near_rsdp123 + distance_rsdp123_targettedlocs_log , vcov = conley(50), data = df_grid)
lm_globcover_crop_g  <- feols(dv_gccrop  ~ near_rsdp123 + distance_rsdp123_targettedlocs_log , vcov = conley(50), data = df_grid)

lm_dmspols_ihs_basentl_g     <- feols(dv_dmspols ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xwor_ntlgroup_2bin , vcov = conley(50), data = df_grid)
lm_globcover_urban_basentl_g <- feols(dv_gcurban ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xwor_ntlgroup_2bin , vcov = conley(50), data = df_grid)
lm_globcover_crop_basentl_g  <- feols(dv_gccrop  ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xwor_ntlgroup_2bin , vcov = conley(50), data = df_grid)

## Kebele
lm_dmspols_ihs_k     <- feols(dv_dmspols ~ near_rsdp123 + distance_rsdp123_targettedlocs_log , vcov = conley(50), data = df_kebele)
lm_globcover_urban_k <- feols(dv_gcurban ~ near_rsdp123 + distance_rsdp123_targettedlocs_log , vcov = conley(50), data = df_kebele)
lm_globcover_crop_k  <- feols(dv_gccrop  ~ near_rsdp123 + distance_rsdp123_targettedlocs_log , vcov = conley(50), data = df_kebele)

lm_dmspols_ihs_basentl_k     <- feols(dv_dmspols ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xwor_ntlgroup_2bin , vcov = conley(50), data = df_kebele)
lm_globcover_urban_basentl_k <- feols(dv_gcurban ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xwor_ntlgroup_2bin , vcov = conley(50), data = df_kebele)
lm_globcover_crop_basentl_k  <- feols(dv_gccrop  ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xwor_ntlgroup_2bin , vcov = conley(50), data = df_kebele)

# MST - Cost Distance --------------------------------------------------------
## Grid
iv_cd_dmspols_ihs_g     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 ~ near_mst_lc, vcov = conley(50), data = df_grid)
iv_cd_globcover_urban_g <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 ~ near_mst_lc, vcov = conley(50), data = df_grid)
iv_cd_globcover_crop_g  <- feols(dv_gccrop  ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 ~ near_mst_lc, vcov = conley(50), data = df_grid)

iv_cd_dmspols_ihs_basentl_g     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 + near_rsdp123Xwor_ntlgroup_2bin ~ near_mst_lc + near_mst_lcXwor_ntlgroup_2bin, vcov = conley(50), data = df_grid)
iv_cd_globcover_urban_basentl_g <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 + near_rsdp123Xwor_ntlgroup_2bin ~ near_mst_lc + near_mst_lcXwor_ntlgroup_2bin, vcov = conley(50), data = df_grid)
iv_cd_globcover_crop_basentl_g  <- feols(dv_gccrop  ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 + near_rsdp123Xwor_ntlgroup_2bin ~ near_mst_lc + near_mst_lcXwor_ntlgroup_2bin, vcov = conley(50), data = df_grid)

## Kebele
iv_cd_dmspols_ihs_k     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 ~ near_mst_lc, vcov = conley(50), data = df_kebele)
iv_cd_globcover_urban_k <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 ~ near_mst_lc, vcov = conley(50), data = df_kebele)
iv_cd_globcover_crop_k  <- feols(dv_gccrop  ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 ~ near_mst_lc, vcov = conley(50), data = df_kebele)

iv_cd_dmspols_ihs_basentl_k     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 + near_rsdp123Xwor_ntlgroup_2bin ~ near_mst_lc + near_mst_lcXwor_ntlgroup_2bin, vcov = conley(50), data = df_kebele)
iv_cd_globcover_urban_basentl_k <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 + near_rsdp123Xwor_ntlgroup_2bin ~ near_mst_lc + near_mst_lcXwor_ntlgroup_2bin, vcov = conley(50), data = df_kebele)
iv_cd_globcover_crop_basentl_k  <- feols(dv_gccrop  ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 + near_rsdp123Xwor_ntlgroup_2bin ~ near_mst_lc + near_mst_lcXwor_ntlgroup_2bin, vcov = conley(50), data = df_kebele)

# MST - Least Distance -------------------------------------------------------
## Grid
iv_ld_dmspols_ihs_g     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 ~ near_mst_euc, vcov = conley(50), data = df_grid)
iv_ld_globcover_urban_g <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 ~ near_mst_euc, vcov = conley(50), data = df_grid)
iv_ld_globcover_crop_g  <- feols(dv_gccrop  ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 ~ near_mst_euc, vcov = conley(50), data = df_grid)

iv_ld_dmspols_ihs_basentl_g     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 + near_rsdp123Xwor_ntlgroup_2bin ~ near_mst_euc + near_mst_eucXwor_ntlgroup_2bin, vcov = conley(50), data = df_grid)
iv_ld_globcover_urban_basentl_g <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 + near_rsdp123Xwor_ntlgroup_2bin ~ near_mst_euc + near_mst_eucXwor_ntlgroup_2bin, vcov = conley(50), data = df_grid)
iv_ld_globcover_crop_basentl_g  <- feols(dv_gccrop  ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 + near_rsdp123Xwor_ntlgroup_2bin ~ near_mst_euc + near_mst_eucXwor_ntlgroup_2bin, vcov = conley(50), data = df_grid)

## Kebele
iv_ld_dmspols_ihs_k     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 ~ near_mst_euc, vcov = conley(50), data = df_kebele)
iv_ld_globcover_urban_k <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 ~ near_mst_euc, vcov = conley(50), data = df_kebele)
iv_ld_globcover_crop_k  <- feols(dv_gccrop  ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 ~ near_mst_euc, vcov = conley(50), data = df_kebele)

iv_ld_dmspols_ihs_basentl_k     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 + near_rsdp123Xwor_ntlgroup_2bin ~ near_mst_euc + near_mst_eucXwor_ntlgroup_2bin, vcov = conley(50), data = df_kebele)
iv_ld_globcover_urban_basentl_k <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 + near_rsdp123Xwor_ntlgroup_2bin ~ near_mst_euc + near_mst_eucXwor_ntlgroup_2bin, vcov = conley(50), data = df_kebele)
iv_ld_globcover_crop_basentl_k  <- feols(dv_gccrop  ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 + near_rsdp123Xwor_ntlgroup_2bin ~ near_mst_euc + near_mst_eucXwor_ntlgroup_2bin, vcov = conley(50), data = df_kebele)

# MST Regions - Cost Distance ------------------------------------------------
## Grid
iv_cd_dmspols_ihs_regions_g     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 ~ near_mst_lc_region, vcov = conley(50), data = df_grid)
iv_cd_globcover_urban_regions_g <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 ~ near_mst_lc_region, vcov = conley(50), data = df_grid)
iv_cd_globcover_crop_regions_g  <- feols(dv_gccrop  ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 ~ near_mst_lc_region, vcov = conley(50), data = df_grid)

iv_cd_dmspols_ihs_basentl_regions_g     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 + near_rsdp123Xwor_ntlgroup_2bin ~ near_mst_lc_region + near_mst_lc_regionXwor_ntlgroup_2bin, vcov = conley(50), data = df_grid)
iv_cd_globcover_urban_basentl_regions_g <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 + near_rsdp123Xwor_ntlgroup_2bin ~ near_mst_lc_region + near_mst_lc_regionXwor_ntlgroup_2bin, vcov = conley(50), data = df_grid)
iv_cd_globcover_crop_basentl_regions_g  <- feols(dv_gccrop  ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 + near_rsdp123Xwor_ntlgroup_2bin ~ near_mst_lc_region + near_mst_lc_regionXwor_ntlgroup_2bin, vcov = conley(50), data = df_grid)

## Kebele
iv_cd_dmspols_ihs_regions_k     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 ~ near_mst_lc_region, vcov = conley(50), data = df_kebele)
iv_cd_globcover_urban_regions_k <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 ~ near_mst_lc_region, vcov = conley(50), data = df_kebele)
iv_cd_globcover_crop_regions_k  <- feols(dv_gccrop  ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 ~ near_mst_lc_region, vcov = conley(50), data = df_kebele)

iv_cd_dmspols_ihs_basentl_regions_k     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 + near_rsdp123Xwor_ntlgroup_2bin ~ near_mst_lc_region + near_mst_lc_regionXwor_ntlgroup_2bin, vcov = conley(50), data = df_kebele)
iv_cd_globcover_urban_basentl_regions_k <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 + near_rsdp123Xwor_ntlgroup_2bin ~ near_mst_lc_region + near_mst_lc_regionXwor_ntlgroup_2bin, vcov = conley(50), data = df_kebele)
iv_cd_globcover_crop_basentl_regions_k  <- feols(dv_gccrop  ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 + near_rsdp123Xwor_ntlgroup_2bin ~ near_mst_lc_region + near_mst_lc_regionXwor_ntlgroup_2bin, vcov = conley(50), data = df_kebele)

# MST Regions - Least Distance -------------------------------------------------------
## Grid
iv_ld_dmspols_ihs_regions_g     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 ~ near_mst_euc_region, vcov = conley(50), data = df_grid)
iv_ld_globcover_urban_regions_g <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 ~ near_mst_euc_region, vcov = conley(50), data = df_grid)
iv_ld_globcover_crop_regions_g  <- feols(dv_gccrop  ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 ~ near_mst_euc_region, vcov = conley(50), data = df_grid)

iv_ld_dmspols_ihs_basentl_regions_g     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 + near_rsdp123Xwor_ntlgroup_2bin ~ near_mst_euc_region + near_mst_euc_regionXwor_ntlgroup_2bin, vcov = conley(50), data = df_grid)
iv_ld_globcover_urban_basentl_regions_g <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 + near_rsdp123Xwor_ntlgroup_2bin ~ near_mst_euc_region + near_mst_euc_regionXwor_ntlgroup_2bin, vcov = conley(50), data = df_grid)
iv_ld_globcover_crop_basentl_regions_g  <- feols(dv_gccrop  ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 + near_rsdp123Xwor_ntlgroup_2bin ~ near_mst_euc_region + near_mst_euc_regionXwor_ntlgroup_2bin, vcov = conley(50), data = df_grid)

## Kebele
iv_ld_dmspols_ihs_regions_k     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 ~ near_mst_euc_region, vcov = conley(50), data = df_kebele)
iv_ld_globcover_urban_regions_k <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 ~ near_mst_euc_region, vcov = conley(50), data = df_kebele)
iv_ld_globcover_crop_regions_k  <- feols(dv_gccrop  ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 ~ near_mst_euc_region, vcov = conley(50), data = df_kebele)

iv_ld_dmspols_ihs_basentl_regions_k     <- feols(dv_dmspols ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 + near_rsdp123Xwor_ntlgroup_2bin ~ near_mst_euc_region + near_mst_euc_regionXwor_ntlgroup_2bin, vcov = conley(50), data = df_kebele)
iv_ld_globcover_urban_basentl_regions_k <- feols(dv_gcurban ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 + near_rsdp123Xwor_ntlgroup_2bin ~ near_mst_euc_region + near_mst_euc_regionXwor_ntlgroup_2bin, vcov = conley(50), data = df_kebele)
iv_ld_globcover_crop_basentl_regions_k  <- feols(dv_gccrop  ~ distance_rsdp123_targettedlocs_log |  near_rsdp123 + near_rsdp123Xwor_ntlgroup_2bin ~ near_mst_euc_region + near_mst_euc_regionXwor_ntlgroup_2bin, vcov = conley(50), data = df_kebele)

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
             stars = c('*' = .1, '**' = .05, "***" = 0.01),
             coef_map = c("near_rsdp123" = "Imp Rd",
                          "near_rsdp123Xwor_ntlgroup_2bin" = "Imp Rd.$\\times NTL_{96}$ Lit",
                          "fit_near_rsdp123" = "Imp Rd",
                          "fit_near_rsdp123Xwor_ntlgroup_2bin" = "Imp Rd.$\\times NTL_{96}$ Lit"),
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
                                paste0("ols_iv_near_mst_cost_distance_5km_","kebele","_results_",rsdp_type,"_regions_2ntlgroups.tex")))

# LOG vs IHS ===================================================================

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
  
  if(iv_model %in% T){
    
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
               stars = c('*' = .1, '**' = .05, "***" = 0.01),
               coef_map = c("near_rsdp123" = "Imp Rd",
                            "near_rsdp123Xwor_ntlgroup_2bin" = "Imp Rd.$\\times NTL_{96}$ Lit",
                            "fit_near_rsdp123" = "Imp Rd",
                            "fit_near_rsdp123Xwor_ntlgroup_2bin" = "Imp Rd.$\\times NTL_{96}$ Lit"),
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
  #                                "Imp Rd.$\\times NTL_{96}$ Lit"),
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
              paste0("ols_near_road_5km_","1kmkebele","_results_",rsdp_type,"_2ntlgroups.tex"))

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
              paste0("iv_near_mst_cost_distance_5km_","1kmkebele","_results_",rsdp_type,"_2ntlgroups.tex"))

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
              paste0("iv_near_mst_least_distance_5km_","1kmkebele","_results_",rsdp_type,"_2ntlgroups.tex"))

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
              paste0("iv_near_mst_cost_distance_5km_","1kmkebele","_results_",rsdp_type,"_regions_2ntlgroups.tex"))

# MST Regions - Least Distance - Stargazer ---------------------------------------------
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
              paste0("iv_near_mst_least_distance_5km_","1kmkebele","_results_",rsdp_type,"_regions_2ntlgroups.tex"))

# First Stage - Stargazer --------------------------------------
if(F){
  # modelsummary_tab(list("NTL" = lm_dmspols_ihs_k, 
  #                   "NTL" = lm_dmspols_ihs_basentl_k, 
  #                   "Urban" = lm_globcover_urban_k,
  #                   "Urban" = lm_globcover_urban_basentl_k, 
  #                   "Cropland" = lm_globcover_crop_k, 
  #                   "Cropland" = lm_globcover_crop_basentl_k,
  #                   
  #                   "NTL" = iv_cd_dmspols_ihs_regions_k, 
  #                   "NTL" = iv_cd_dmspols_ihs_basentl_regions_k, 
  #                   "Urban" = iv_cd_globcover_urban_regions_k, 
  #                   "Urban" = iv_cd_globcover_urban_basentl_regions_k, 
  #                   "Cropland" = iv_cd_globcover_crop_regions_k, 
  #                   "Cropland" = iv_cd_globcover_crop_basentl_regions_k),
  #              stars = c('*' = .1, '**' = .05, "***" = 0.01),
  #              coef_map = c("near_rsdp123" = "Imp Rd",
  #                           "near_rsdp123Xwor_ntlgroup_2bin" = "Imp Rd.$\\times NTL_{96}$ Lit",
  #                           "fit_near_rsdp123" = "Imp Rd",
  #                           "fit_near_rsdp123Xwor_ntlgroup_2bin" = "Imp Rd.$\\times NTL_{96}$ Lit"),
  #              gof_map = c("nobs", "adj.r.squared"),
  #              escape = FALSE,
  #              add_rows = tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6, ~V7, ~V8, ~V9, ~V10, ~V11, ~V12,
  #                                 'Type', "OLS","OLS","OLS","OLS","OLS","OLS","IV","IV","IV","IV","IV","IV",
  #                                 '1st Stage F-Stat',"NA","NA","NA","NA","NA","NA",
  #                                 fitstat(iv_cd_dmspols_ihs_regions_k, type = "ivf", simplify = T)$stat %>% round_char(),
  #                                 fitstat(iv_cd_dmspols_ihs_basentl_regions_k, type = "ivf", simplify = T)$stat %>% round_char(),
  #                                 fitstat(iv_cd_globcover_urban_regions_k, type = "ivf", simplify = T)$stat %>% round_char(),
  #                                 fitstat(iv_cd_globcover_urban_basentl_regions_k, type = "ivf", simplify = T)$stat %>% round_char(),
  #                                 fitstat(iv_cd_globcover_crop_regions_k, type = "ivf", simplify = T)$stat %>% round_char(),
  #                                 fitstat(iv_cd_globcover_crop_basentl_regions_k, type = "ivf", simplify = T)$stat %>% round_char()),
  #              output = file.path(paper_tables, 
  #                                 paste0("ols_iv_near_mst_cost_distance_5km_","kebele","_results_",rsdp_type,"_regions_2ntlgroups.tex")))
  # 
  
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
                            paste0("iv_near_mst_5km_1ststage_","1kmkebele","_results_",rsdp_type,"_regions_2ntlgroups.tex")))
}

# Cleanup ----------------------------------------------------------------------
OBJECTS_AFTER_CODE <- ls()
OBJECTS_TO_DELETE <- setdiff(OBJECTS_AFTER_CODE, OBJECTS_BEFORE_CODE)
rm(list = OBJECTS_TO_DELETE)

gc(); gc(); gc()





