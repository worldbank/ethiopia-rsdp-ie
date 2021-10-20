# Instrumental Variables

# Resources for IV in R
# https://rpubs.com/wsundstrom/t_ivreg
# http://eclr.humanities.manchester.ac.uk/index.php/IV_in_R
# MA_varXdmspols_zhang_sum2_ihs_1996

ROUND_NUM <- 1 # number of digits to round numbers

# Load Data --------------------------------------------------------------------
# Dataframe using 2012 as endline
df2012 <- readRDS(file.path(panel_rsdp_imp_data_file_path, 
                            "dmspols_grid_ethiopia", 
                            "merged_datasets", 
                            paste0("longdiff_data_clean_base", 
                                   1996,
                                   "_end",
                                   2012,
                                   ".Rds")))

# Dataframe using 2016 as endline
df2016 <- readRDS(file.path(panel_rsdp_imp_data_file_path, 
                            "dmspols_grid_ethiopia", 
                            "merged_datasets", 
                            paste0("longdiff_data_clean_base", 
                                   1996,
                                   "_end",
                                   2016,
                                   ".Rds")))

# sum12 <- data %>%
#   filter(year == 2012) %>%
#   group_by(near_anyimproved_ever_5km) %>%
#   dplyr::summarise(dmspols_0 = sum(dmspols > 0, na.rm = T),
#                    dmspols_2 = sum(dmspols >= 2, na.rm = T),
#                    dmspols_6 = sum(dmspols >= 6, na.rm = T),
#                    dmspols_10 = sum(dmspols >= 10, na.rm = T))
# 
# sum16 <- data %>%
#   filter(year == 2016) %>%
#   group_by(near_anyimproved_ever_5km) %>%
#   dplyr::summarise(dmspols_0 = sum(dmspols_harmon > 0, na.rm = T),
#                    dmspols_2 = sum(dmspols_harmon >= 2, na.rm = T),
#                    dmspols_6 = sum(dmspols_harmon >= 6, na.rm = T),
#                    dmspols_10 = sum(dmspols_harmon >= 10, na.rm = T))

## NTL Baseline Bins
#data <- data %>%
#  group_by(cell_id) %>%
#  dplyr::mutate(dmspols_sum2_1996 = dmspols_sum2[year== 1996],
#                dmspols_sum6_1996 = dmspols_sum6[year== 1996]) %>%
#  ungroup()

# Prep Variables ---------------------------------------------------------------

## Standardize improved variable names
# Do this so variable is the same row in the output table
df2012 <- df2012 %>%
  dplyr::rename(near_anyimproved_5km = near_anyimproved_by2012_5km)

df2016 <- df2016 %>%
  dplyr::rename(near_anyimproved_5km = near_anyimproved_ever_5km)

## Distance Addis - Units of 100km
df2012$distance_city_addisababa <- (df2012$distance_city_addisababa/1000)/100
df2016$distance_city_addisababa <- (df2016$distance_city_addisababa/1000)/100

## Interactions
df2012 <- df2012 %>%
  mutate(near_anyimproved_5kmXdistance_city_addisababa      = near_anyimproved_5km * distance_city_addisababa,
         #near_anyimproved_5kmXdmspols_zhang_ihs_1996_woreda = near_anyimproved_5km * dmspols_zhang_ihs_1996_woreda,
         #near_anyimproved_5kmXdmspols_zhang_ihs_sum2_1996_woreda = near_anyimproved_5km * dmspols_zhang_ihs_sum2_1996_woreda,
         near_anyimproved_5kmXdmspols_1996_bin4_1 = near_anyimproved_5km * dmspols_1996_bin4_1,
         near_anyimproved_5kmXdmspols_1996_bin4_2 = near_anyimproved_5km * dmspols_1996_bin4_2,
         near_anyimproved_5kmXdmspols_1996_bin4_3 = near_anyimproved_5km * dmspols_1996_bin4_3,
         near_anyimproved_5kmXdmspols_1996_bin4_4 = near_anyimproved_5km * dmspols_1996_bin4_4,
         
         near_mst_5kmXdistance_city_addisababa      = near_mst_5km * distance_city_addisababa,
         #near_mst_5kmXdmspols_zhang_ihs_1996_woreda = near_mst_5km * dmspols_zhang_ihs_1996_woreda,
         #near_mst_5kmXdmspols_zhang_ihs_sum2_1996_woreda = near_mst_5km * dmspols_zhang_ihs_sum2_1996_woreda,
         near_mst_5kmXdmspols_1996_bin4_1 = near_mst_5km * dmspols_1996_bin4_1,
         near_mst_5kmXdmspols_1996_bin4_2 = near_mst_5km * dmspols_1996_bin4_2,
         near_mst_5kmXdmspols_1996_bin4_3 = near_mst_5km * dmspols_1996_bin4_3,
         near_mst_5kmXdmspols_1996_bin4_4 = near_mst_5km * dmspols_1996_bin4_4,
         
         near_mst_mindist_5kmXdistance_city_addisababa      = near_mst_mindist_5km * distance_city_addisababa,
         #near_mst_mindist_5kmXdmspols_zhang_ihs_1996_woreda = near_mst_mindist_5km * dmspols_zhang_ihs_1996_woreda,
         #near_mst_mindist_5kmXdmspols_zhang_ihs_sum2_1996_woreda = near_mst_mindist_5km * dmspols_zhang_ihs_sum2_1996_woreda,
         near_mst_mindist_5kmXdmspols_1996_bin4_1 = near_mst_mindist_5km * dmspols_1996_bin4_1,
         near_mst_mindist_5kmXdmspols_1996_bin4_2 = near_mst_mindist_5km * dmspols_1996_bin4_2,
         near_mst_mindist_5kmXdmspols_1996_bin4_3 = near_mst_mindist_5km * dmspols_1996_bin4_3,
         near_mst_mindist_5kmXdmspols_1996_bin4_4 = near_mst_mindist_5km * dmspols_1996_bin4_4)

df2016 <- df2016 %>%
  mutate(near_anyimproved_5kmXdistance_city_addisababa      = near_anyimproved_5km * distance_city_addisababa,
         #near_anyimproved_5kmXdmspols_zhang_ihs_1996_woreda = near_anyimproved_5km * dmspols_zhang_ihs_1996_woreda,
         #near_anyimproved_5kmXdmspols_zhang_ihs_sum2_1996_woreda = near_anyimproved_5km * dmspols_zhang_ihs_sum2_1996_woreda,
         near_anyimproved_5kmXdmspols_1996_bin4_1 = near_anyimproved_5km * dmspols_1996_bin4_1,
         near_anyimproved_5kmXdmspols_1996_bin4_2 = near_anyimproved_5km * dmspols_1996_bin4_2,
         near_anyimproved_5kmXdmspols_1996_bin4_3 = near_anyimproved_5km * dmspols_1996_bin4_3,
         near_anyimproved_5kmXdmspols_1996_bin4_4 = near_anyimproved_5km * dmspols_1996_bin4_4,
         
         near_mst_5kmXdistance_city_addisababa      = near_mst_5km * distance_city_addisababa,
         #near_mst_5kmXdmspols_zhang_ihs_1996_woreda = near_mst_5km * dmspols_zhang_ihs_1996_woreda,
         #near_mst_5kmXdmspols_zhang_ihs_sum2_1996_woreda = near_mst_5km * dmspols_zhang_ihs_sum2_1996_woreda,
         near_mst_5kmXdmspols_1996_bin4_1 = near_mst_5km * dmspols_1996_bin4_1,
         near_mst_5kmXdmspols_1996_bin4_2 = near_mst_5km * dmspols_1996_bin4_2,
         near_mst_5kmXdmspols_1996_bin4_3 = near_mst_5km * dmspols_1996_bin4_3,
         near_mst_5kmXdmspols_1996_bin4_4 = near_mst_5km * dmspols_1996_bin4_4,
         
         near_mst_mindist_5kmXdistance_city_addisababa      = near_mst_mindist_5km * distance_city_addisababa,
         #near_mst_mindist_5kmXdmspols_zhang_ihs_1996_woreda = near_mst_mindist_5km * dmspols_zhang_ihs_1996_woreda,
         #near_mst_mindist_5kmXdmspols_zhang_ihs_sum2_1996_woreda = near_mst_mindist_5km * dmspols_zhang_ihs_sum2_1996_woreda,
         near_mst_mindist_5kmXdmspols_1996_bin4_1 = near_mst_mindist_5km * dmspols_1996_bin4_1,
         near_mst_mindist_5kmXdmspols_1996_bin4_2 = near_mst_mindist_5km * dmspols_1996_bin4_2,
         near_mst_mindist_5kmXdmspols_1996_bin4_3 = near_mst_mindist_5km * dmspols_1996_bin4_3,
         near_mst_mindist_5kmXdmspols_1996_bin4_4 = near_mst_mindist_5km * dmspols_1996_bin4_4)

# OLS --------------------------------------------------------------------------
lm_dmspols_ihs <- felm(dmspols_harmon_ihs ~ near_anyimproved_5km | 0 | 0 | W_CODE, data = df2016)
#lm_dmspols_2   <- felm(dmspols_harmon_2   ~ near_anyimproved_5km | 0 | 0 | W_CODE, data = df2016)
lm_globcover_urban <- felm(globcover_urban   ~ near_anyimproved_5km | 0 | 0 | W_CODE, data = df2016)
lm_globcover_crop  <- felm(globcover_cropland   ~ near_anyimproved_5km | 0 | 0 | W_CODE, data = df2016)

lm_dmspols_ihs_addis <- felm(dmspols_harmon_ihs ~ near_anyimproved_5km + near_anyimproved_5kmXdistance_city_addisababa | 0 | 0 | W_CODE, data = df2016)
#lm_dmspols_2_addis   <- felm(dmspols_harmon_2   ~ near_anyimproved_5km + near_anyimproved_5kmXdistance_city_addisababa | 0 | 0 | W_CODE, data = df2016)
lm_globcover_urban_addis   <- felm(globcover_urban   ~ near_anyimproved_5km + near_anyimproved_5kmXdistance_city_addisababa | 0 | 0 | W_CODE, data = df2016)
lm_globcover_crop_addis   <- felm(globcover_cropland   ~ near_anyimproved_5km + near_anyimproved_5kmXdistance_city_addisababa | 0 | 0 | W_CODE, data = df2016)

lm_dmspols_ihs_basentl <- felm(dmspols_harmon_ihs ~ near_anyimproved_5km + near_anyimproved_5kmXdmspols_1996_bin4_2 + near_anyimproved_5kmXdmspols_1996_bin4_3 + near_anyimproved_5kmXdmspols_1996_bin4_4 | 0 | 0 | W_CODE, data = df2016)
#lm_dmspols_2_basentl   <- felm(dmspols_harmon_2   ~ near_anyimproved_5km + near_anyimproved_5kmXdmspols_1996_bin4_2 + near_anyimproved_5kmXdmspols_1996_bin4_3 + near_anyimproved_5kmXdmspols_1996_bin4_4 | 0 | 0 | W_CODE, data = df2016)
lm_globcover_urban_basentl   <- felm(globcover_urban   ~ near_anyimproved_5km + near_anyimproved_5kmXdmspols_1996_bin4_2 + near_anyimproved_5kmXdmspols_1996_bin4_3 + near_anyimproved_5kmXdmspols_1996_bin4_4 | 0 | 0 | W_CODE, data = df2016)
lm_globcover_crop_basentl   <- felm(globcover_cropland   ~ near_anyimproved_5km + near_anyimproved_5kmXdmspols_1996_bin4_2 + near_anyimproved_5kmXdmspols_1996_bin4_3 + near_anyimproved_5kmXdmspols_1996_bin4_4 | 0 | 0 | W_CODE, data = df2016)

# MST - Cost Distance ----------------------------------------------------------
iv_cd_dmspols_ihs <- felm(dmspols_harmon_ihs ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_5km) | W_CODE, data = df2016)
#iv_cd_dmspols_2   <- felm(dmspols_harmon_2   ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_5km) | W_CODE, data = df2016)
iv_cd_globcover_urban   <- felm(globcover_urban   ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_5km) | W_CODE, data = df2016)
iv_cd_globcover_crop  <- felm(globcover_cropland   ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_5km) | W_CODE, data = df2016)

iv_cd_dmspols_ihs_addis <- felm(dmspols_harmon_ihs ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdistance_city_addisababa ~ near_mst_5km + near_mst_5kmXdistance_city_addisababa) | W_CODE, data = df2016)
#iv_cd_dmspols_2_addis   <- felm(dmspols_harmon_2   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdistance_city_addisababa ~ near_mst_5km + near_mst_5kmXdistance_city_addisababa) | W_CODE, data = df2016)
iv_cd_globcover_urban_addis   <- felm(globcover_urban   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdistance_city_addisababa ~ near_mst_5km + near_mst_5kmXdistance_city_addisababa) | W_CODE, data = df2016)
iv_cd_globcover_crop_addis   <- felm(globcover_cropland   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdistance_city_addisababa ~ near_mst_5km + near_mst_5kmXdistance_city_addisababa) | W_CODE, data = df2016)

iv_cd_dmspols_ihs_basentl <- felm(dmspols_harmon_ihs ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdmspols_1996_bin4_2|near_anyimproved_5kmXdmspols_1996_bin4_3|near_anyimproved_5kmXdmspols_1996_bin4_4 ~ near_mst_5km + near_mst_5kmXdmspols_1996_bin4_2 + near_mst_5kmXdmspols_1996_bin4_3 + near_mst_5kmXdmspols_1996_bin4_4) | W_CODE, data = df2016)
#iv_cd_dmspols_2_basentl   <- felm(dmspols_harmon_2   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdmspols_1996_bin4_2|near_anyimproved_5kmXdmspols_1996_bin4_3|near_anyimproved_5kmXdmspols_1996_bin4_4 ~ near_mst_5km + near_mst_5kmXdmspols_1996_bin4_2 + near_mst_5kmXdmspols_1996_bin4_3 + near_mst_5kmXdmspols_1996_bin4_4) | W_CODE, data = df2016)
iv_cd_globcover_urban_basentl   <- felm(globcover_urban   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdmspols_1996_bin4_2|near_anyimproved_5kmXdmspols_1996_bin4_3|near_anyimproved_5kmXdmspols_1996_bin4_4 ~ near_mst_5km + near_mst_5kmXdmspols_1996_bin4_2 + near_mst_5kmXdmspols_1996_bin4_3 + near_mst_5kmXdmspols_1996_bin4_4) | W_CODE, data = df2016)
iv_cd_globcover_crop_basentl   <- felm(globcover_cropland   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdmspols_1996_bin4_2|near_anyimproved_5kmXdmspols_1996_bin4_3|near_anyimproved_5kmXdmspols_1996_bin4_4 ~ near_mst_5km + near_mst_5kmXdmspols_1996_bin4_2 + near_mst_5kmXdmspols_1996_bin4_3 + near_mst_5kmXdmspols_1996_bin4_4) | W_CODE, data = df2016)

# MST - Least Distance ---------------------------------------------------------
iv_ld_dmspols_ihs <- felm(dmspols_harmon_ihs ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_mindist_5km) | W_CODE, data = df2016)
#iv_ld_dmspols_2   <- felm(dmspols_harmon_2   ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_mindist_5km) | W_CODE, data = df2016)
iv_ld_globcover_urban   <- felm(globcover_urban   ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_mindist_5km) | W_CODE, data = df2016)
iv_ld_globcover_crop   <- felm(globcover_cropland   ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_mindist_5km) | W_CODE, data = df2016)

iv_ld_dmspols_ihs_addis <- felm(dmspols_harmon_ihs ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdistance_city_addisababa ~ near_mst_mindist_5km + near_mst_mindist_5kmXdistance_city_addisababa) | W_CODE, data = df2016)
#iv_ld_dmspols_2_addis   <- felm(dmspols_harmon_2   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdistance_city_addisababa ~ near_mst_mindist_5km + near_mst_mindist_5kmXdistance_city_addisababa) | W_CODE, data = df2016)
iv_ld_globcover_urban_addis   <- felm(globcover_urban   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdistance_city_addisababa ~ near_mst_mindist_5km + near_mst_mindist_5kmXdistance_city_addisababa) | W_CODE, data = df2016)
iv_ld_globcover_crop_addis   <- felm(globcover_cropland   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdistance_city_addisababa ~ near_mst_mindist_5km + near_mst_mindist_5kmXdistance_city_addisababa) | W_CODE, data = df2016)

iv_ld_dmspols_ihs_basentl <- felm(dmspols_harmon_ihs ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdmspols_1996_bin4_2|near_anyimproved_5kmXdmspols_1996_bin4_3|near_anyimproved_5kmXdmspols_1996_bin4_4 ~ near_mst_mindist_5km + near_mst_mindist_5kmXdmspols_1996_bin4_2 + near_mst_mindist_5kmXdmspols_1996_bin4_3 + near_mst_mindist_5kmXdmspols_1996_bin4_4) | W_CODE, data = df2016)
#iv_ld_dmspols_2_basentl   <- felm(dmspols_harmon_2   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdmspols_1996_bin4_2|near_anyimproved_5kmXdmspols_1996_bin4_3|near_anyimproved_5kmXdmspols_1996_bin4_4 ~ near_mst_mindist_5km + near_mst_mindist_5kmXdmspols_1996_bin4_2 + near_mst_mindist_5kmXdmspols_1996_bin4_3 + near_mst_mindist_5kmXdmspols_1996_bin4_4) | W_CODE, data = df2016)
iv_ld_globcover_urban_basentl   <- felm(globcover_urban   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdmspols_1996_bin4_2|near_anyimproved_5kmXdmspols_1996_bin4_3|near_anyimproved_5kmXdmspols_1996_bin4_4 ~ near_mst_mindist_5km + near_mst_mindist_5kmXdmspols_1996_bin4_2 + near_mst_mindist_5kmXdmspols_1996_bin4_3 + near_mst_mindist_5kmXdmspols_1996_bin4_4) | W_CODE, data = df2016)
iv_ld_globcover_crop_basentl   <- felm(globcover_cropland   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdmspols_1996_bin4_2|near_anyimproved_5kmXdmspols_1996_bin4_3|near_anyimproved_5kmXdmspols_1996_bin4_4 ~ near_mst_mindist_5km + near_mst_mindist_5kmXdmspols_1996_bin4_2 + near_mst_mindist_5kmXdmspols_1996_bin4_3 + near_mst_mindist_5kmXdmspols_1996_bin4_4) | W_CODE, data = df2016)

# OLS - Stargazer --------------------------------------------------------------
stargazer(lm_dmspols_ihs,
          lm_dmspols_ihs_basentl,
          lm_dmspols_ihs_addis,
          
          #lm_dmspols_2,
          #lm_dmspols_2_basentl,
          #lm_dmspols_2_addis,
          
          #lm_dmspols_6,
          #lm_dmspols_6_basentl,
          #lm_dmspols_6_addis,
          
          lm_globcover_urban,
          lm_globcover_urban_basentl,
          lm_globcover_urban_addis,
          
          lm_globcover_crop,
          lm_globcover_crop_basentl,
          lm_globcover_crop_addis,
          
          dep.var.labels.include = T,
          dep.var.labels = c("NTL", "Urban", "Cropland"), #  "NTL $\\geq$ 2", "NTL $\\geq$ 6",
          dep.var.caption = "",
          omit = c("temp_avg", "precipitation"),
          covariate.labels = c("Imp Rd.",
                               "Imp Rd.$\\times NTL_{96}$ Low",
                               "Imp Rd.$\\times NTL_{96}$ Med",
                               "Imp Rd.$\\times NTL_{96}$ High",
                               "Imp Rd. X Dist Addis"),
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          omit.table.layout = "n",
          out = file.path(paper_tables, 
                          "ols_near_road_5km_eth_grid_eth_grid_results.tex"))

# MST - Cost Distance - Stargazer ----------------------------------------------
stargazer(iv_cd_dmspols_ihs,
          iv_cd_dmspols_ihs_basentl,
          iv_cd_dmspols_ihs_addis,
          
          #iv_cd_dmspols_2,
          #iv_cd_dmspols_2_basentl,
          #iv_cd_dmspols_2_addis,
          
          #iv_cd_dmspols_6,
          #iv_cd_dmspols_6_basentl,
          #iv_cd_dmspols_6_addis,
          
          iv_cd_globcover_urban,
          iv_cd_globcover_urban_basentl,
          iv_cd_globcover_urban_addis,
          
          iv_cd_globcover_crop,
          iv_cd_globcover_crop_basentl,
          iv_cd_globcover_crop_addis,
          
          dep.var.labels.include = T,
          dep.var.labels = c("NTL", "Urban", "Crop"), # "NTL $\\geq$ 2", "NTL $\\geq$ 6",
          dep.var.caption = "",
          covariate.labels = c("Imp Rd.",
                               "Imp Rd.$\\times NTL_{96}$ Low",
                               "Imp Rd.$\\times NTL_{96}$ Med",
                               "Imp Rd.$\\times NTL_{96}$ High",
                               "Imp Rd. X Dist Addis"),
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          omit.table.layout = "n",
          out = file.path(paper_tables,
                          "iv_near_mst_cost_distance_5km_eth_grid_results.tex"))

# MST - Least Distance - Stargazer ---------------------------------------------
stargazer(iv_ld_dmspols_ihs,
          iv_ld_dmspols_ihs_basentl,
          iv_ld_dmspols_ihs_addis,
          
          #iv_ld_dmspols_2,
          #iv_ld_dmspols_2_basentl,
          #iv_ld_dmspols_2_addis,
          
          #iv_ld_dmspols_6,
          #iv_ld_dmspols_6_basentl,
          #iv_ld_dmspols_6_addis,
          
          iv_ld_globcover_urban,
          iv_ld_globcover_urban_basentl,
          iv_ld_globcover_urban_addis,
          
          iv_ld_globcover_crop,
          iv_ld_globcover_crop_basentl,
          iv_ld_globcover_crop_addis,
          
          dep.var.labels.include = T,
          dep.var.labels = c("NTL", "Urban", "Cropland"), # "NTL $\\geq$ 2", "NTL $\\geq$ 6", 
          dep.var.caption = "",
          covariate.labels = c("Imp Rd.",
                               "Imp Rd.$\\times NTL_{96}$ Low",
                               "Imp Rd.$\\times NTL_{96}$ Med",
                               "Imp Rd.$\\times NTL_{96}$ High",
                               "Imp Rd. X Dist Addis"),
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          omit.table.layout = "n",
          out = file.path(paper_tables, 
                          "iv_near_mst_least_distance_5km_eth_grid_results.tex"))

# First Stage - Cost Distance - Stargazer --------------------------------------
stargazer(iv_cd_dmspols_ihs$stage1,
          dep.var.labels.include = T,
          dep.var.labels = c("Near Improved Road"),
          dep.var.caption = "",
          covariate.labels = "Near MST",
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          omit.table.layout = "n",
          add.lines = list(
            c("1st Stage F-Stat", 
              lfe::waldtest(iv_cd_dmspols_ihs$stage1, ~near_mst_5km, lhs=iv_cd_dmspols_ihs$stage1$lhs)[5] %>% round(ROUND_NUM)
            )
          ),
          out = file.path(paper_tables, 
                          "iv_near_mst_cost_distance_5km_1ststage_eth_grid_results.tex"))

# IV - First Stage - Stargazer -------------------------------------------------
stargazer(iv_cd_dmspols_ihs$stage1,
          dep.var.labels.include = T,
          dep.var.labels = c("Near Improved Road"),
          dep.var.caption = "",
          covariate.labels = "Near MST",
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          omit.table.layout = "n",
          add.lines = list(
            c("1st Stage F-Stat", 
              lfe::waldtest(iv_cd_dmspols_ihs$stage1, ~near_mst_5km, lhs=iv_cd_dmspols_ihs$stage1$lhs)[5] %>% round(ROUND_NUM)
            )
          ),
          out = file.path(paper_tables, 
                          "iv_near_mst_cost_distance_5km_1ststage_eth_grid_results.tex"))

stargazer(iv_ld_dmspols_ihs$stage1,
          dep.var.labels.include = T,
          dep.var.labels = c("Near Improved Road"),
          dep.var.caption = "",
          covariate.labels = "Near MST",
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          omit.table.layout = "n",
          add.lines = list(
            c("1st Stage F-Stat", 
              lfe::waldtest(iv_ld_dmspols_ihs$stage1, ~near_mst_mindist_5km, lhs=iv_ld_dmspols_ihs$stage1$lhs)[5] %>% round(ROUND_NUM)
            )
          ),
          out = file.path(paper_tables, 
                          "iv_near_mst_least_distance_5km_1ststage_eth_grid_results.tex"))


