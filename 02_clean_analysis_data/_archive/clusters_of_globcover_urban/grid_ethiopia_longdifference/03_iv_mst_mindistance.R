# Instrumental Variables

# Resources for IV in R
# https://rpubs.com/wsundstrom/t_ivreg
# http://eclr.humanities.manchester.ac.uk/index.php/IV_in_R

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

## Standardize improved variable names
# Do this so variable is the same row in the output table
df2012 <- df2012 %>%
  dplyr::rename(near_anyimproved_5km = near_anyimproved_by2012_5km)

df2016 <- df2016 %>%
  dplyr::rename(near_anyimproved_5km = near_anyimproved_ever_5km)

# Estimate Models --------------------------------------------------------------
## 2012 as Endline
iv_dmspols_zhang_ihs         <- felm(dmspols_zhang_ihs         ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_mindist_5km) | W_CODE, data = df2012)
iv_dmspols_zhang_ihs_base0na <- felm(dmspols_zhang_ihs_base0na ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_mindist_5km) | W_CODE, data = df2012)
iv_dmspols_zhang_2           <- felm(dmspols_zhang_2           ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_mindist_5km) | W_CODE, data = df2012)
iv_dmspols_zhang_6           <- felm(dmspols_zhang_6           ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_mindist_5km) | W_CODE, data = df2012)

## 2016 as Endline
iv_globcover_urban    <- felm(globcover_urban    ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_mindist_5km) | W_CODE, data = df2016)
iv_globcover_cropland <- felm(globcover_cropland ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_mindist_5km) | W_CODE, data = df2016)
iv_ndvi               <- felm(ndvi               ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_mindist_5km) | W_CODE, data = df2016)
iv_ndvi_cropland      <- felm(ndvi_cropland      ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_mindist_5km) | W_CODE, data = df2016)

# Stargazer --------------------------------------------------------------------
stargazer(iv_dmspols_zhang_ihs,
          iv_dmspols_zhang_ihs_base0na,
          iv_dmspols_zhang_2,
          iv_dmspols_zhang_6,
          iv_globcover_urban,
          iv_globcover_cropland,
          iv_ndvi,
          iv_ndvi_cropland,
          dep.var.labels.include = T,
          dep.var.labels = c("NTL (IHS)",
                             "NTL (IHS), Lit at Base.",
                             "NTL $>$ 2",
                             "NTL $>$ 6",
                             "Urban",
                             "Cropland",
                             "NDVI",
                             "NDVI, Crop"),
          dep.var.caption = "",
          covariate.labels = "Near Improved Rd.",
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          add.lines = list(
            c("1st Stage F-Stat", 
              waldtest(iv_dmspols_zhang_ihs$stage1,         ~near_mst_mindist_5km, lhs=iv_dmspols_zhang_ihs$stage1$lhs)[5] %>% round(ROUND_NUM),
              waldtest(iv_dmspols_zhang_ihs_base0na$stage1, ~near_mst_mindist_5km, lhs=iv_dmspols_zhang_ihs_base0na$stage1$lhs)[5] %>% round(ROUND_NUM),
              waldtest(iv_dmspols_zhang_2$stage1,           ~near_mst_mindist_5km, lhs=iv_dmspols_zhang_2$stage1$lhs)[5] %>% round(ROUND_NUM),
              waldtest(iv_dmspols_zhang_6$stage1,           ~near_mst_mindist_5km, lhs=iv_dmspols_zhang_6$stage1$lhs)[5] %>% round(ROUND_NUM),
              waldtest(iv_globcover_urban$stage1,           ~near_mst_mindist_5km, lhs=iv_globcover_urban$stage1$lhs)[5] %>% round(ROUND_NUM),
              waldtest(iv_globcover_cropland$stage1,        ~near_mst_mindist_5km, lhs=iv_globcover_cropland$stage1$lhs)[5] %>% round(ROUND_NUM),
              waldtest(iv_ndvi$stage1,                      ~near_mst_mindist_5km, lhs=iv_ndvi$stage1$lhs)[5] %>% round(ROUND_NUM),
              waldtest(iv_ndvi_cropland$stage1,             ~near_mst_mindist_5km, lhs=iv_ndvi_cropland$stage1$lhs)[5] %>% round(ROUND_NUM)
            )
          ),
          out = file.path(panel_rsdp_imp_data_file_path, 
                          "dmspols_grid_ethiopia", 
                          "outputs",
                          "tables",
                          "iv_near_mst_mindistance_5km_results.tex"))

### First Stage
stargazer(iv_dmspols_zhang_ihs$stage1,
          iv_dmspols_zhang_ihs_base0na$stage1,
          iv_dmspols_zhang_2$stage1,
          iv_dmspols_zhang_6$stage1,
          iv_globcover_urban$stage1,
          iv_globcover_cropland$stage1,
          iv_ndvi$stage1,
          iv_ndvi_cropland$stage1,
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
          add.lines = list(
            c("1st Stage F-Stat", 
              waldtest(iv_dmspols_zhang_ihs$stage1,         ~near_mst_mindist_5km, lhs=iv_dmspols_zhang_ihs$stage1$lhs)[5] %>% round(ROUND_NUM),
              waldtest(iv_dmspols_zhang_ihs_base0na$stage1, ~near_mst_mindist_5km, lhs=iv_dmspols_zhang_ihs_base0na$stage1$lhs)[5] %>% round(ROUND_NUM),
              waldtest(iv_dmspols_zhang_2$stage1,           ~near_mst_mindist_5km, lhs=iv_dmspols_zhang_2$stage1$lhs)[5] %>% round(ROUND_NUM),
              waldtest(iv_dmspols_zhang_6$stage1,           ~near_mst_mindist_5km, lhs=iv_dmspols_zhang_6$stage1$lhs)[5] %>% round(ROUND_NUM),
              waldtest(iv_globcover_urban$stage1,           ~near_mst_mindist_5km, lhs=iv_globcover_urban$stage1$lhs)[5] %>% round(ROUND_NUM),
              waldtest(iv_globcover_cropland$stage1,        ~near_mst_mindist_5km, lhs=iv_globcover_cropland$stage1$lhs)[5] %>% round(ROUND_NUM),
              waldtest(iv_ndvi$stage1,                      ~near_mst_mindist_5km, lhs=iv_ndvi$stage1$lhs)[5] %>% round(ROUND_NUM),
              waldtest(iv_ndvi_cropland$stage1,             ~near_mst_mindist_5km, lhs=iv_ndvi_cropland$stage1$lhs)[5] %>% round(ROUND_NUM)
            )
          ),
          out = file.path(panel_rsdp_imp_data_file_path, 
                          "dmspols_grid_ethiopia", 
                          "outputs",
                          "tables",
                          "iv_near_mst_mindistance_5km_1stage_results.tex"))







