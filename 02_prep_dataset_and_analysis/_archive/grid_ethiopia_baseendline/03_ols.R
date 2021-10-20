# OLS

# Load / Prep Data -------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_ethiopia",
                          "merged_datasets", "panel_data_clean.Rds"))

data$end2012sample <- data$year %in% c(1996, 2012)
data$end2016sample <- data$year %in% c(1996, 2016)

lm <- felm(dmspols_zhang_ihs ~ endline + near_anyimproved_by2012_5km:endline | cell_id | 0 | W_CODE, data = data[data$end2012sample %in% T,])


# Estimate Models --------------------------------------------------------------
## 2012 as Endline
lm_dmspols_zhang_ihs         <- felm(dmspols_zhang_ihs         ~ near_anyimproved_5km | 0 | 0 | W_CODE, data = df2012)
lm_dmspols_zhang_ihs_base0na <- felm(dmspols_zhang_ihs_base0na ~ near_anyimproved_5km | 0 | 0 | W_CODE, data = df2012)
lm_dmspols_zhang_2           <- felm(dmspols_zhang_2           ~ near_anyimproved_5km | 0 | 0 | W_CODE, data = df2012)
lm_dmspols_zhang_6           <- felm(dmspols_zhang_6           ~ near_anyimproved_5km | 0 | 0 | W_CODE, data = df2012)

## 2016 as Endline
lm_globcover_urban    <- felm(globcover_urban    ~ near_anyimproved_5km | 0 | 0 | W_CODE, data = df2016)
lm_globcover_cropland <- felm(globcover_cropland ~ near_anyimproved_5km | 0 | 0 | W_CODE, data = df2016)
lm_ndvi               <- felm(ndvi               ~ near_anyimproved_5km | 0 | 0 | W_CODE, data = df2016)
lm_ndvi_cropland      <- felm(ndvi_cropland      ~ near_anyimproved_5km | 0 | 0 | W_CODE, data = df2016)

# Stargazer --------------------------------------------------------------------
stargazer(lm_dmspols_zhang_ihs,
          lm_dmspols_zhang_ihs_base0na,
          lm_dmspols_zhang_2,
          lm_dmspols_zhang_6,
          lm_globcover_urban,
          lm_globcover_cropland,
          lm_ndvi,
          lm_ndvi_cropland,
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
          out = file.path(panel_rsdp_imp_data_file_path, 
                          "dmspols_grid_ethiopia", 
                          "outputs",
                          "tables",
                          "ols_near_mst_5km_results.tex"))





