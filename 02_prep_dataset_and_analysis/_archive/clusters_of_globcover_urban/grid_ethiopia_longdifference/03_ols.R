# Instrumental Variables

# Resources for IV in R
# https://rpubs.com/wsundstrom/t_ivreg
# http://eclr.humanities.manchester.ac.uk/index.php/IV_in_R

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
          lm_dmspols_zhang_2,
          lm_dmspols_zhang_6,
          lm_globcover_urban,
          dep.var.labels.include = T,
          dep.var.labels = c("NTL (IHS)",
                             "NTL $>$ 2",
                             "NTL $>$ 6",
                             "Urban"),
          dep.var.caption = "",
          covariate.labels = "Near Improved Rd.",
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          out = file.path(paper_tables, "ols_near_imprd_5km_results.tex"))
          
          
          
          
          
          