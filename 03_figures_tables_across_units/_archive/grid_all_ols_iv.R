# Ethiopia Grid Long Difference - OLS and IV

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

# OLS --------------------------------------------------------------------------
ols_dmspols_zhang_ihs <- felm(dmspols_zhang_ihs ~ near_anyimproved_5km | 0 | 0 | W_CODE, data = df2012)
ols_dmspols_zhang_2   <- felm(dmspols_zhang_2   ~ near_anyimproved_5km | 0 | 0 | W_CODE, data = df2012)
ols_dmspols_zhang_6   <- felm(dmspols_zhang_6   ~ near_anyimproved_5km | 0 | 0 | W_CODE, data = df2012)
ols_globcover_urban   <- felm(globcover_urban   ~ near_anyimproved_5km | 0 | 0 | W_CODE, data = df2016)

# IV - Least Cost Distance -----------------------------------------------------
iv_lc_dmspols_zhang_ihs <- felm(dmspols_zhang_ihs ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_5km) | W_CODE, data = df2012)
iv_lc_dmspols_zhang_2   <- felm(dmspols_zhang_2   ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_5km) | W_CODE, data = df2012)
iv_lc_dmspols_zhang_6   <- felm(dmspols_zhang_6   ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_5km) | W_CODE, data = df2012)
iv_lc_globcover_urban   <- felm(globcover_urban   ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_5km) | W_CODE, data = df2016)

# IV - Min Euclidean Distance --------------------------------------------------
iv_ed_dmspols_zhang_ihs <- felm(dmspols_zhang_ihs ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_mindist_5km) | W_CODE, data = df2012)
iv_ed_dmspols_zhang_2   <- felm(dmspols_zhang_2   ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_mindist_5km) | W_CODE, data = df2012)
iv_ed_dmspols_zhang_6   <- felm(dmspols_zhang_6   ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_mindist_5km) | W_CODE, data = df2012)
iv_ed_globcover_urban   <- felm(globcover_urban   ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_mindist_5km) | W_CODE, data = df2016)

# Table: OLS -------------------------------------------------------------------
stargazer(ols_dmspols_zhang_ihs,
          ols_dmspols_zhang_2,
          ols_dmspols_zhang_6,
          ols_globcover_urban,
          dep.var.labels.include = T,
          dep.var.labels = c("IHS(NTL)",
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

# Table: IV --------------------------------------------------------------------
## Least Cost
stargazer(iv_lc_dmspols_zhang_ihs,
          iv_lc_dmspols_zhang_2,
          iv_lc_dmspols_zhang_6,
          iv_lc_globcover_urban,
          dep.var.labels.include = T,
          dep.var.labels = c("IHS(NTL)",
                             "NTL $>$ 2",
                             "NTL $>$ 6",
                             "Urban"),
          dep.var.caption = "",
          covariate.labels = c("Near Improved Rd."),
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          out = file.path(paper_tables, "iv_leastcost_near_imprd_5km_results.tex"))

## Min Distance
stargazer(iv_ed_dmspols_zhang_ihs,
          iv_ed_dmspols_zhang_2,
          iv_ed_dmspols_zhang_6,
          iv_ed_globcover_urban,
          dep.var.labels.include = T,
          dep.var.labels = c("IHS(NTL)",
                             "NTL $>$ 2",
                             "NTL $>$ 6",
                             "Urban"),
          dep.var.caption = "",
          covariate.labels = c("Near Improved Rd."),
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          out = file.path(paper_tables, "iv_mindist_near_imprd_5km_results.tex"))

# Table: First Stage -----------------------------------------------------------
## Least Cost
stargazer(iv_lc_dmspols_zhang_ihs$stage1,
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
              lfe::waldtest(iv_lc_dmspols_zhang_ihs$stage1, ~near_mst_5km, lhs=iv_lc_dmspols_zhang_ihs$stage1$lhs)[5] %>% round(ROUND_NUM)
            )
          ),
          out = file.path(paper_tables,
                          "fststage_leastcost_near_imprd_5km_results.tex"))

## Min Distance
stargazer(iv_ed_dmspols_zhang_ihs$stage1,
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
              lfe::waldtest(iv_ed_dmspols_zhang_ihs$stage1, ~near_mst_mindist_5km, lhs=iv_ed_dmspols_zhang_ihs$stage1$lhs)[5] %>% round(ROUND_NUM)
            )
          ),
          out = file.path(paper_tables,
                          "fststage_mindist_near_imprd_5km_results.tex"))

          
          
          