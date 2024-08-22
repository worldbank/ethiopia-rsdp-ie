# Placebo: Electicity Transmission

start_date <- 2006

for(end_date in c(2009, 2016)){
  # Load data --------------------------------------------------------------------
  df <- readRDS(file.path(panel_rsdp_imp_dir, "kebele", "merged_datasets", 
                          paste0("longdiff_data_clean_base", 
                                 start_date,
                                 "_end",
                                 end_date,
                                 ".Rds")))
  
  # Prep data --------------------------------------------------------------------
  df <- df %>%
    dplyr::rename(dv_dmspols = dmspols_harmon_ihs,
                  dv_gcurban = globcover_urban_sum_ihs,
                  dv_gccrop  = globcover_cropland_sum_ihs,
                  
                  dv_dmspols_1996 = dmspols_harmon_ihs_1996,
                  dv_gcurban_1996 = globcover_urban_sum_ihs_1996,
                  dv_gccrop_1996 = globcover_cropland_sum_ihs_1996) %>%
    dplyr::mutate(near_elec_trans = as.numeric(distance_elec_trans <= 5000)) %>%
    dplyr::mutate(near_elec_transXdmspols_1996_bin4_1      = near_elec_trans * dmspols_harmon_1996_bin4_1,
                  near_elec_transXdmspols_1996_bin4_2      = near_elec_trans * dmspols_harmon_1996_bin4_2,
                  near_elec_transXdmspols_1996_bin4_3      = near_elec_trans * dmspols_harmon_1996_bin4_3,
                  near_elec_transXdmspols_1996_bin4_4      = near_elec_trans * dmspols_harmon_1996_bin4_4) %>%
    dplyr::mutate(cluster_var = woreda_id)
  
  # Regressions ------------------------------------------------------------------
  dmspols_k <- feols(dv_dmspols ~ near_elec_trans, vcov = conley(50), data = df)
  urban_k   <- feols(dv_gcurban ~ near_elec_trans, vcov = conley(50), data = df)
  crop_k    <- feols(dv_gccrop  ~ near_elec_trans, vcov = conley(50), data = df)
  
  dmspols_b_k <- feols(dv_dmspols ~ near_elec_trans + near_elec_transXdmspols_1996_bin4_2 + near_elec_transXdmspols_1996_bin4_3 + near_elec_transXdmspols_1996_bin4_4, vcov = conley(50), data = df)
  urban_b_k <- feols(dv_gcurban ~ near_elec_trans + near_elec_transXdmspols_1996_bin4_2 + near_elec_transXdmspols_1996_bin4_3 + near_elec_transXdmspols_1996_bin4_4, vcov = conley(50), data = df)
  crop_b_k  <- feols(dv_gccrop  ~ near_elec_trans + near_elec_transXdmspols_1996_bin4_2 + near_elec_transXdmspols_1996_bin4_3 + near_elec_transXdmspols_1996_bin4_4, vcov = conley(50), data = df)
  
  modelsummary_tab(list("NTL" = dmspols_k,
                    "Urban" = urban_k,
                    "Crop" = crop_k,
                    
                    "NTL" = dmspols_b_k,
                    "Urban" = urban_b_k,
                    "Crop" = crop_b_k),
               stars = c('*' = .1, '**' = .05, "***" = 0.01),
               coef_map = c("near_elec_trans" = "Trans. Lines",
                            "near_elec_transXdmspols_1996_bin4_2" = "Trans. Lines$\\times NTL_{96}$ Low",
                            "near_elec_transXdmspols_1996_bin4_3" = "Trans. Lines$\\times NTL_{96}$ Med",
                            "near_elec_transXdmspols_1996_bin4_4" = "Trans. Lines$\\times NTL_{96}$ High"),
               gof_map = c("nobs", "adj.r.squared"),
               escape = FALSE,
               output = file.path(paper_tables, 
                                  paste0("elect_trans_placebo_reg_",start_date,"_",end_date,".tex")))
  
  # stargazer(dmspols_k,
  #           urban_k,
  #           crop_k,
  #           
  #           dmspols_b_k,
  #           urban_b_k,
  #           crop_b_k,
  #           
  #           dep.var.labels.include = T,
  #           dep.var.labels = c("NTL", "Urban", "Cropland",
  #                              "NTL", "Urban", "Cropland"), #  "NTL $\\geq$ 2", "NTL $\\geq$ 6",
  #           dep.var.caption = "",
  #           covariate.labels = c("Trans. Lines",
  #                                "Trans. Lines$\\times NTL_{96}$ Low",
  #                                "Trans. Lines$\\times NTL_{96}$ Med",
  #                                "Trans. Lines$\\times NTL_{96}$ High"),
  #           omit.stat = c("f","ser", "rsq"),
  #           align=TRUE,
  #           no.space=TRUE,
  #           float=FALSE,
  #           column.sep.width="-15pt",
  #           digits=2,
  #           omit.table.layout = "n",
  #           #add.lines = T,
  #           out = file.path(paper_tables, 
  #                           paste0("elect_trans_placebo_reg_",start_date,"_",end_date,".tex")))
}

