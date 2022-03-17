# Market Access Analysis

# Grab names of objects before running code. At end of code, delete new objects
# created; code is memory intensive cleaning up avoids memory issues for
# subsequent scripts.
OBJECTS_BEFORE_CODE <- ls()

# Functions --------------------------------------------------------------------
prep_data <- function(unit, log, theta, exclude, end_year){
  
  #### Load Data
  data <- readRDS(file.path(panel_rsdp_imp_dir, unit, "merged_datasets",
                            paste0("longdiff_data_clean_base1996_end",end_year,".Rds")))
  
  #### Cluster Var and FE Var
  if(unit %in% "woreda") data$cluster_var <- data$Z_CODE
  if(unit %in% "kebele") data$cluster_var <- data$woreda_id
  
  if(unit %in% "woreda") data$fe_var <- data$Z_CODE
  if(unit %in% "kebele") data$fe_var <- data$Z_CODE
  
  #### Prep Data
  
  ## Dist Addis
  data$distance_city_addisababa <- data$distance_city_addisababa / 1000 / 100
  
  ## MA Variables
  data$MA_var      <- data[[paste0("MA_pop2000_tt_theta",theta, log)]]
  data$MA_var_1996 <- data[[paste0("MA_pop2000_tt_theta",theta, log, "_1996")]]
  
  data$MA_var_exc      <- data[[paste0("MA_pop2000_tt_theta",theta, exclude, log)]]
  data$MA_var_exc_1996 <- data[[paste0("MA_pop2000_tt_theta",theta, exclude, log, "_1996")]]
  
  ## Interactions - MA
  data$MA_varXdistance_city_addisababa <- data$MA_var * data$distance_city_addisababa
  data$MA_varXglobcover_urban_1996     <- data$MA_var * data$globcover_urban_1996
  
  data$MA_varXdmspols_harmon_ihs_1996 <- data$MA_var * data$dmspols_harmon_ihs_1996
  
  data$MA_varXdmspols_harmon_1996_bin4_1 <- data$MA_var * data$dmspols_harmon_1996_bin4_1
  data$MA_varXdmspols_harmon_1996_bin4_2 <- data$MA_var * data$dmspols_harmon_1996_bin4_2
  data$MA_varXdmspols_harmon_1996_bin4_3 <- data$MA_var * data$dmspols_harmon_1996_bin4_3
  data$MA_varXdmspols_harmon_1996_bin4_4 <- data$MA_var * data$dmspols_harmon_1996_bin4_4
  
  ## Interactions - MA_exclude
  data$MA_var_excXdistance_city_addisababa <- data$MA_var_exc * data$distance_city_addisababa
  data$MA_var_excXglobcover_urban_1996     <- data$MA_var_exc * data$globcover_urban_1996
  
  data$MA_var_excXdmspols_harmon_ihs_1996 <- data$MA_var_exc * data$dmspols_harmon_ihs_1996
  
  data$MA_var_excXdmspols_harmon_1996_bin4_1 <- data$MA_var_exc * data$dmspols_harmon_1996_bin4_1
  data$MA_var_excXdmspols_harmon_1996_bin4_2 <- data$MA_var_exc * data$dmspols_harmon_1996_bin4_2
  data$MA_var_excXdmspols_harmon_1996_bin4_3 <- data$MA_var_exc * data$dmspols_harmon_1996_bin4_3
  data$MA_var_excXdmspols_harmon_1996_bin4_4 <- data$MA_var_exc * data$dmspols_harmon_1996_bin4_4
  
  return(data)
}

# Regressions ------------------------------------------------------------------
unit <- "kebele"
log <- "_log"
theta <- "1"
exclude <- "_exclude20km"
#for(unit in c("woreda", "kebele")){ # "woreda", "clusters_of_ntl"
for(log in c("_log")){
  for(theta in c("2", "3_8", "5", "8")){ # 
    for(exclude in c("_exclude20km", "_exclude50km", "_exclude100km")){ # "_exclude20km", "_exclude50km", "_exclude100km"
      for(end_year in c("2016")){
        
        data_kebele <- prep_data("kebele", log, theta, exclude, end_year)
        data_woreda <- prep_data("woreda", log, theta, exclude, end_year)
        
        #### OLS - Kebele
        ols1k <- felm(dmspols_harmon_ihs         ~ MA_var                                                                                         + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var | 0 | cluster_var, data = data_kebele)
        ols2k <- felm(dmspols_harmon_ihs         ~ MA_var + MA_varXdmspols_harmon_1996_bin4_2 + MA_varXdmspols_harmon_1996_bin4_3 + MA_varXdmspols_harmon_1996_bin4_4  + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var | 0 | cluster_var, data = data_kebele) 
        
        ols3k <- felm(globcover_urban_sum_ihs     ~ MA_var                                                                                         + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var | 0 | cluster_var, data = data_kebele)
        ols4k <- felm(globcover_urban_sum_ihs    ~ MA_var + MA_varXdmspols_harmon_1996_bin4_2 + MA_varXdmspols_harmon_1996_bin4_3 + MA_varXdmspols_harmon_1996_bin4_4  + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var | 0 | cluster_var, data = data_kebele) 
        
        ols5k <- felm(globcover_cropland_sum_ihs ~ MA_var                                                                                         + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var | 0 | cluster_var, data = data_kebele)
        ols6k <- felm(globcover_cropland_sum_ihs ~ MA_var + MA_varXdmspols_harmon_1996_bin4_2 + MA_varXdmspols_harmon_1996_bin4_3 + MA_varXdmspols_harmon_1996_bin4_4  + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var | 0 | cluster_var, data = data_kebele) 
        
        #### OLS - Woreda
        ols1w <- felm(dmspols_harmon_ihs         ~ MA_var                                                                                         + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var | 0 | cluster_var, data = data_woreda)
        ols2w <- felm(dmspols_harmon_ihs         ~ MA_var + MA_varXdmspols_harmon_1996_bin4_2 + MA_varXdmspols_harmon_1996_bin4_3 + MA_varXdmspols_harmon_1996_bin4_4  + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var | 0 | cluster_var, data = data_woreda) 
        
        ols3w <- felm(globcover_urban_sum_ihs     ~ MA_var                                                                                         + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var | 0 | cluster_var, data = data_woreda)
        ols4w <- felm(globcover_urban_sum_ihs    ~ MA_var + MA_varXdmspols_harmon_1996_bin4_2 + MA_varXdmspols_harmon_1996_bin4_3 + MA_varXdmspols_harmon_1996_bin4_4  + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var | 0 | cluster_var, data = data_woreda) 
        
        ols5w <- felm(globcover_cropland_sum_ihs ~ MA_var                                                                                         + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var | 0 | cluster_var, data = data_woreda)
        ols6w <- felm(globcover_cropland_sum_ihs ~ MA_var + MA_varXdmspols_harmon_1996_bin4_2 + MA_varXdmspols_harmon_1996_bin4_3 + MA_varXdmspols_harmon_1996_bin4_4  + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var | 0 | cluster_var, data = data_woreda) 
        
        #### IV - Kebele
        iv1k <- felm(dmspols_harmon_ihs         ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | (MA_var ~ MA_var_exc)                                                                             | cluster_var, data = data_kebele)
        iv2k <- felm(dmspols_harmon_ihs         ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | (MA_var|MA_varXdmspols_harmon_1996_bin4_2|MA_varXdmspols_harmon_1996_bin4_3|MA_varXdmspols_harmon_1996_bin4_4      ~ MA_var_exc + MA_var_excXdmspols_harmon_1996_bin4_2 + MA_var_excXdmspols_harmon_1996_bin4_3 + MA_var_excXdmspols_harmon_1996_bin4_4)      | cluster_var, data = data_kebele)
        
        iv3k <- felm(globcover_urban_sum_ihs    ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | (MA_var ~ MA_var_exc)                                                                             | cluster_var, data = data_kebele)
        iv4k <- felm(globcover_urban_sum_ihs    ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | (MA_var|MA_varXdmspols_harmon_1996_bin4_2|MA_varXdmspols_harmon_1996_bin4_3|MA_varXdmspols_harmon_1996_bin4_4      ~ MA_var_exc + MA_var_excXdmspols_harmon_1996_bin4_2 + MA_var_excXdmspols_harmon_1996_bin4_3 + MA_var_excXdmspols_harmon_1996_bin4_4)      | cluster_var, data = data_kebele)
        
        iv5k <- felm(globcover_cropland_sum_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | (MA_var ~ MA_var_exc)                                                                             | cluster_var, data = data_kebele)
        iv6k <- felm(globcover_cropland_sum_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | (MA_var|MA_varXdmspols_harmon_1996_bin4_2|MA_varXdmspols_harmon_1996_bin4_3|MA_varXdmspols_harmon_1996_bin4_4      ~ MA_var_exc + MA_var_excXdmspols_harmon_1996_bin4_2 + MA_var_excXdmspols_harmon_1996_bin4_3 + MA_var_excXdmspols_harmon_1996_bin4_4)      | cluster_var, data = data_kebele)
        
        #### IV - Woreda
        iv1w <- felm(dmspols_harmon_ihs         ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | (MA_var ~ MA_var_exc)                                                                             | cluster_var, data = data_woreda)
        iv2w <- felm(dmspols_harmon_ihs         ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | (MA_var|MA_varXdmspols_harmon_1996_bin4_2|MA_varXdmspols_harmon_1996_bin4_3|MA_varXdmspols_harmon_1996_bin4_4      ~ MA_var_exc + MA_var_excXdmspols_harmon_1996_bin4_2 + MA_var_excXdmspols_harmon_1996_bin4_3 + MA_var_excXdmspols_harmon_1996_bin4_4)      | cluster_var, data = data_woreda)
        
        iv3w <- felm(globcover_urban_sum_ihs    ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | (MA_var ~ MA_var_exc)                                                                             | cluster_var, data = data_woreda)
        iv4w <- felm(globcover_urban_sum_ihs    ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | (MA_var|MA_varXdmspols_harmon_1996_bin4_2|MA_varXdmspols_harmon_1996_bin4_3|MA_varXdmspols_harmon_1996_bin4_4      ~ MA_var_exc + MA_var_excXdmspols_harmon_1996_bin4_2 + MA_var_excXdmspols_harmon_1996_bin4_3 + MA_var_excXdmspols_harmon_1996_bin4_4)      | cluster_var, data = data_woreda)
        
        iv5w <- felm(globcover_cropland_sum_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | (MA_var ~ MA_var_exc)                                                                             | cluster_var, data = data_woreda)
        iv6w <- felm(globcover_cropland_sum_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | (MA_var|MA_varXdmspols_harmon_1996_bin4_2|MA_varXdmspols_harmon_1996_bin4_3|MA_varXdmspols_harmon_1996_bin4_4      ~ MA_var_exc + MA_var_excXdmspols_harmon_1996_bin4_2 + MA_var_excXdmspols_harmon_1996_bin4_3 + MA_var_excXdmspols_harmon_1996_bin4_4)      | cluster_var, data = data_woreda)
        
        #### OLS and IV Regression [For Main Text]
        stargazer(ols1k,
                  ols2k,
                  ols3k,
                  ols4k,
                  ols5k,
                  ols6k,
                  iv1k %>% update_iv_coef_name(),
                  iv2k %>% update_iv_coef_name(),
                  iv3k %>% update_iv_coef_name(),
                  iv4k %>% update_iv_coef_name(),
                  iv5k %>% update_iv_coef_name(),
                  iv6k %>% update_iv_coef_name(),
                  dep.var.labels.include = T,
                  dep.var.labels   = c("NTL", "Urban", "Cropland",
                                       "NTL", "Urban", "Cropland"), # "NTL$\\geq$2", "NTL$\\geq6$",
                  omit = c("Z_CODE", "Constant"),
                  #keep=c("MA_var", "MA_var_1996", "dmspols_ihs_1996", "dmspols_ihs_pretnd96_92", "globcover_urban_sum_pretnd96_92.y"),
                  covariate.labels = c("MA",
                                       "MA$\\times NTL_{96}$ Low",
                                       "MA$\\times NTL_{96}$ Med",
                                       "MA$\\times NTL_{96}$ High",
                                       #"MA X Dist Addis",
                                       "MA, 1996",
                                       "Log mean light, 1996",
                                       "Pre-trend: log mean light",
                                       "Pre-trend: log N urban pixels"),
                  #covariate.labels = c("log(MA); $\\theta=1$",
                  #                     "log(MA); $\\theta=8$"),
                  dep.var.caption = "",
                  omit.stat = c("f","ser", "rsq"), 
                  align=TRUE,
                  no.space=TRUE,
                  float=FALSE,
                  column.sep.width = "8pt",
                  digits = 2,
                  omit.table.layout = "n",
                  add.lines = list(
                    c("Model", rep("OLS", 6), rep("IV", 6))
                  ),
                  out=file.path(paper_tables,
                                paste0("MA","_table_longdiff_theta",theta,exclude,log,"_","kebele","_endyear",end_year,"_ols_iv.tex")))
        
        stargazer(iv1k %>% update_iv_coef_name(),
                  iv2k %>% update_iv_coef_name(),
                  iv3k %>% update_iv_coef_name(),
                  iv4k %>% update_iv_coef_name(),
                  iv5k %>% update_iv_coef_name(),
                  iv6k %>% update_iv_coef_name(),
                  dep.var.labels.include = T,
                  dep.var.labels   = c("NTL", "Urban", "Cropland",
                                       "NTL", "Urban", "Cropland"), # "NTL$\\geq$2", "NTL$\\geq6$",
                  omit = c("Z_CODE", "Constant"),
                  #keep=c("MA_var", "MA_var_1996", "dmspols_ihs_1996", "dmspols_ihs_pretnd96_92", "globcover_urban_sum_pretnd96_92.y"),
                  covariate.labels = c("MA",
                                       "MA$\\times NTL_{96}$ Low",
                                       "MA$\\times NTL_{96}$ Med",
                                       "MA$\\times NTL_{96}$ High",
                                       #"MA X Dist Addis",
                                       "MA, 1996",
                                       "Log mean light, 1996",
                                       "Pre-trend: log mean light",
                                       "Pre-trend: log N urban pixels"),
                  #covariate.labels = c("log(MA); $\\theta=1$",
                  #                     "log(MA); $\\theta=8$"),
                  dep.var.caption = "",
                  omit.stat = c("f","ser", "rsq"), 
                  align=TRUE,
                  no.space=TRUE,
                  float=FALSE,
                  column.sep.width = "8pt",
                  digits = 2,
                  omit.table.layout = "n",
                  add.lines = list(
                    c("Model", rep("IV", 6))
                  ),
                  out=file.path(paper_tables,
                                paste0("MA","_table_longdiff_theta",theta,exclude,log,"_","kebele","_endyear",end_year,"_ols_iv_IVonly.tex")))
        
        stargazer(ols1w,
                  ols2w,
                  ols3w,
                  ols4w,
                  ols5w,
                  ols6w,
                  iv1w %>% update_iv_coef_name(),
                  iv2w %>% update_iv_coef_name(),
                  iv3w %>% update_iv_coef_name(),
                  iv4w %>% update_iv_coef_name(),
                  iv5w %>% update_iv_coef_name(),
                  iv6w %>% update_iv_coef_name(),
                  dep.var.labels.include = T,
                  dep.var.labels   = c("NTL", "Urban", "Cropland",
                                       "NTL", "Urban", "Cropland"), # "NTL$\\geq$2", "NTL$\\geq6$",
                  omit = c("Z_CODE", "Constant"),
                  #keep=c("MA_var", "MA_var_1996", "dmspols_ihs_1996", "dmspols_ihs_pretnd96_92", "globcover_urban_sum_pretnd96_92.y"),
                  covariate.labels = c("MA",
                                       "MA$\\times NTL_{96}$ Low",
                                       "MA$\\times NTL_{96}$ Med",
                                       "MA$\\times NTL_{96}$ High",
                                       #"MA X Dist Addis",
                                       "MA, 1996",
                                       "Log mean light, 1996",
                                       "Pre-trend: log mean light",
                                       "Pre-trend: log N urban pixels"),
                  #covariate.labels = c("log(MA); $\\theta=1$",
                  #                     "log(MA); $\\theta=8$"),
                  dep.var.caption = "",
                  omit.stat = c("f","ser", "rsq"), 
                  align=TRUE,
                  no.space=TRUE,
                  float=FALSE,
                  column.sep.width = "8pt",
                  digits = 2,
                  omit.table.layout = "n",
                  add.lines = list(
                    c("Model", rep("OLS", 6), rep("IV", 6))
                  ),
                  out=file.path(paper_tables,
                                paste0("MA","_table_longdiff_theta",theta,exclude,log,"_","woreda","_endyear",end_year,"_ols_iv.tex")))
        
        
        #### OLS Only Regression [For SI]
        stargazer(ols1k,
                  ols2k,
                  ols3k,
                  ols4k,
                  ols5k,
                  ols6k,
                  #ols1w,
                  #ols2w,
                  #ols3w,
                  #ols4w,
                  #ols5w,
                  #ols6w,
                  dep.var.labels.include = T,
                  dep.var.labels   = c(#"NTL", "Urban", "Cropland",
                    "NTL", "Urban", "Cropland"), # "NTL$\\geq$2", "NTL$\\geq6$",
                  omit = c("Z_CODE", "Constant"),
                  #keep=c("MA_var", "MA_var_1996", "dmspols_ihs_1996", "dmspols_ihs_pretnd96_92", "globcover_urban_sum_pretnd96_92.y"),
                  covariate.labels = c("MA",
                                       "MA$\\times NTL_{96}$ Low",
                                       "MA$\\times NTL_{96}$ Med",
                                       "MA$\\times NTL_{96}$ High",
                                       #"MA X Dist Addis",
                                       "MA, 1996",
                                       "Log mean light, 1996",
                                       "Pre-trend: log mean light",
                                       "Pre-trend: log N urban pixels"),
                  #covariate.labels = c("log(MA); $\\theta=1$",
                  #                     "log(MA); $\\theta=8$"),
                  dep.var.caption = "",
                  omit.stat = c("f","ser", "rsq"), 
                  align=TRUE,
                  no.space=TRUE,
                  float=FALSE,
                  column.sep.width = "8pt",
                  digits = 2,
                  omit.table.layout = "n",
                  #add.lines = list(
                  #  c("Unit", rep("Keb.", 6), rep("Wor.", 6))
                  #),
                  out=file.path(paper_tables,
                                paste0("MA","_table_longdiff_theta",theta,log,"_",unit,"_endyear",end_year,"_ols.tex")))
        
        #### IV Only Regression [For SI]
        stargazer(iv1k,
                  iv2k,
                  iv3k,
                  iv4k,
                  iv5k,
                  iv6k,
                  iv1w,
                  iv2w,
                  iv3w,
                  iv4w,
                  iv5w,
                  iv6w,
                  dep.var.labels.include = T,
                  dep.var.labels   = c("NTL", "Urban", "Cropland",
                                       "NTL", "Urban", "Cropland"), #  "NTL$\\geq$2", "NTL$\\geq6$",
                  omit = c("Z_CODE", "Constant"),
                  order = c(5:9, 1:4),
                  #keep=c("MA_var", "MA_var_1996", "dmspols_ihs_1996", "dmspols_ihs_pretnd96_92", "globcover_urban_sum_pretnd96_92.y"),
                  covariate.labels = c("MA",
                                       "MA$\\times NTL_{96}$ Low",
                                       "MA$\\times NTL_{96}$ Med",
                                       "MA$\\times NTL_{96}$ High",
                                       #"MA X Dist Addis",
                                       "MA, 1996",
                                       "Log mean light, 1996",
                                       "Pre-trend: log mean light",
                                       "Pre-trend: log N urban pixels"),
                  #covariate.labels = c("log(MA); $\\theta=1$",
                  #                     "log(MA); $\\theta=8$"),
                  dep.var.caption = "",
                  omit.stat = c("f","ser", "rsq"), 
                  align=TRUE,
                  no.space=TRUE,
                  float=FALSE,
                  column.sep.width = "8pt",
                  digits = 2,
                  omit.table.layout = "n",
                  add.lines = list(
                    c("Unit", rep("Keb.", 6), rep("Wor.", 6))
                  ),
                  out=file.path(paper_tables,
                                paste0("MA","_table_longdiff_theta",theta,exclude,log,"_","kebeleworeda","_endyear",end_year,"_iv.tex")))
      }
    }
  }
}
#}

# Cleanup ----------------------------------------------------------------------
OBJECTS_AFTER_CODE <- ls()
OBJECTS_TO_DELETE <- setdiff(OBJECTS_AFTER_CODE, OBJECTS_BEFORE_CODE)
rm(list = OBJECTS_TO_DELETE)

gc(); gc(); gc()

