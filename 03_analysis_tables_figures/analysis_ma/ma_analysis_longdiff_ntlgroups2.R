# Market Access Analysis

# Grab names of objects before running code. At end of code, delete new objects
# created; code is memory intensive cleaning up avoids memory issues for
# subsequent scripts.
OBJECTS_BEFORE_CODE <- ls()

# Functions --------------------------------------------------------------------
round_char <- function(x){
  x %>% round(2) %>% as.character()
}

prep_data <- function(unit, log, theta, exclude, start_year, end_year){
  
  #### Load Data
  data <- readRDS(file.path(panel_rsdp_imp_dir, unit, "merged_datasets",
                            paste0("longdiff_data_clean_base",start_year,"_end",end_year,".Rds")))
  
  if(start_year == "1996") start_year_m1 <- 1996
  if(start_year == "2012") start_year_m1 <- 2011
  if(start_year == "2013") start_year_m1 <- 2012
  
  #### Prep RSDP IV Variables
  if(start_year == "2012"){
    data$dmspols_harmon_ihs_1996 <- data$dmspols_harmon_ihs_2011
    data$dmspols_harmon_ihs      <- data$viirs_bm_ihs
    
    data$globcover_urban_1996         <- data$globcover_urban_2011
    data$globcover_urban_sum_ihs_1996 <- data$globcover_urban_sum_ihs_2011
    
    #data$MA_var_1996                         <- data$MA_var_2011 
    data$dmspols_harmon_ihs_1996             <- data$dmspols_harmon_ihs_2011
    data$dmspols_harmon_ihs_pretnd96_92      <- data$dmspols_harmon_ihs_pretnd11_07 
    data$globcover_urban_sum_ihs_pretnd96_92 <- data$globcover_urban_sum_ihs_pretnd11_07
    
    data$wor_ntlgroup_2bin <- data$wor_ntlgroup_2011_2bin
    
  }
  
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
  data$MA_var_1996 <- data[[paste0("MA_pop2000_tt_theta",theta, log, "_", start_year_m1)]]
  
  data$MA_var_exc      <- data[[paste0("MA_pop2000_tt_theta",theta, exclude, log)]]
  data$MA_var_exc_1996 <- data[[paste0("MA_pop2000_tt_theta",theta, exclude, log, "_", start_year_m1)]]
  
  ## Interactions - MA
  data$MA_varXdistance_city_addisababa <- data$MA_var * data$distance_city_addisababa
  data$MA_varXglobcover_urban_1996     <- data$MA_var * data$globcover_urban_1996
  data$MA_varXdmspols_harmon_ihs_1996  <- data$MA_var * data$dmspols_harmon_ihs_1996
  data$MA_varXwor_ntlgroup_2bin        <- data$MA_var * data$wor_ntlgroup_2bin
  
  ## Interactions - MA_exclude
  data$MA_var_excXdistance_city_addisababa <- data$MA_var_exc * data$distance_city_addisababa
  data$MA_var_excXglobcover_urban_1996     <- data$MA_var_exc * data$globcover_urban_1996
  data$MA_var_excXdmspols_harmon_ihs_1996  <- data$MA_var_exc * data$dmspols_harmon_ihs_1996
  data$MA_var_excXwor_ntlgroup_2bin        <- data$MA_var_exc * data$wor_ntlgroup_2bin
  
  return(data)
}

# Regressions ------------------------------------------------------------------
unit <- "kebele"
log <- "_log"
theta <- "1"
exclude <- "_exclude20km"
#for(unit in c("woreda", "kebele")){ # "woreda", "clusters_of_ntl"

log = "_log"
theta = "3_8"
exclude = "_exclude20km"
start_year = "1996"
end_year = "2009"

for(log in c("_log")){
  for(theta in c("2", "3_8", "5", "8")){ # 
    for(exclude in c("_exclude20km", "_exclude50km", "_exclude100km")){ # "_exclude20km", "_exclude50km", "_exclude100km"
      for(start_year in c("1996", "2012")){
        for(end_year in c("2009", "2016")){
          
          print(paste(log, theta, exclude, start_year, end_year))
          
          if((start_year == "2012") & (end_year == "2009")) next
          if((theta %in% c("2", "5", "8")) & (exclude != "_exclude50km")) next
          
          # Out path for Rds file
          OUT_PATH <- file.path(panel_rsdp_imp_dir, "ma_results", "individual_files",
                                paste0("ma_ntlgroups2_", log, theta, exclude, start_year, end_year, ".Rds"))
          
          if(!file.exists(OUT_PATH)){
            
            data_kebele <- prep_data("kebele", log, theta, exclude, start_year, end_year)
            data_woreda <- prep_data("woreda", log, theta, exclude, start_year, end_year)
            
            #### OLS - Kebele
            ols1k <- feols(dmspols_harmon_ihs         ~ MA_var                                                                                         + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var   , conley(50), data = data_kebele)
            ols2k <- feols(dmspols_harmon_ihs         ~ MA_var + wor_ntlgroup_2bin  + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var   , conley(50), data = data_kebele) 
            
            ols3k <- feols(globcover_urban_sum_ihs     ~ MA_var                                                                                         + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var   , conley(50), data = data_kebele)
            ols4k <- feols(globcover_urban_sum_ihs    ~ MA_var + wor_ntlgroup_2bin  + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var   , conley(50), data = data_kebele) 
            
            ols5k <- feols(globcover_cropland_sum_ihs ~ MA_var                                                                                         + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var   , conley(50), data = data_kebele)
            ols6k <- feols(globcover_cropland_sum_ihs ~ MA_var + wor_ntlgroup_2bin  + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var   , conley(50), data = data_kebele) 
            
            #### OLS - Woreda
            ols1w <- feols(dmspols_harmon_ihs         ~ MA_var                                                                                         + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var   , conley(50), data = data_woreda)
            ols2w <- feols(dmspols_harmon_ihs         ~ MA_var + wor_ntlgroup_2bin  + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var   , conley(50), data = data_woreda) 
            
            ols3w <- feols(globcover_urban_sum_ihs     ~ MA_var                                                                                         + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var   , conley(50), data = data_woreda)
            ols4w <- feols(globcover_urban_sum_ihs    ~ MA_var + wor_ntlgroup_2bin  + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var   , conley(50), data = data_woreda) 
            
            ols5w <- feols(globcover_cropland_sum_ihs ~ MA_var                                                                                         + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var   , conley(50), data = data_woreda)
            ols6w <- feols(globcover_cropland_sum_ihs ~ MA_var + wor_ntlgroup_2bin  + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var   , conley(50), data = data_woreda) 
            
            #### IV - Kebele
            iv1k <- feols(dmspols_harmon_ihs         ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var |  MA_var ~ MA_var_exc                                                                             , conley(50), data = data_kebele)
            iv2k <- feols(dmspols_harmon_ihs         ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | MA_var + wor_ntlgroup_2bin      ~ MA_var_exc + MA_var_excXwor_ntlgroup_2bin      , conley(50), data = data_kebele)
            
            iv3k <- feols(globcover_urban_sum_ihs    ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var |  MA_var ~ MA_var_exc                                                                             , conley(50), data = data_kebele)
            iv4k <- feols(globcover_urban_sum_ihs    ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | MA_var + wor_ntlgroup_2bin      ~ MA_var_exc + MA_var_excXwor_ntlgroup_2bin      , conley(50), data = data_kebele)
            
            iv5k <- feols(globcover_cropland_sum_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var |  MA_var ~ MA_var_exc                                                                             , conley(50), data = data_kebele)
            iv6k <- feols(globcover_cropland_sum_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | MA_var + wor_ntlgroup_2bin      ~ MA_var_exc + MA_var_excXwor_ntlgroup_2bin      , conley(50), data = data_kebele)
            
            #### IV - Woreda
            iv1w <- feols(dmspols_harmon_ihs         ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var |  MA_var ~ MA_var_exc                                                                             , conley(50), data = data_woreda)
            iv2w <- feols(dmspols_harmon_ihs         ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | MA_var + wor_ntlgroup_2bin      ~ MA_var_exc + MA_var_excXwor_ntlgroup_2bin      , conley(50), data = data_woreda)
            
            iv3w <- feols(globcover_urban_sum_ihs    ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var |  MA_var ~ MA_var_exc                                                                             , conley(50), data = data_woreda)
            iv4w <- feols(globcover_urban_sum_ihs    ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | MA_var + wor_ntlgroup_2bin      ~ MA_var_exc + MA_var_excXwor_ntlgroup_2bin      , conley(50), data = data_woreda)
            
            iv5w <- feols(globcover_cropland_sum_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var |  MA_var ~ MA_var_exc                                                                             , conley(50), data = data_woreda)
            iv6w <- feols(globcover_cropland_sum_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | MA_var + wor_ntlgroup_2bin      ~ MA_var_exc + MA_var_excXwor_ntlgroup_2bin      , conley(50), data = data_woreda)
            
            #### Collect coefficients
            collect_coef <- function(lm_result){
              broom::tidy(lm_result, conf.int = TRUE) %>% clean_names()
            }
            
            ## Kebele
            results_df <- bind_rows(
              ols1k %>% collect_coef() %>% mutate(dv = "dmspols_harmon_ihs", MA_interact = F, ols_iv = "ols", unit = "kebele"),
              ols2k %>% collect_coef() %>% mutate(dv = "dmspols_harmon_ihs", MA_interact = T, ols_iv = "ols", unit = "kebele"),
              ols3k %>% collect_coef() %>% mutate(dv = "globcover_urban_sum_ihs", MA_interact = F, ols_iv = "ols", unit = "kebele"),
              ols4k %>% collect_coef() %>% mutate(dv = "globcover_urban_sum_ihs", MA_interact = T, ols_iv = "ols", unit = "kebele"),
              ols5k %>% collect_coef() %>% mutate(dv = "globcover_cropland_sum_ihs", MA_interact = F, ols_iv = "ols", unit = "kebele"),
              ols6k %>% collect_coef() %>% mutate(dv = "globcover_cropland_sum_ihs", MA_interact = T, ols_iv = "ols", unit = "kebele"),
              
              iv1k %>% collect_coef() %>% mutate(dv = "dmspols_harmon_ihs", MA_interact = F, ols_iv = "iv", unit = "kebele"),
              iv2k %>% collect_coef() %>% mutate(dv = "dmspols_harmon_ihs", MA_interact = T, ols_iv = "iv", unit = "kebele"),
              iv3k %>% collect_coef() %>% mutate(dv = "globcover_urban_sum_ihs", MA_interact = F, ols_iv = "iv", unit = "kebele"),
              iv4k %>% collect_coef() %>% mutate(dv = "globcover_urban_sum_ihs", MA_interact = T, ols_iv = "iv", unit = "kebele"),
              iv5k %>% collect_coef() %>% mutate(dv = "globcover_cropland_sum_ihs", MA_interact = F, ols_iv = "iv", unit = "kebele"),
              iv6k %>% collect_coef() %>% mutate(dv = "globcover_cropland_sum_ihs", MA_interact = T, ols_iv = "iv", unit = "kebele"),
              
              ## Woreda
              ols1w %>% collect_coef() %>% mutate(dv = "dmspols_harmon_ihs", MA_interact = F, ols_iv = "ols", unit = "woreda"),
              ols2w %>% collect_coef() %>% mutate(dv = "dmspols_harmon_ihs", MA_interact = T, ols_iv = "ols", unit = "woreda"),
              ols3w %>% collect_coef() %>% mutate(dv = "globcover_urban_sum_ihs", MA_interact = F, ols_iv = "ols", unit = "woreda"),
              ols4w %>% collect_coef() %>% mutate(dv = "globcover_urban_sum_ihs", MA_interact = T, ols_iv = "ols", unit = "woreda"),
              ols5w %>% collect_coef() %>% mutate(dv = "globcover_cropland_sum_ihs", MA_interact = F, ols_iv = "ols", unit = "woreda"),
              ols6w %>% collect_coef() %>% mutate(dv = "globcover_cropland_sum_ihs", MA_interact = T, ols_iv = "ols", unit = "woreda"),
              
              iv1w %>% collect_coef() %>% mutate(dv = "dmspols_harmon_ihs", MA_interact = F, ols_iv = "iv", unit = "woreda"),
              iv2w %>% collect_coef() %>% mutate(dv = "dmspols_harmon_ihs", MA_interact = T, ols_iv = "iv", unit = "woreda"),
              iv3w %>% collect_coef() %>% mutate(dv = "globcover_urban_sum_ihs", MA_interact = F, ols_iv = "iv", unit = "woreda"),
              iv4w %>% collect_coef() %>% mutate(dv = "globcover_urban_sum_ihs", MA_interact = T, ols_iv = "iv", unit = "woreda"),
              iv5w %>% collect_coef() %>% mutate(dv = "globcover_cropland_sum_ihs", MA_interact = F, ols_iv = "iv", unit = "woreda"),
              iv6w %>% collect_coef() %>% mutate(dv = "globcover_cropland_sum_ihs", MA_interact = T, ols_iv = "iv", unit = "woreda")
            )
            
            results_df <- results_df %>%
              dplyr::mutate(param_log = log,
                            param_theta = theta,
                            param_exclude = exclude,
                            param_start_year = start_year,
                            param_end_year = end_year,
                            param_trans_type = "ihs",
                            param_fe_var_i = "Z_CODE",
                            param_remove_sparse_regions = F,
                            ntl_groups = 2)
            
            #### OLS and IV Regression [For Main Text]
            modelsummary_tab(list("NTL" = ols1k,
                                  "NTL" = ols2k,
                                  "Urban" = ols3k,
                                  "Urban" = ols4k,
                                  "Cropland" = ols5k,
                                  "Cropland" = ols6k,
                                  "NTL" = iv1k,
                                  "NTL" = iv2k,
                                  "Urban" = iv3k,
                                  "Urban" = iv4k,
                                  "Cropland" = iv5k,
                                  "Cropland" = iv6k),
                             stars = c('*' = .1, '**' = .05, "***" = 0.01),
                             coef_map = c("MA_var" = "$\\Delta$MA",
                                          "wor_ntlgroup_2bin" = "$\\Delta$MA$\\times NTL_{96}$ Lit",
                                          
                                          "fit_MA_var" = "$\\Delta$MA",
                                          "fit_wor_ntlgroup_2bin" = "$\\Delta$MA$\\times NTL_{96}$ Lit",
                                          
                                          "MA_var_1996" = "MA, 1996",
                                          "MA_var_exc_1996" = "MA, 1996",
                                          "dmspols_harmon_ihs_1996" = "Log mean light, 1996",
                                          "dmspols_harmon_ihs_pretnd96_92" = "Pre-trend: log mean light",
                                          "globcover_urban_sum_ihs_pretnd96_92" = "Pre-trend: log N urban pixels"),
                             gof_map = c("nobs", "adj.r.squared"),
                             escape = FALSE,
                             add_rows = tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6, ~V7, ~V8, ~V9, ~V10, ~V11, ~V12,
                                                'Model', "OLS", "OLS", "OLS", "OLS", "OLS", "OLS", "IV", "IV", "IV", "IV", "IV", "IV",
                                                "1st Stage F-Stat",
                                                "NA","NA","NA","NA","NA","NA",
                                                fitstat(iv1k, type = "ivf", simplify = T)$stat %>% round_char(),
                                                fitstat(iv2k, type = "ivf", simplify = T)$stat %>% round_char(),
                                                fitstat(iv3k, type = "ivf", simplify = T)$stat %>% round_char(),
                                                fitstat(iv4k, type = "ivf", simplify = T)$stat %>% round_char(),
                                                fitstat(iv5k, type = "ivf", simplify = T)$stat %>% round_char(),
                                                fitstat(iv6k, type = "ivf", simplify = T)$stat %>% round_char()),
                             output = file.path(paper_tables,
                                                paste0("MA","_table_longdiff_theta",theta,exclude,log,"_","kebele","_startyear",start_year,"_endyear",end_year,"_ols_iv_2ntlgroups.tex")))
            
            
            # stargazer(ols1k,
            #           ols2k,
            #           ols3k,
            #           ols4k,
            #           ols5k,
            #           ols6k,
            #           iv1k %>% update_iv_coef_name(),
            #           iv2k %>% update_iv_coef_name(),
            #           iv3k %>% update_iv_coef_name(),
            #           iv4k %>% update_iv_coef_name(),
            #           iv5k %>% update_iv_coef_name(),
            #           iv6k %>% update_iv_coef_name(),
            #           dep.var.labels.include = T,
            #           dep.var.labels   = c("NTL", "Urban", "Cropland",
            #                                "NTL", "Urban", "Cropland"), # "NTL$\\geq$2", "NTL$\\geq6$",
            #           omit = c("Z_CODE", "Constant"),
            #           #keep=c("MA_var", "MA_var_1996", "dmspols_ihs_1996", "dmspols_ihs_pretnd96_92", "globcover_urban_sum_pretnd96_92.y"),
            #           covariate.labels = c("MA",
            #                                "MA$\\times NTL_{96}$ Lit",
            #                                #"MA X Dist Addis",
            #                                "MA, 1996",
            #                                "Log mean light, 1996",
            #                                "Pre-trend: log mean light",
            #                                "Pre-trend: log N urban pixels"),
            #           #covariate.labels = c("log(MA); $\\theta=1$",
            #           #                     "log(MA); $\\theta=8$"),
            #           dep.var.caption = "",
            #           omit.stat = c("f","ser", "rsq"), 
            #           align=TRUE,
            #           no.space=TRUE,
            #           float=FALSE,
            #           column.sep.width = "8pt",
            #           digits = 2,
            #           omit.table.layout = "n",
            #           add.lines = list(
            #             c("Model", rep("OLS", 6), rep("IV", 6))
            #           ),
            #           out=file.path(paper_tables,
            #                         paste0("MA","_table_longdiff_theta",theta,exclude,log,"_","kebele","_startyear",start_year,"_endyear",end_year,"_ols_iv_2ntlgroups.tex")))
            
            modelsummary_tab(list("NTL" = ols1w,
                                  "NTL" = ols2w,
                                  "Urban" = ols3w,
                                  "Urban" = ols4w,
                                  "Cropland" = ols5w,
                                  "Cropland" = ols6w,
                                  "NTL" = iv1w,
                                  "NTL" = iv2w,
                                  "Urban" = iv3w,
                                  "Urban" = iv4w,
                                  "Cropland" = iv5w,
                                  "Cropland" = iv6w),
                             stars = c('*' = .1, '**' = .05, "***" = 0.01),
                             coef_map = c("MA_var" = "$\\Delta$MA",
                                          "wor_ntlgroup_2bin" = "$\\Delta$MA$\\times NTL_{96}$ Lit",
                                          
                                          "fit_MA_var" = "$\\Delta$MA",
                                          "fit_wor_ntlgroup_2bin" = "$\\Delta$MA$\\times NTL_{96}$ Lit",
                                          
                                          "MA_var_1996" = "MA, 1996",
                                          "MA_var_exc_1996" = "MA, 1996",
                                          "dmspols_harmon_ihs_1996" = "Log mean light, 1996",
                                          "dmspols_harmon_ihs_pretnd96_92" = "Pre-trend: log mean light",
                                          "globcover_urban_sum_ihs_pretnd96_92" = "Pre-trend: log N urban pixels"),
                             gof_map = c("nobs", "adj.r.squared"),
                             escape = FALSE,
                             add_rows = tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6, ~V7, ~V8, ~V9, ~V10, ~V11, ~V12,
                                                'Model', "OLS", "OLS", "OLS", "OLS", "OLS", "OLS", "IV", "IV", "IV", "IV", "IV", "IV",
                                                "1st Stage F-Stat",
                                                "NA","NA","NA","NA","NA","NA",
                                                fitstat(iv1w, type = "ivf", simplify = T)$stat %>% round_char(),
                                                fitstat(iv2w, type = "ivf", simplify = T)$stat %>% round_char(),
                                                fitstat(iv3w, type = "ivf", simplify = T)$stat %>% round_char(),
                                                fitstat(iv4w, type = "ivf", simplify = T)$stat %>% round_char(),
                                                fitstat(iv5w, type = "ivf", simplify = T)$stat %>% round_char(),
                                                fitstat(iv6w, type = "ivf", simplify = T)$stat %>% round_char()),
                             output = file.path(paper_tables,
                                                paste0("MA","_table_longdiff_theta",theta,exclude,log,"_","woreda","_startyear",start_year,"_endyear",end_year,"_ols_iv_2ntlgroups.tex")))
            
            # stargazer(ols1w,
            #           ols2w,
            #           ols3w,
            #           ols4w,
            #           ols5w,
            #           ols6w,
            #           iv1w %>% update_iv_coef_name(),
            #           iv2w %>% update_iv_coef_name(),
            #           iv3w %>% update_iv_coef_name(),
            #           iv4w %>% update_iv_coef_name(),
            #           iv5w %>% update_iv_coef_name(),
            #           iv6w %>% update_iv_coef_name(),
            #           dep.var.labels.include = T,
            #           dep.var.labels   = c("NTL", "Urban", "Cropland",
            #                                "NTL", "Urban", "Cropland"), # "NTL$\\geq$2", "NTL$\\geq6$",
            #           omit = c("Z_CODE", "Constant"),
            #           #keep=c("MA_var", "MA_var_1996", "dmspols_ihs_1996", "dmspols_ihs_pretnd96_92", "globcover_urban_sum_pretnd96_92.y"),
            #           covariate.labels = c("MA",
            #                                "MA$\\times NTL_{96}$ Lit",
            #                                #"MA X Dist Addis",
            #                                "MA, 1996",
            #                                "Log mean light, 1996",
            #                                "Pre-trend: log mean light",
            #                                "Pre-trend: log N urban pixels"),
            #           #covariate.labels = c("log(MA); $\\theta=1$",
            #           #                     "log(MA); $\\theta=8$"),
            #           dep.var.caption = "",
            #           omit.stat = c("f","ser", "rsq"), 
            #           align=TRUE,
            #           no.space=TRUE,
            #           float=FALSE,
            #           column.sep.width = "8pt",
            #           digits = 2,
            #           omit.table.layout = "n",
            #           add.lines = list(
            #             c("Model", rep("OLS", 6), rep("IV", 6))
            #           ),
            #           out=file.path(paper_tables,
            #                         paste0("MA","_table_longdiff_theta",theta,exclude,log,"_","woreda","_startyear",start_year,"_endyear",end_year,"_ols_iv_2ntlgroups.tex")))
            # 
            
            #### OLS Only Regression [For SI]
            modelsummary_tab(list("NTL" = ols1k,
                                  "NTL" = ols2k,
                                  "Urban" = ols3k,
                                  "Urban" = ols4k,
                                  "Cropland" = ols5k,
                                  "Cropland" = ols6k),
                             stars = c('*' = .1, '**' = .05, "***" = 0.01),
                             coef_map = c("MA_var" = "$\\Delta$MA",
                                          "wor_ntlgroup_2bin" = "$\\Delta$MA$\\times NTL_{96}$ Lit",
                                          
                                          "MA_var_1996" = "MA, 1996",
                                          "dmspols_harmon_ihs_1996" = "Log mean light, 1996",
                                          "dmspols_harmon_ihs_pretnd96_92" = "Pre-trend: log mean light",
                                          "globcover_urban_sum_ihs_pretnd96_92" = "Pre-trend: log N urban pixels"),
                             gof_map = c("nobs", "adj.r.squared"),
                             escape = FALSE,
                             output = file.path(paper_tables,
                                                paste0("MA","_table_longdiff_theta",theta,log,"_",unit,"_startyear",start_year,"_endyear",end_year,"_ols_2ntlgroups.tex")))
            
            
            # stargazer(ols1k,
            #           ols2k,
            #           ols3k,
            #           ols4k,
            #           ols5k,
            #           ols6k,
            #           dep.var.labels.include = T,
            #           dep.var.labels   = c(#"NTL", "Urban", "Cropland",
            #             "NTL", "Urban", "Cropland"), # "NTL$\\geq$2", "NTL$\\geq6$",
            #           omit = c("Z_CODE", "Constant"),
            #           #keep=c("MA_var", "MA_var_1996", "dmspols_ihs_1996", "dmspols_ihs_pretnd96_92", "globcover_urban_sum_pretnd96_92.y"),
            #           covariate.labels = c("MA",
            #                                "MA$\\times NTL_{96}$ Lit",
            #                                #"MA X Dist Addis",
            #                                "MA, 1996",
            #                                "Log mean light, 1996",
            #                                "Pre-trend: log mean light",
            #                                "Pre-trend: log N urban pixels"),
            #           #covariate.labels = c("log(MA); $\\theta=1$",
            #           #                     "log(MA); $\\theta=8$"),
            #           dep.var.caption = "",
            #           omit.stat = c("f","ser", "rsq"), 
            #           align=TRUE,
            #           no.space=TRUE,
            #           float=FALSE,
            #           column.sep.width = "8pt",
            #           digits = 2,
            #           omit.table.layout = "n",
            #           #add.lines = list(
            #           #  c("Unit", rep("Keb.", 6), rep("Wor.", 6))
            #           #),
            #           out=file.path(paper_tables,
            #                         paste0("MA","_table_longdiff_theta",theta,log,"_",unit,"_startyear",start_year,"_endyear",end_year,"_ols_2ntlgroups.tex")))
            # 
            #### IV Only Regression [For SI]
            modelsummary_tab(list("NTL" = iv1k,
                                  "NTL" = iv2k,
                                  "Urban" = iv3k,
                                  "Urban" = iv4k,
                                  "Cropland" = iv5k,
                                  "Cropland" = iv6k,
                                  "NTL" = iv1w,
                                  "NTL" = iv2w,
                                  "Urban" = iv3w,
                                  "Urban" = iv4w,
                                  "Cropland" = iv5w,
                                  "Cropland" = iv6w),
                             stars = c('*' = .1, '**' = .05, "***" = 0.01),
                             coef_map = c("MA_var" = "$\\Delta$MA",
                                          "wor_ntlgroup_2bin" = "$\\Delta$MA$\\times NTL_{96}$ Lit",
                                          
                                          "fit_MA_var" = "$\\Delta$MA",
                                          "fit_wor_ntlgroup_2bin" = "$\\Delta$MA$\\times NTL_{96}$ Lit",
                                          
                                          "MA_var_1996" = "MA, 1996",
                                          "MA_var_exc_1996" = "MA, 1996",
                                          "dmspols_harmon_ihs_1996" = "Log mean light, 1996",
                                          "dmspols_harmon_ihs_pretnd96_92" = "Pre-trend: log mean light",
                                          "globcover_urban_sum_ihs_pretnd96_92" = "Pre-trend: log N urban pixels"),
                             gof_map = c("nobs", "adj.r.squared"),
                             escape = FALSE,
                             add_rows = tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6, ~V7, ~V8, ~V9, ~V10, ~V11, ~V12,
                                                'Unit', "Keb.", "Keb.", "Keb.", "Keb.", "Keb.", "Keb.", "Wor.", "Wor.", "Wor.", "Wor.", "Wor.", "Wor.",
                                                "1st Stage F-Stat",
                                                fitstat(iv1k, type = "ivf", simplify = T)$stat %>% round_char(),
                                                fitstat(iv2k, type = "ivf", simplify = T)$stat %>% round_char(),
                                                fitstat(iv3k, type = "ivf", simplify = T)$stat %>% round_char(),
                                                fitstat(iv4k, type = "ivf", simplify = T)$stat %>% round_char(),
                                                fitstat(iv5k, type = "ivf", simplify = T)$stat %>% round_char(),
                                                fitstat(iv6k, type = "ivf", simplify = T)$stat %>% round_char(),
                                                fitstat(iv1w, type = "ivf", simplify = T)$stat %>% round_char(),
                                                fitstat(iv2w, type = "ivf", simplify = T)$stat %>% round_char(),
                                                fitstat(iv3w, type = "ivf", simplify = T)$stat %>% round_char(),
                                                fitstat(iv4w, type = "ivf", simplify = T)$stat %>% round_char(),
                                                fitstat(iv5w, type = "ivf", simplify = T)$stat %>% round_char(),
                                                fitstat(iv6w, type = "ivf", simplify = T)$stat %>% round_char()),
                             output = file.path(paper_tables,
                                                paste0("MA","_table_longdiff_theta",theta,exclude,log,"_","kebeleworeda","_startyear",start_year,"_endyear",end_year,"_iv_2ntlgroups.tex")))
            
            
            
            # stargazer(iv1k,
            #           iv2k,
            #           iv3k,
            #           iv4k,
            #           iv5k,
            #           iv6k,
            #           iv1w,
            #           iv2w,
            #           iv3w,
            #           iv4w,
            #           iv5w,
            #           iv6w,
            #           dep.var.labels.include = T,
            #           dep.var.labels   = c("NTL", "Urban", "Cropland",
            #                                "NTL", "Urban", "Cropland"), #  "NTL$\\geq$2", "NTL$\\geq6$",
            #           omit = c("Z_CODE", "Constant"),
            #           order = c(5:9, 1:4),
            #           #keep=c("MA_var", "MA_var_1996", "dmspols_ihs_1996", "dmspols_ihs_pretnd96_92", "globcover_urban_sum_pretnd96_92.y"),
            #           covariate.labels = c("MA",
            #                                "MA$\\times NTL_{96}$ Lit",
            #                                #"MA X Dist Addis",
            #                                "MA, 1996",
            #                                "Log mean light, 1996",
            #                                "Pre-trend: log mean light",
            #                                "Pre-trend: log N urban pixels"),
            #           #covariate.labels = c("log(MA); $\\theta=1$",
            #           #                     "log(MA); $\\theta=8$"),
            #           dep.var.caption = "",
            #           omit.stat = c("f","ser", "rsq"), 
            #           align=TRUE,
            #           no.space=TRUE,
            #           float=FALSE,
            #           column.sep.width = "8pt",
            #           digits = 2,
            #           omit.table.layout = "n",
            #           add.lines = list(
            #             c("Unit", rep("Keb.", 6), rep("Wor.", 6))
            #           ),
            #           out=file.path(paper_tables,
            #                         paste0("MA","_table_longdiff_theta",theta,exclude,log,"_","kebeleworeda","_startyear",start_year,"_endyear",end_year,"_iv_2ntlgroups.tex")))
            
            saveRDS(results_df, OUT_PATH)
            
            
          }
        }
      }
    }
  }
}


# Cleanup ----------------------------------------------------------------------
OBJECTS_AFTER_CODE <- ls()
OBJECTS_TO_DELETE <- setdiff(OBJECTS_AFTER_CODE, OBJECTS_BEFORE_CODE)
rm(list = OBJECTS_TO_DELETE)

gc(); gc(); gc()

