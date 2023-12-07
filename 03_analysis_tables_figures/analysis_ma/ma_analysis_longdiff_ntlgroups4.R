# Market Access Analysis

# Grab names of objects before running code. At end of code, delete new objects
# created; code is memory intensive cleaning up avoids memory issues for
# subsequent scripts.
OBJECTS_BEFORE_CODE <- ls()

# Functions --------------------------------------------------------------------
prep_data <- function(unit, log, theta, exclude, start_year, end_year){
  
  #### Load Data
  data <- readRDS(file.path(panel_rsdp_imp_dir, unit, "merged_datasets",
                            paste0("longdiff_data_clean_base",start_year,"_end",end_year,".Rds")))
  
  if(start_year == "1996") start_year_m1 <- 1996
  if(start_year == "2012") start_year_m1 <- 2011
  
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
    
    data$dmspols_harmon_1996_bin4_1 <- data$dmspols_harmon_2011_bin4_1
    data$dmspols_harmon_1996_bin4_2 <- data$dmspols_harmon_2011_bin4_2
    data$dmspols_harmon_1996_bin4_3 <- data$dmspols_harmon_2011_bin4_3
    data$dmspols_harmon_1996_bin4_4 <- data$dmspols_harmon_2011_bin4_4
    
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
  data$MA_var_exc_1996 <- data[[paste0("MA_pop2000_tt_theta",theta, exclude, log, "_",start_year_m1)]]
  
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
log <- "_log"
theta <- "1"
exclude <- "_exclude20km"
start_year <- "1996"
end_year <- "2009"
trans_type <- "log"

unit <- "kebele"

for(log in c("_log")){
  for(theta in c("2", "3_8", "5", "8")){ # 
    for(exclude in c("_exclude20km", "_exclude50km", "_exclude100km")){ # "_exclude20km", "_exclude50km", "_exclude100km"
      for(start_year in c("1996", "2012")){
        for(end_year in c("2009", "2016")){
          for(trans_type in c("log")){ # ihs, log
            
            print(paste(log, theta, exclude, start_year, end_year))
            
            if((start_year == "2012") & (end_year == "2009")) next
            
            
            data_kebele <- prep_data("kebele", log, theta, exclude, start_year, end_year)
            data_woreda <- prep_data("woreda", log, theta, exclude, start_year, end_year)
            
            #### Transformation type
            if(trans_type == "ihs"){
              # Only indicate if log, for appendix
              trans_type_suffix <- ""
              
            } else{
              trans_type_suffix <- "_log"
              
              data_kebele <- data_kebele %>%
                dplyr::mutate(dmspols_harmon_ihs = dmspols_harmon_log,
                              globcover_urban_sum_ihs = globcover_urban_sum_log,
                              globcover_cropland_sum_ihs = globcover_cropland_sum_log)
              
              data_woreda <- data_woreda %>%
                dplyr::mutate(dmspols_harmon_ihs = dmspols_harmon_log,
                              globcover_urban_sum_ihs = globcover_urban_sum_log,
                              globcover_cropland_sum_ihs = globcover_cropland_sum_log)
            }
            
            #### OLS - Kebele
            ols1k <- feols(dmspols_harmon_ihs         ~ MA_var                                                                                         + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var   , conley(20), data = data_kebele)
            ols2k <- feols(dmspols_harmon_ihs         ~ MA_var + MA_varXdmspols_harmon_1996_bin4_2 + MA_varXdmspols_harmon_1996_bin4_3 + MA_varXdmspols_harmon_1996_bin4_4  + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var   , conley(20), data = data_kebele) 
            
            ols3k <- feols(globcover_urban_sum_ihs     ~ MA_var                                                                                         + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var   , conley(20), data = data_kebele)
            ols4k <- feols(globcover_urban_sum_ihs    ~ MA_var + MA_varXdmspols_harmon_1996_bin4_2 + MA_varXdmspols_harmon_1996_bin4_3 + MA_varXdmspols_harmon_1996_bin4_4  + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var   , conley(20), data = data_kebele) 
            
            ols5k <- feols(globcover_cropland_sum_ihs ~ MA_var                                                                                         + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var   , conley(20), data = data_kebele)
            ols6k <- feols(globcover_cropland_sum_ihs ~ MA_var + MA_varXdmspols_harmon_1996_bin4_2 + MA_varXdmspols_harmon_1996_bin4_3 + MA_varXdmspols_harmon_1996_bin4_4  + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var   , conley(20), data = data_kebele) 
            
            #### OLS - Woreda
            ols1w <- feols(dmspols_harmon_ihs         ~ MA_var                                                                                         + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var   , vcov = ~cluster_var, data = data_woreda)
            ols2w <- feols(dmspols_harmon_ihs         ~ MA_var + MA_varXdmspols_harmon_1996_bin4_2 + MA_varXdmspols_harmon_1996_bin4_3 + MA_varXdmspols_harmon_1996_bin4_4  + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var   , vcov = ~cluster_var, data = data_woreda) 
            
            ols3w <- feols(globcover_urban_sum_ihs     ~ MA_var                                                                                         + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var   , vcov = ~cluster_var, data = data_woreda)
            ols4w <- feols(globcover_urban_sum_ihs    ~ MA_var + MA_varXdmspols_harmon_1996_bin4_2 + MA_varXdmspols_harmon_1996_bin4_3 + MA_varXdmspols_harmon_1996_bin4_4  + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var   , vcov = ~cluster_var, data = data_woreda) 
            
            ols5w <- feols(globcover_cropland_sum_ihs ~ MA_var                                                                                         + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var   , vcov = ~cluster_var, data = data_woreda)
            ols6w <- feols(globcover_cropland_sum_ihs ~ MA_var + MA_varXdmspols_harmon_1996_bin4_2 + MA_varXdmspols_harmon_1996_bin4_3 + MA_varXdmspols_harmon_1996_bin4_4  + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var   , vcov = ~cluster_var, data = data_woreda) 
            
            #### IV - Kebele
            iv1k <- feols(dmspols_harmon_ihs         ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var |  MA_var ~ MA_var_exc                                                                             , conley(20), data = data_kebele)
            iv2k <- feols(dmspols_harmon_ihs         ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | MA_var + MA_varXdmspols_harmon_1996_bin4_2 + MA_varXdmspols_harmon_1996_bin4_3 + MA_varXdmspols_harmon_1996_bin4_4      ~ MA_var_exc + MA_var_excXdmspols_harmon_1996_bin4_2 + MA_var_excXdmspols_harmon_1996_bin4_3 + MA_var_excXdmspols_harmon_1996_bin4_4      , conley(20), data = data_kebele)
            
            iv3k <- feols(globcover_urban_sum_ihs    ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var |  MA_var ~ MA_var_exc                                                                             , conley(20), data = data_kebele)
            iv4k <- feols(globcover_urban_sum_ihs    ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | MA_var + MA_varXdmspols_harmon_1996_bin4_2 + MA_varXdmspols_harmon_1996_bin4_3 + MA_varXdmspols_harmon_1996_bin4_4      ~ MA_var_exc + MA_var_excXdmspols_harmon_1996_bin4_2 + MA_var_excXdmspols_harmon_1996_bin4_3 + MA_var_excXdmspols_harmon_1996_bin4_4      , conley(20), data = data_kebele)
            
            iv5k <- feols(globcover_cropland_sum_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var |  MA_var ~ MA_var_exc                                                                             , conley(20), data = data_kebele)
            iv6k <- feols(globcover_cropland_sum_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | MA_var + MA_varXdmspols_harmon_1996_bin4_2 + MA_varXdmspols_harmon_1996_bin4_3 + MA_varXdmspols_harmon_1996_bin4_4      ~ MA_var_exc + MA_var_excXdmspols_harmon_1996_bin4_2 + MA_var_excXdmspols_harmon_1996_bin4_3 + MA_var_excXdmspols_harmon_1996_bin4_4      , conley(20), data = data_kebele)
            
            #### IV - Woreda
            iv1w <- feols(dmspols_harmon_ihs         ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var |  MA_var ~ MA_var_exc                                                                             , vcov = ~cluster_var, data = data_woreda)
            iv2w <- feols(dmspols_harmon_ihs         ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | MA_var + MA_varXdmspols_harmon_1996_bin4_2 + MA_varXdmspols_harmon_1996_bin4_3 + MA_varXdmspols_harmon_1996_bin4_4      ~ MA_var_exc + MA_var_excXdmspols_harmon_1996_bin4_2 + MA_var_excXdmspols_harmon_1996_bin4_3 + MA_var_excXdmspols_harmon_1996_bin4_4      , vcov = ~cluster_var, data = data_woreda)
            
            iv3w <- feols(globcover_urban_sum_ihs    ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var |  MA_var ~ MA_var_exc                                                                             , vcov = ~cluster_var, data = data_woreda)
            iv4w <- feols(globcover_urban_sum_ihs    ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | MA_var + MA_varXdmspols_harmon_1996_bin4_2 + MA_varXdmspols_harmon_1996_bin4_3 + MA_varXdmspols_harmon_1996_bin4_4      ~ MA_var_exc + MA_var_excXdmspols_harmon_1996_bin4_2 + MA_var_excXdmspols_harmon_1996_bin4_3 + MA_var_excXdmspols_harmon_1996_bin4_4      , vcov = ~cluster_var, data = data_woreda)
            
            iv5w <- feols(globcover_cropland_sum_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var |  MA_var ~ MA_var_exc                                                                             , vcov = ~cluster_var, data = data_woreda)
            iv6w <- feols(globcover_cropland_sum_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | MA_var + MA_varXdmspols_harmon_1996_bin4_2 + MA_varXdmspols_harmon_1996_bin4_3 + MA_varXdmspols_harmon_1996_bin4_4      ~ MA_var_exc + MA_var_excXdmspols_harmon_1996_bin4_2 + MA_var_excXdmspols_harmon_1996_bin4_3 + MA_var_excXdmspols_harmon_1996_bin4_4      , vcov = ~cluster_var, data = data_woreda)
            
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
                         stars = c('*' = .05, '**' = .01, "***" = 0.001),
                         coef_map = c("MA_var" = "MA",
                                      "MA_varXdmspols_harmon_1996_bin4_2" = "MA$\\times NTL_{96}$ Low",
                                      "MA_varXdmspols_harmon_1996_bin4_3" = "MA$\\times NTL_{96}$ Med",
                                      "MA_varXdmspols_harmon_1996_bin4_4" = "MA$\\times NTL_{96}$ High",
                                      
                                      "fit_MA_var" = "MA",
                                      "fit_MA_varXdmspols_harmon_1996_bin4_2" = "MA$\\times NTL_{96}$ Low",
                                      "fit_MA_varXdmspols_harmon_1996_bin4_3" = "MA$\\times NTL_{96}$ Med",
                                      "fit_MA_varXdmspols_harmon_1996_bin4_4" = "MA$\\times NTL_{96}$ High",
                                      
                                      "MA_var_1996" = "MA, 1996",
                                      "MA_var_exc_1996" = "MA, 1996",
                                      "dmspols_harmon_ihs_1996" = "Log mean light, 1996",
                                      "dmspols_harmon_ihs_pretnd96_92" = "Pre-trend: log mean light",
                                      "globcover_urban_sum_ihs_pretnd96_92" = "Pre-trend: log N urban pixels"),
                         gof_map = c("nobs", "adj.r.squared"),
                         escape = FALSE,
                         add_rows = tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6, ~V7, ~V8, ~V9, ~V10, ~V11, ~V12,
                                            'Model', "OLS", "OLS", "OLS", "OLS", "OLS", "OLS", "IV", "IV", "IV", "IV", "IV", "IV"),
                         output = file.path(paper_tables,
                                            paste0("MA","_table_longdiff_theta",theta,exclude,log,"_","kebele","_startyear",start_year,"_endyear",end_year,"_ols_iv",trans_type_suffix,".tex")))
            
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
            #                                "MA$\\times NTL_{96}$ Low",
            #                                "MA$\\times NTL_{96}$ Med",
            #                                "MA$\\times NTL_{96}$ High",
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
            #                         paste0("MA","_table_longdiff_theta",theta,exclude,log,"_","kebele","_startyear",start_year,"_endyear",end_year,"_ols_iv",trans_type_suffix,".tex")))
            
            modelsummary_tab(list("NTL" = iv1k,
                              "NTL" = iv2k,
                              "Urban" = iv3k,
                              "Urban" = iv4k,
                              "Cropland" = iv5k,
                              "Cropland" = iv6k),
                         stars = c('*' = .05, '**' = .01, "***" = 0.001),
                         coef_map = c("MA_var" = "MA",
                                      "MA_varXdmspols_harmon_1996_bin4_2" = "MA$\\times NTL_{96}$ Low",
                                      "MA_varXdmspols_harmon_1996_bin4_3" = "MA$\\times NTL_{96}$ Med",
                                      "MA_varXdmspols_harmon_1996_bin4_4" = "MA$\\times NTL_{96}$ High",
                                      
                                      "fit_MA_var" = "MA",
                                      "fit_MA_varXdmspols_harmon_1996_bin4_2" = "MA$\\times NTL_{96}$ Low",
                                      "fit_MA_varXdmspols_harmon_1996_bin4_3" = "MA$\\times NTL_{96}$ Med",
                                      "fit_MA_varXdmspols_harmon_1996_bin4_4" = "MA$\\times NTL_{96}$ High",
                                      
                                      "MA_var_1996" = "MA, 1996",
                                      "MA_var_exc_1996" = "MA, 1996",
                                      "dmspols_harmon_ihs_1996" = "Log mean light, 1996",
                                      "dmspols_harmon_ihs_pretnd96_92" = "Pre-trend: log mean light",
                                      "globcover_urban_sum_ihs_pretnd96_92" = "Pre-trend: log N urban pixels"),
                         gof_map = c("nobs", "adj.r.squared"),
                         escape = FALSE,
                         add_rows = tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6,
                                            'Model', "IV", "IV", "IV", "IV", "IV", "IV"),
                         output = file.path(paper_tables,
                                            paste0("MA","_table_longdiff_theta",theta,exclude,log,"_","kebele","_startyear",start_year,"_endyear",end_year,"_ols_iv_IVonly",trans_type_suffix,".tex")))
            
            # stargazer(iv1k %>% update_iv_coef_name(),
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
            #                                "MA$\\times NTL_{96}$ Low",
            #                                "MA$\\times NTL_{96}$ Med",
            #                                "MA$\\times NTL_{96}$ High",
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
            #             c("Model", rep("IV", 6))
            #           ),
            #           out=file.path(paper_tables,
            #                         paste0("MA","_table_longdiff_theta",theta,exclude,log,"_","kebele","_startyear",start_year,"_endyear",end_year,"_ols_iv_IVonly",trans_type_suffix,".tex")))
            
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
                         stars = c('*' = .05, '**' = .01, "***" = 0.001),
                         coef_map = c("MA_var" = "MA",
                                      "MA_varXdmspols_harmon_1996_bin4_2" = "MA$\\times NTL_{96}$ Low",
                                      "MA_varXdmspols_harmon_1996_bin4_3" = "MA$\\times NTL_{96}$ Med",
                                      "MA_varXdmspols_harmon_1996_bin4_4" = "MA$\\times NTL_{96}$ High",
                                      
                                      "fit_MA_var" = "MA",
                                      "fit_MA_varXdmspols_harmon_1996_bin4_2" = "MA$\\times NTL_{96}$ Low",
                                      "fit_MA_varXdmspols_harmon_1996_bin4_3" = "MA$\\times NTL_{96}$ Med",
                                      "fit_MA_varXdmspols_harmon_1996_bin4_4" = "MA$\\times NTL_{96}$ High",
                                      
                                      "MA_var_1996" = "MA, 1996",
                                      "MA_var_exc_1996" = "MA, 1996",
                                      "dmspols_harmon_ihs_1996" = "Log mean light, 1996",
                                      "dmspols_harmon_ihs_pretnd96_92" = "Pre-trend: log mean light",
                                      "globcover_urban_sum_ihs_pretnd96_92" = "Pre-trend: log N urban pixels"),
                         gof_map = c("nobs", "adj.r.squared"),
                         escape = FALSE,
                         add_rows = tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6, ~V7, ~V8, ~V9, ~V10, ~V11, ~V12,
                                            'Model', "OLS", "OLS", "OLS", "OLS", "OLS", "OLS", "IV", "IV", "IV", "IV", "IV", "IV"),
                         output = file.path(paper_tables,
                                            paste0("MA","_table_longdiff_theta",theta,exclude,log,"_","woreda","_startyear",start_year,"_endyear",end_year,"_ols_iv",trans_type_suffix,".tex")))
            
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
            #                                "MA$\\times NTL_{96}$ Low",
            #                                "MA$\\times NTL_{96}$ Med",
            #                                "MA$\\times NTL_{96}$ High",
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
            #                         paste0("MA","_table_longdiff_theta",theta,exclude,log,"_","woreda","_startyear",start_year,"_endyear",end_year,"_ols_iv",trans_type_suffix,".tex")))
            # 
            
            #### OLS Only Regression [For SI]
            modelsummary_tab(list("NTL" = ols1k,
                              "NTL" = ols2k,
                              "Urban" = ols3k,
                              "Urban" = ols4k,
                              "Cropland" = ols5k,
                              "Cropland" = ols6k),
                         stars = c('*' = .05, '**' = .01, "***" = 0.001),
                         coef_map = c("MA_var" = "MA",
                                      "MA_varXdmspols_harmon_1996_bin4_2" = "MA$\\times NTL_{96}$ Low",
                                      "MA_varXdmspols_harmon_1996_bin4_3" = "MA$\\times NTL_{96}$ Med",
                                      "MA_varXdmspols_harmon_1996_bin4_4" = "MA$\\times NTL_{96}$ High",
                                      
                                      "fit_MA_var" = "MA",
                                      "fit_MA_varXdmspols_harmon_1996_bin4_2" = "MA$\\times NTL_{96}$ Low",
                                      "fit_MA_varXdmspols_harmon_1996_bin4_3" = "MA$\\times NTL_{96}$ Med",
                                      "fit_MA_varXdmspols_harmon_1996_bin4_4" = "MA$\\times NTL_{96}$ High",
                                      
                                      "MA_var_1996" = "MA, 1996",
                                      "MA_var_exc_1996" = "MA, 1996",
                                      "dmspols_harmon_ihs_1996" = "Log mean light, 1996",
                                      "dmspols_harmon_ihs_pretnd96_92" = "Pre-trend: log mean light",
                                      "globcover_urban_sum_ihs_pretnd96_92" = "Pre-trend: log N urban pixels"),
                         gof_map = c("nobs", "adj.r.squared"),
                         escape = FALSE,
                         output = file.path(paper_tables,
                                            paste0("MA","_table_longdiff_theta",theta,log,"_",unit,"_startyear",start_year,"_endyear",end_year,"_ols",trans_type_suffix,".tex")))
            # 
            # stargazer(ols1k,
            #           ols2k,
            #           ols3k,
            #           ols4k,
            #           ols5k,
            #           ols6k,
            #           #ols1w,
            #           #ols2w,
            #           #ols3w,
            #           #ols4w,
            #           #ols5w,
            #           #ols6w,
            #           dep.var.labels.include = T,
            #           dep.var.labels   = c(#"NTL", "Urban", "Cropland",
            #             "NTL", "Urban", "Cropland"), # "NTL$\\geq$2", "NTL$\\geq6$",
            #           omit = c("Z_CODE", "Constant"),
            #           #keep=c("MA_var", "MA_var_1996", "dmspols_ihs_1996", "dmspols_ihs_pretnd96_92", "globcover_urban_sum_pretnd96_92.y"),
            #           covariate.labels = c("MA",
            #                                "MA$\\times NTL_{96}$ Low",
            #                                "MA$\\times NTL_{96}$ Med",
            #                                "MA$\\times NTL_{96}$ High",
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
            #                         paste0("MA","_table_longdiff_theta",theta,log,"_",unit,"_startyear",start_year,"_endyear",end_year,"_ols",trans_type_suffix,".tex")))
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
                         stars = c('*' = .05, '**' = .01, "***" = 0.001),
                         coef_map = c("MA_var" = "MA",
                                      "MA_varXdmspols_harmon_1996_bin4_2" = "MA$\\times NTL_{96}$ Low",
                                      "MA_varXdmspols_harmon_1996_bin4_3" = "MA$\\times NTL_{96}$ Med",
                                      "MA_varXdmspols_harmon_1996_bin4_4" = "MA$\\times NTL_{96}$ High",
                                      
                                      "fit_MA_var" = "MA",
                                      "fit_MA_varXdmspols_harmon_1996_bin4_2" = "MA$\\times NTL_{96}$ Low",
                                      "fit_MA_varXdmspols_harmon_1996_bin4_3" = "MA$\\times NTL_{96}$ Med",
                                      "fit_MA_varXdmspols_harmon_1996_bin4_4" = "MA$\\times NTL_{96}$ High",
                                      
                                      "MA_var_1996" = "MA, 1996",
                                      "MA_var_exc_1996" = "MA, 1996",
                                      "dmspols_harmon_ihs_1996" = "Log mean light, 1996",
                                      "dmspols_harmon_ihs_pretnd96_92" = "Pre-trend: log mean light",
                                      "globcover_urban_sum_ihs_pretnd96_92" = "Pre-trend: log N urban pixels"),
                         gof_map = c("nobs", "adj.r.squared"),
                         escape = FALSE,
                         add_rows = tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6, ~V7, ~V8, ~V9, ~V10, ~V11, ~V12,
                                            'Unit', "Keb.", "Keb.", "Keb.", "Keb.", "Keb.", "Keb.", "Wor.", "Wor.", "Wor.", "Wor.", "Wor.", "Wor."),
                         output = file.path(paper_tables,
                                            paste0("MA","_table_longdiff_theta",theta,exclude,log,"_","kebeleworeda","_startyear",start_year,"_endyear",end_year,"_iv",trans_type_suffix,".tex")))
            
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
            #                                "MA$\\times NTL_{96}$ Low",
            #                                "MA$\\times NTL_{96}$ Med",
            #                                "MA$\\times NTL_{96}$ High",
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
            #                         paste0("MA","_table_longdiff_theta",theta,exclude,log,"_","kebeleworeda","_startyear",start_year,"_endyear",end_year,"_iv",trans_type_suffix,".tex")))
          }
        }
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

