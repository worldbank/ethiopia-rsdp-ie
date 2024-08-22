# Market Access Analysis

# Grab names of objects before running code. At end of code, delete new objects
# created; code is memory intensive cleaning up avoids memory issues for
# subsequent scripts.
OBJECTS_BEFORE_CODE <- ls()

# Functions --------------------------------------------------------------------
round_char <- function(x){
  x %>% round(2) %>% as.character()
}

prep_data <- function(unit, log, theta, exclude, time_period){
  
  #### Load/Subset Data
  data <- readRDS(file.path(panel_rsdp_imp_dir, unit, "merged_datasets", "panel_data_clean.Rds"))
  
  #### Time Period
  if(time_period == "all")       data <- data %>% filter(year >= 1996)
  if(time_period == "rsdpi_iii") data <- data %>% filter(year >= 1996) %>% filter(year <= 2009)
  if(time_period == "rsdpiv")    data <- data %>% filter(year >= 2012) %>% filter(year <= 2016)
  
  #### Prep RSDP IV Variables
  if(time_period == "rsdpiv"){
    data$dmspols_harmon_ihs_1996 <- data$dmspols_harmon_ihs_2011
    #data$dmspols_harmon_ihs      <- data$viirs_bm_ihs
    
    data$globcover_urban_1996         <- data$globcover_urban_2011
    data$globcover_urban_sum_ihs_1996 <- data$globcover_urban_sum_ihs_2011
  }
  
  #### Cluster Var and FE Var
  if(unit %in% "woreda") data$cluster_var <- data$Z_CODE
  if(unit %in% "kebele") data$cluster_var <- data$woreda_id
  
  #### Prep Variables
  data$MA_var     <- data[[paste0("MA_pop2000_tt_theta",theta, log)]]
  data$MA_var_exc <- data[[paste0("MA_pop2000_tt_theta",theta,exclude, log)]]
  
  # 100km
  data$distance_city_addisababa <- data$distance_city_addisababa / 1000 / 100
  
  ## Interaction Vars - MA
  data$MA_varXdmspols_harmon_ihs_1996      <- data$MA_var * data$dmspols_harmon_ihs_1996
  data$MA_varXglobcover_urban_1996         <- data$MA_var * data$globcover_urban_1996
  data$MA_varXdistance_city_addisababa     <- data$MA_var * data$distance_city_addisababa
  data$MA_varXglobcover_urban_sum_ihs_1996 <- data$MA_var * data$globcover_urban_sum_ihs_1996
  
  data$MA_varXwor_ntlgroup_2bin            <- data$MA_var * data$wor_ntlgroup_2bin
  
  ## Interaction Vars - MA_exclude
  data$MA_var_excXdmspols_harmon_ihs_1996      <- data$MA_var_exc * data$dmspols_harmon_ihs_1996
  data$MA_var_excXglobcover_urban_1996         <- data$MA_var_exc * data$globcover_urban_1996
  data$MA_var_excXdistance_city_addisababa     <- data$MA_var_exc * data$distance_city_addisababa
  data$MA_var_excXglobcover_urban_sum_ihs_1996 <- data$MA_var_exc * data$globcover_urban_sum_ihs_1996
  
  data$MA_var_excXwor_ntlgroup_2bin <- data$MA_var_exc * data$wor_ntlgroup_2bin
  
  return(data)
}

# Regressions ------------------------------------------------------------------
unit <- "kebeleworeda"


log <- "_log"
theta <- "3_8"
exclude <- "_exclude20km"
time_period <- "all"

for(log in c("_log")){
  for(theta in c("3_8")){ # 
    for(exclude in c("_exclude50km")){  # "_exclude20km", "_exclude50km", "_exclude100km"
      for(time_period in c("all", "rsdpi_iii", "rsdpiv")){
        
        data_kebele <- prep_data("kebele", log, theta, exclude, time_period)
        data_woreda <- prep_data("woreda", log, theta, exclude, time_period)
        
        ## OLS - Kebele
        ols1k <- feols(dmspols_harmon_ihs ~ MA_var                            + temp_avg + precipitation | year + cell_id    , vcov = conley(50), data = data_kebele)
        ols2k <- feols(dmspols_harmon_ihs ~ MA_var + MA_varXwor_ntlgroup_2bin + temp_avg + precipitation | year + cell_id    , vcov = conley(50), data = data_kebele)
        
        ols3k <- feols(globcover_urban_sum_ihs ~ MA_var                            + temp_avg + precipitation | year + cell_id                       , vcov = conley(50), data = data_kebele)
        ols4k <- feols(globcover_urban_sum_ihs ~ MA_var + MA_varXwor_ntlgroup_2bin + temp_avg + precipitation | year + cell_id    , vcov = conley(50), data = data_kebele)
        
        ols5k <- feols(globcover_cropland_sum_ihs ~ MA_var                            + temp_avg + precipitation | year + cell_id                       , vcov = conley(50), data = data_kebele)
        ols6k <- feols(globcover_cropland_sum_ihs ~ MA_var + MA_varXwor_ntlgroup_2bin + temp_avg + precipitation | year + cell_id    , vcov = conley(50), data = data_kebele)
        
        ## OLS - Woreda
        ols1w <- feols(dmspols_harmon_ihs ~ MA_var                            + temp_avg + precipitation | year + cell_id                       , conley(50), data = data_woreda)
        ols2w <- feols(dmspols_harmon_ihs ~ MA_var + MA_varXwor_ntlgroup_2bin + temp_avg + precipitation | year + cell_id    , conley(50), data = data_woreda)
        
        ols3w <- feols(globcover_urban_sum_ihs ~ MA_var                            + temp_avg + precipitation | year + cell_id                       , conley(50), data = data_woreda)
        ols4w <- feols(globcover_urban_sum_ihs ~ MA_var + MA_varXwor_ntlgroup_2bin + temp_avg + precipitation | year + cell_id    , conley(50), data = data_woreda)
        
        ols5w <- feols(globcover_cropland_sum_ihs ~ MA_var                            + temp_avg + precipitation | year + cell_id                       , conley(50), data = data_woreda)
        ols6w <- feols(globcover_cropland_sum_ihs ~ MA_var + MA_varXwor_ntlgroup_2bin + temp_avg + precipitation | year + cell_id    , conley(50), data = data_woreda)
        
        ## IV - Kebele
        iv1k <- feols(dmspols_harmon_ihs ~ temp_avg + precipitation           | year + cell_id |  MA_var ~ MA_var_exc ,                                                                       vcov = conley(50), data = data_kebele) 
        iv2k <- feols(dmspols_harmon_ihs ~ temp_avg + precipitation           | year + cell_id | MA_var + MA_varXwor_ntlgroup_2bin ~ MA_var_exc + MA_var_excXwor_ntlgroup_2bin , vcov = conley(50), data = data_kebele) 
        
        iv3k <- feols(globcover_urban_sum_ihs ~ temp_avg + precipitation      | year + cell_id |  MA_var ~ MA_var_exc ,                                                                       vcov = conley(50), data = data_kebele) 
        iv4k <- feols(globcover_urban_sum_ihs ~ temp_avg + precipitation      | year + cell_id | MA_var + MA_varXwor_ntlgroup_2bin ~ MA_var_exc + MA_var_excXwor_ntlgroup_2bin , vcov = conley(50), data = data_kebele) 
        
        iv5k <- feols(globcover_cropland_sum_ihs ~ temp_avg + precipitation   | year + cell_id |  MA_var ~ MA_var_exc ,                                                                       vcov = conley(50), data = data_kebele) 
        iv6k <- feols(globcover_cropland_sum_ihs ~ temp_avg + precipitation   | year + cell_id | MA_var + MA_varXwor_ntlgroup_2bin ~ MA_var_exc + MA_var_excXwor_ntlgroup_2bin , vcov = conley(50), data = data_kebele) 
        
        ## IV - Woreda
        iv1w <- feols(dmspols_harmon_ihs ~ temp_avg + precipitation           | year + cell_id |  MA_var ~ MA_var_exc ,                                                                       conley(50), data = data_woreda) 
        iv2w <- feols(dmspols_harmon_ihs ~ temp_avg + precipitation           | year + cell_id | MA_var + MA_varXwor_ntlgroup_2bin ~ MA_var_exc + MA_var_excXwor_ntlgroup_2bin , conley(50), data = data_woreda) 
        
        iv3w <- feols(globcover_urban_sum_ihs ~ temp_avg + precipitation      | year + cell_id |  MA_var ~ MA_var_exc ,                                                                       conley(50), data = data_woreda) 
        iv4w <- feols(globcover_urban_sum_ihs ~ temp_avg + precipitation      | year + cell_id | MA_var + MA_varXwor_ntlgroup_2bin ~ MA_var_exc + MA_var_excXwor_ntlgroup_2bin , conley(50), data = data_woreda) 
        
        iv5w <- feols(globcover_cropland_sum_ihs ~ temp_avg + precipitation   | year + cell_id |  MA_var ~ MA_var_exc ,                                                                       conley(50), data = data_woreda) 
        iv6w <- feols(globcover_cropland_sum_ihs ~ temp_avg + precipitation   | year + cell_id | MA_var + MA_varXwor_ntlgroup_2bin ~ MA_var_exc + MA_var_excXwor_ntlgroup_2bin , conley(50), data = data_woreda) 
        
        ## OLS Stargazer
        modelsummary_tab(list("NTL" = ols1k,
                          "NTL" = ols2k,
                          "Urban" = ols3k,
                          "Urban" = ols4k,
                          "Cropland" = ols5k,
                          "Cropland" = ols6k,
                          "NTL" = ols1w,
                          "NTL" = ols2w,
                          "Urban" = ols3w,
                          "Urban" = ols4w,
                          "Cropland" = ols5w,
                          "Cropland" = ols6w),
                     stars = c('*' = .1, '**' = .05, "***" = 0.01),
                     coef_map = c("MA_var" = "MA",
                                  "MA_varXwor_ntlgroup_2bin" = "MA$\\times NTL_{96}$ Lit"),
                     gof_map = c("nobs", "adj.r.squared"),
                     escape = FALSE,
                     add_rows = tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6, ~V7, ~V8, ~V9, ~V10, ~V11, ~V12,
                                        'Unit', "Keb.","Keb.","Keb.","Keb.","Keb.","Keb.","Wor.","Wor.","Wor.","Wor.","Wor.","Wor.",
                                        'Year FE', "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y",
                                        'Unit FE', "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y"),
                     output = file.path(paper_tables,
                                        paste0("MA_table",log,"_theta",theta,"_",unit,"_",time_period,"_ols_2ntlgroups.tex")))
        

        # stargazer(ols1k,
        #           ols2k,
        #           ols3k,
        #           ols4k,
        #           ols5k,
        #           ols6k,
        #           ols1w,
        #           ols2w,
        #           ols3w,
        #           ols4w,
        #           ols5w,
        #           ols6w,
        #           dep.var.labels.include = T,
        #           dep.var.labels   = c("NTL", "Urban", "Cropland",
        #                                "NTL", "Urban", "Cropland"), # "NTL$\\geq$2", "NTL$\\geq6$",
        #           #keep=c("MA_var"),
        #           omit = c("temp_avg", "precipitation"),
        #           covariate.labels = c("MA",
        #                                "MA$\\times NTL_{96}$ Lit"),
        #           dep.var.caption = "",
        #           omit.stat = c("f","ser"), 
        #           align=TRUE,
        #           no.space=TRUE,
        #           float=FALSE,
        #           column.sep.width = "8pt",
        #           digits = 2,
        #           omit.table.layout = "n",
        #           add.lines = list(
        #             c("Unit", rep("Keb.", 6), rep("Wor.", 6)),
        #             c("Year FE", rep("Y", 12)),
        #             c("Unit FE", rep("Y", 12))
        #           ),
        #           out=file.path(paper_tables,
        #                         paste0("MA_table",log,"_theta",theta,"_",unit,"_",time_period,"_ols_2ntlgroups.tex")))
        # 
        
        ## IV Stargazer
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
                     coef_map = c("fit_MA_var" = "MA",
                                  "fit_MA_varXwor_ntlgroup_2bin" = "MA$\\times NTL_{96}$ Lit"),
                     gof_map = c("nobs", "adj.r.squared"),
                     escape = FALSE,
                     add_rows = tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6, ~V7, ~V8, ~V9, ~V10, ~V11, ~V12,
                                        'Unit', "Keb.","Keb.","Keb.","Keb.","Keb.","Keb.","Wor.","Wor.","Wor.","Wor.","Wor.","Wor.",
                                        'Year FE', "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y",
                                        'Unit FE', "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y",
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
                                        fitstat(iv6w, type = "ivf", simplify = T)$stat %>% round_char()
                                        ),
                     output = file.path(paper_tables,
                                        paste0("MA_table",log,"_theta",theta,exclude,"_",unit,"_",time_period,"_iv_2ntlgroups.tex")))
        
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
        #                                "NTL", "Urban", "Cropland"), # "NTL$\\geq$2", "NTL$\\geq6$",
        #           #keep=c("MA_var"),
        #           omit = c("temp_avg", "precipitation"),
        #           covariate.labels = c("MA",
        #                                "MA$\\times NTL_{96}$ Lit"),
        #           dep.var.caption = "",
        #           omit.stat = c("f","ser"), 
        #           align=TRUE,
        #           no.space=TRUE,
        #           float=FALSE,
        #           column.sep.width = "8pt",
        #           digits = 2,
        #           omit.table.layout = "n",
        #           add.lines = list(
        #             c("Unit", rep("Keb.", 6), rep("Wor.", 6)),
        #             c("Year FE", rep("Y", 12)),
        #             c("Unit FE", rep("Y", 12))
        #           ),
        #           out=file.path(paper_tables,
        #                         paste0("MA_table",log,"_theta",theta,exclude,"_",unit,"_",time_period,"_iv_2ntlgroups.tex")))
        
      }
    }
  }
}

# Cleanup ----------------------------------------------------------------------
OBJECTS_AFTER_CODE <- ls()
OBJECTS_TO_DELETE <- setdiff(OBJECTS_AFTER_CODE, OBJECTS_BEFORE_CODE)
rm(list = OBJECTS_TO_DELETE)

gc(); gc(); gc()





