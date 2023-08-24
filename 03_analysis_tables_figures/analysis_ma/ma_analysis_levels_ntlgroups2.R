# Market Access Analysis

# Grab names of objects before running code. At end of code, delete new objects
# created; code is memory intensive cleaning up avoids memory issues for
# subsequent scripts.
OBJECTS_BEFORE_CODE <- ls()

# Functions --------------------------------------------------------------------
prep_data <- function(unit, log, theta, exclude, time_period){
  
  #### Load/Subset Data
  data <- readRDS(file.path(panel_rsdp_imp_dir, unit, "merged_datasets", "panel_data_clean.Rds"))
  
  #### Time Period
  if(time_period == "all")       data <- data %>% filter(year >= 1996)
  if(time_period == "rsdpi_iii") data <- data %>% filter(year >= 1996) %>% filter(year <= 2009)
  if(time_period == "rsdpiv")    data <- data %>% filter(year >= 2012)
  
  #### Prep RSDP IV Variables
  if(time_period == "rsdpiv"){
    data$dmspols_harmon_ihs_1996 <- data$viirs_bm_ihs_2011
    data$dmspols_harmon_ihs      <- data$viirs_bm_ihs
    
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

for(log in c("_log")){
  for(theta in c("3_8")){ # 
    for(exclude in c("_exclude20km", "_exclude50km", "_exclude100km")){ 
      for(time_period in c("all", "rsdpi_iii", "rsdpiv")){
        
        data_kebele <- prep_data("kebele", log, theta, exclude, time_period)
        data_woreda <- prep_data("woreda", log, theta, exclude, time_period)
        
        ## OLS - Kebele
        ols1k <- felm(dmspols_harmon_ihs ~ MA_var                            + temp_avg + precipitation | year + cell_id | 0                     | cluster_var, data = data_kebele)
        ols2k <- felm(dmspols_harmon_ihs ~ MA_var + MA_varXwor_ntlgroup_2bin + temp_avg + precipitation | year + cell_id | 0  | cluster_var, data = data_kebele)
        
        ols3k <- felm(globcover_urban_sum_ihs ~ MA_var                            + temp_avg + precipitation | year + cell_id | 0                     | cluster_var, data = data_kebele)
        ols4k <- felm(globcover_urban_sum_ihs ~ MA_var + MA_varXwor_ntlgroup_2bin + temp_avg + precipitation | year + cell_id | 0  | cluster_var, data = data_kebele)
        
        ols5k <- felm(globcover_cropland_sum_ihs ~ MA_var                            + temp_avg + precipitation | year + cell_id | 0                     | cluster_var, data = data_kebele)
        ols6k <- felm(globcover_cropland_sum_ihs ~ MA_var + MA_varXwor_ntlgroup_2bin + temp_avg + precipitation | year + cell_id | 0  | cluster_var, data = data_kebele)
        
        ## OLS - Woreda
        ols1w <- felm(dmspols_harmon_ihs ~ MA_var                            + temp_avg + precipitation | year + cell_id | 0                     | cluster_var, data = data_woreda)
        ols2w <- felm(dmspols_harmon_ihs ~ MA_var + MA_varXwor_ntlgroup_2bin + temp_avg + precipitation | year + cell_id | 0  | cluster_var, data = data_woreda)
        
        ols3w <- felm(globcover_urban_sum_ihs ~ MA_var                            + temp_avg + precipitation | year + cell_id | 0                     | cluster_var, data = data_woreda)
        ols4w <- felm(globcover_urban_sum_ihs ~ MA_var + MA_varXwor_ntlgroup_2bin + temp_avg + precipitation | year + cell_id | 0  | cluster_var, data = data_woreda)
        
        ols5w <- felm(globcover_cropland_sum_ihs ~ MA_var                            + temp_avg + precipitation | year + cell_id | 0                     | cluster_var, data = data_woreda)
        ols6w <- felm(globcover_cropland_sum_ihs ~ MA_var + MA_varXwor_ntlgroup_2bin + temp_avg + precipitation | year + cell_id | 0  | cluster_var, data = data_woreda)
        
        ## IV - Kebele
        iv1k <- felm(dmspols_harmon_ihs ~ temp_avg + precipitation           | year + cell_id | (MA_var ~ MA_var_exc) |                                                                       cluster_var, data = data_kebele) 
        iv2k <- felm(dmspols_harmon_ihs ~ temp_avg + precipitation           | year + cell_id | (MA_var|MA_varXwor_ntlgroup_2bin ~ MA_var_exc + MA_var_excXwor_ntlgroup_2bin) | cluster_var, data = data_kebele) 
        
        iv3k <- felm(globcover_urban_sum_ihs ~ temp_avg + precipitation      | year + cell_id | (MA_var ~ MA_var_exc) |                                                                       cluster_var, data = data_kebele) 
        iv4k <- felm(globcover_urban_sum_ihs ~ temp_avg + precipitation      | year + cell_id | (MA_var|MA_varXwor_ntlgroup_2bin ~ MA_var_exc + MA_var_excXwor_ntlgroup_2bin) | cluster_var, data = data_kebele) 
        
        iv5k <- felm(globcover_cropland_sum_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var ~ MA_var_exc) |                                                                       cluster_var, data = data_kebele) 
        iv6k <- felm(globcover_cropland_sum_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXwor_ntlgroup_2bin ~ MA_var_exc + MA_var_excXwor_ntlgroup_2bin) | cluster_var, data = data_kebele) 
        
        ## IV - Woreda
        iv1w <- felm(dmspols_harmon_ihs ~ temp_avg + precipitation           | year + cell_id | (MA_var ~ MA_var_exc) |                                                                       cluster_var, data = data_woreda) 
        iv2w <- felm(dmspols_harmon_ihs ~ temp_avg + precipitation           | year + cell_id | (MA_var|MA_varXwor_ntlgroup_2bin ~ MA_var_exc + MA_var_excXwor_ntlgroup_2bin) | cluster_var, data = data_woreda) 
        
        iv3w <- felm(globcover_urban_sum_ihs ~ temp_avg + precipitation      | year + cell_id | (MA_var ~ MA_var_exc) |                                                                       cluster_var, data = data_woreda) 
        iv4w <- felm(globcover_urban_sum_ihs ~ temp_avg + precipitation      | year + cell_id | (MA_var|MA_varXwor_ntlgroup_2bin ~ MA_var_exc + MA_var_excXwor_ntlgroup_2bin) | cluster_var, data = data_woreda) 
        
        iv5w <- felm(globcover_cropland_sum_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var ~ MA_var_exc) |                                                                       cluster_var, data = data_woreda) 
        iv6w <- felm(globcover_cropland_sum_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXwor_ntlgroup_2bin ~ MA_var_exc + MA_var_excXwor_ntlgroup_2bin) | cluster_var, data = data_woreda) 
        
        ## OLS Stargazer
        stargazer(ols1k,
                  ols2k,
                  ols3k,
                  ols4k,
                  ols5k,
                  ols6k,
                  ols1w,
                  ols2w,
                  ols3w,
                  ols4w,
                  ols5w,
                  ols6w,
                  dep.var.labels.include = T,
                  dep.var.labels   = c("NTL", "Urban", "Cropland",
                                       "NTL", "Urban", "Cropland"), # "NTL$\\geq$2", "NTL$\\geq6$",
                  #keep=c("MA_var"),
                  omit = c("temp_avg", "precipitation"),
                  covariate.labels = c("MA",
                                       "MA$\\times NTL_{96}$ Lit"),
                  dep.var.caption = "",
                  omit.stat = c("f","ser"), 
                  align=TRUE,
                  no.space=TRUE,
                  float=FALSE,
                  column.sep.width = "8pt",
                  digits = 2,
                  omit.table.layout = "n",
                  add.lines = list(
                    c("Unit", rep("Keb.", 6), rep("Wor.", 6)),
                    c("Year FE", rep("Y", 12)),
                    c("Unit FE", rep("Y", 12))
                  ),
                  out=file.path(paper_tables,
                                paste0("MA_table",log,"_theta",theta,"_",unit,"_",time_period,"_ols_2ntlgroups.tex")))
        
        ## IV Stargazer
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
                                       "NTL", "Urban", "Cropland"), # "NTL$\\geq$2", "NTL$\\geq6$",
                  #keep=c("MA_var"),
                  omit = c("temp_avg", "precipitation"),
                  covariate.labels = c("MA",
                                       "MA$\\times NTL_{96}$ Lit"),
                  dep.var.caption = "",
                  omit.stat = c("f","ser"), 
                  align=TRUE,
                  no.space=TRUE,
                  float=FALSE,
                  column.sep.width = "8pt",
                  digits = 2,
                  omit.table.layout = "n",
                  add.lines = list(
                    c("Unit", rep("Keb.", 6), rep("Wor.", 6)),
                    c("Year FE", rep("Y", 12)),
                    c("Unit FE", rep("Y", 12))
                  ),
                  out=file.path(paper_tables,
                                paste0("MA_table",log,"_theta",theta,exclude,"_",unit,"_",time_period,"_iv_2ntlgroups.tex")))
        
      }
    }
  }
}

# Cleanup ----------------------------------------------------------------------
OBJECTS_AFTER_CODE <- ls()
OBJECTS_TO_DELETE <- setdiff(OBJECTS_AFTER_CODE, OBJECTS_BEFORE_CODE)
rm(list = OBJECTS_TO_DELETE)

gc(); gc(); gc()





