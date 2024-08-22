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
theta <- "3_8"
exclude <- "_exclude50km"
start_year <- "1996"
end_year <- "2009"
trans_type <- "log"

unit <- "kebele"



print(paste(log, theta, exclude, start_year, end_year))

if((start_year == "2012") & (end_year == "2009")) next

data_kebele <- prep_data("kebele", log, theta, exclude, start_year, end_year)
data_woreda <- prep_data("woreda", log, theta, exclude, start_year, end_year)

data_kebele$globcover_urban %>% summary()
data_woreda$globcover_urban %>% summary()

data_kebele$dmspols_harmon %>% summary()
data_woreda$dmspols_harmon %>% summary()

data_kebele$globcover_urban_1996 %>% mean()
(data_kebele$dmspols_harmon_1996 != 0) %>% mean()

(0.03996436 - 0.003650545) / 0.003650545
