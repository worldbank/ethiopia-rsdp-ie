# Exploratory Analysis

# APPROACH: Somewhat follow the Haiti paper. There it seems road improvement
# kinda random throughout their years. Here that's not the case, but maybe
# can assume that within an RDSP phase?

# OUTLINE
# 1. Overall impact of each phase.
# 2. Heterogeneity of impact, within each phase
#    2.1. Road type 
#    2.2. Baseline Dep Var (for ntl, num or divide into thirds a la aiddata?)
#    2.3. Distance to City (could break down by city pop)

# DEPENDENT VARIABLES
# 1. dmspols_zhang_ihs
# 2. globcover_urban
# 3. Cropland? NDVI?

# PRESENT RESULTS
# 1. Post-treatment
# 2. Coef-plots. In same plot. All cells and (for hetro), below/above cutoffs (median / quartiles). Super important to see pre-trends.

calc_ihs <- function(x) log(x + sqrt(x^2 + 1))

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data_clean.Rds"))

data$far_addis <- as.numeric(data$distance_city_addisababa >= 100*1000)

data$post_improvedroad_50aboveafter[is.na(data$post_improvedroad_50aboveafter) & !is.na(data$post_improvedroad)] <- 0
data$post_improvedroad_below50after[is.na(data$post_improvedroad_below50after) & !is.na(data$post_improvedroad)] <- 0

data$viirs_mean_above1 <- as.numeric(data$viirs_mean >= 1)
data$viirs_mean_above5 <- as.numeric(data$viirs_mean >= 5)

data$viirs_max <- data$viirs_max %>% calc_ihs()
data$viirs_mean <- data$viirs_mean %>% calc_ihs()
data$viirs_median <- data$viirs_median %>% calc_ihs()

#### Treatment Vars
data$years_since_improvedroad <- data$years_since_improvedroad %>% as.character() %>% as.numeric()
data$pre_improvedroad_neg2_5 <- as.numeric(data$years_since_improvedroad %in% -2:-5) %>% as.numeric()
data$pre_improvedroad_neg6_10 <- as.numeric(data$years_since_improvedroad %in% -6:-10) %>% as.numeric()

data$years_since_improvedroad_50aboveafter <- data$years_since_improvedroad_50aboveafter %>% as.character() %>% as.numeric()
data$pre_improvedroad_50aboveafter_neg2_5 <- as.numeric(data$years_since_improvedroad_50aboveafter %in% -2:-5) %>% as.numeric()
data$pre_improvedroad_50aboveafter_neg6_10 <- as.numeric(data$years_since_improvedroad_50aboveafter %in% -6:-10) %>% as.numeric()

data$years_since_improvedroad_below50after <- data$years_since_improvedroad_below50after %>% as.character() %>% as.numeric()
data$pre_improvedroad_below50after_neg2_5 <- as.numeric(data$years_since_improvedroad_below50after %in% -2:-5) %>% as.numeric()
data$pre_improvedroad_below50after_neg6_10 <- as.numeric(data$years_since_improvedroad_below50after %in% -6:-10) %>% as.numeric()

# Export Results ---------------------------------------------------------------
if(F){
  dv <- "dmspols_zhang_ihs"
  addis_distance <- "Far"
  cluster_var <- "woreda_hdx_z_code"
  time_period <- "all"
}

# All Phases Together ----------------------------------------------------------
#for(dv in c("viirs_mean","viirs_median", "viirs_max", "viirs_mean_above1", "ndvi_cropland", "ndvi", "dmspols_zhang_ihs", "dmspols_zhang_6", "globcover_urban", "globcover_cropland")){ 
#  for(addis_distance in c("All", "Far")){
#    for(cluster_var in c("woreda_hdx_w_uid", "woreda_hdx_z_code")){

#for(dv in c("viirs_mean","viirs_median", "viirs_max", "viirs_mean_above1","viirs_mean_above5","ndvi_cropland", "ndvi", "globcover_urban", "globcover_cropland", "dmspols_zhang_ihs", "dmspols_zhang_6")){ 
for(base_ntl in c(1,2,3,123)){
  for(addis_distance in c("All", "Far")){ # "All", "Far"
    for(cluster_var in c("woreda_hdx_w_uid")){      
      for(time_period in c("all")){ # "all", "viirs_time"
        
        # For viirs_time, only consider phase_all
        #if(time_period %in% "viirs_time"){
        #  if(grepl("dmsp", dv)) next
        #}
        
        time_period_name <- ""
        if(time_period %in% "viirs_time") time_period_name <- "viirs_time"
        
        #Sys.sleep(.1)
        #for(i in 1:5) gc()
        #Sys.sleep(.1)
        
        print(paste(dv, addis_distance,cluster_var, "-----------------"))
        
        #### If DATASET_TYPE is woreda, skip select units
        if(DATASET_TYPE %in% "woreda_panel_hdx_csa"){
          #if(cluster_var %in% "woreda_hdx_w_uid") cluster_var <- "uid"
          #if(cluster_var %in% "woreda_hdx_z_code") cluster_var <- "Z_CODE"
          unit <- "woreda"
        } else{
          unit <- "cell"
        }
        
        #### Define Dependent Variable and Cluster Variable
        data$dv <- data[[dv]]
        data$cluster_var <- data[[cluster_var]]
        
        #### Subset by Addis Distance
        if(addis_distance %in% "Far"){
          data_temp <- data[data$far_addis %in% 1,]
          #data_w_temp <- data_w[data_w$far_addis %in% 1,]
        } else{
          data_temp <- data
          #data_w_temp <- data_w
        }
        
        if(!(base_ntl %in% 123)){
          data_temp <- data_temp[data_temp$dmspols_1996_group_woreda %in% base_ntl,]
        }
        
        #### Create Dependent Variable Label
        if(dv %in% "dmspols_zhang_ihs") dep_var_label <- "DMSP-OLS: IHS"
        if(dv %in% "dmspols_zhang_6") dep_var_label <- "DMSP-OLS: Above Median"
        if(dv %in% "globcover_urban") dep_var_label <- "Globcover: Urban"
        if(dv %in% "globcover_cropland") dep_var_label <- "Globcover: Cropland"
        if(dv %in% "ndvi") dep_var_label <- "NDVI"
        if(dv %in% "ndvi_cropland") dep_var_label <- "NDVI in Cropland Areas"
        
        if(dv %in% "viirs_mean") dep_var_label <- "VIIRS: Mean"
        if(dv %in% "viirs_median") dep_var_label <- "VIIRS: Median"
        if(dv %in% "viirs_max") dep_var_label <- "VIIRS: Max"
        if(dv %in% "viirs_mean_above1") dep_var_label <- "VIIRS: Mean Above 1"
        
        if(dv %in% c("ndvi", "ndvi_cropland")){
          control_vars <- "+ temp_avg + precipitation"
        } else{
          #control_vars <- ""
          control_vars <- "+ temp_avg + precipitation"
        }
        
        #### VIIRS Restrict Analysis
        if((time_period %in% "viirs_time")){
          # Restrict to years with VIIRs data
          data_temp <- data_temp[data_temp$year %in% 2012:2019,]
          
          # Restrict based on year improved, making sure at least 1 year
          # before/after
          data_temp <- data_temp[data_temp$year_improvedroad %in% 2013:2016,]
        }
        
        
        #   globcover_urban ndvi_cropland viirs_mean
        
        #### DMSP-OLS
        lm_dmsp <- felm(as.formula(as.formula(paste("dmspols_zhang_ihs ~ ",
                                                    "post_improvedroad_t1 + ",
                                                    "post_improvedroad_t2 + ",
                                                    "post_improvedroad_t3 ",
                                                    control_vars,
                                                    "| cell_id + year | 0 | cluster_var"))), data=data_temp)
        
        lm_50_dmsp <- felm(as.formula(as.formula(paste("dmspols_zhang_ihs ~ ",
                                                       "post_improvedroad_50aboveafter_t1 + ",
                                                       "post_improvedroad_50aboveafter_t2 + ",
                                                       "post_improvedroad_50aboveafter_t3 + ",
                                                       "post_improvedroad_below50after_t1 + ",
                                                       "post_improvedroad_below50after_t2 + ",
                                                       "post_improvedroad_below50after_t3 ",
                                                       control_vars,
                                                       "| cell_id + year | 0 | cluster_var"))), data=data_temp)
        
        #### Urban
        lm_urban <- felm(as.formula(as.formula(paste("globcover_urban ~ ",
                                                     "post_improvedroad_t1 + ",
                                                     "post_improvedroad_t2 + ",
                                                     "post_improvedroad_t3 ",
                                                     control_vars,
                                                     "| cell_id + year | 0 | cluster_var"))), data=data_temp)
        
        lm_50_urban <- felm(as.formula(as.formula(paste("globcover_urban ~ ",
                                                        "post_improvedroad_50aboveafter_t1 + ",
                                                        "post_improvedroad_50aboveafter_t2 + ",
                                                        "post_improvedroad_50aboveafter_t3 + ",
                                                        "post_improvedroad_below50after_t1 + ",
                                                        "post_improvedroad_below50after_t2 + ",
                                                        "post_improvedroad_below50after_t3 ",
                                                        control_vars,
                                                        "| cell_id + year | 0 | cluster_var"))), data=data_temp)
        
        #### Cropalned
        lm_cropland <- felm(as.formula(as.formula(paste("globcover_cropland ~ ",
                                                        "post_improvedroad_t1 + ",
                                                        "post_improvedroad_t2 + ",
                                                        "post_improvedroad_t3 ",
                                                        control_vars,
                                                        "| cell_id + year | 0 | cluster_var"))), data=data_temp)
        
        lm_50_cropland <- felm(as.formula(as.formula(paste("globcover_cropland ~ ",
                                                           "post_improvedroad_50aboveafter_t1 + ",
                                                           "post_improvedroad_50aboveafter_t2 + ",
                                                           "post_improvedroad_50aboveafter_t3 + ",
                                                           "post_improvedroad_below50after_t1 + ",
                                                           "post_improvedroad_below50after_t2 + ",
                                                           "post_improvedroad_below50after_t3 ",
                                                           control_vars,
                                                           "| cell_id + year | 0 | cluster_var"))), data=data_temp)
        
        #### ndvi_cropland
        lm_ndvicrop <- felm(as.formula(as.formula(paste("ndvi_cropland ~ ",
                                                        "post_improvedroad_t1 + ",
                                                        "post_improvedroad_t2 + ",
                                                        "post_improvedroad_t3 ",
                                                        control_vars,
                                                        "| cell_id + year | 0 | cluster_var"))), data=data_temp)
        
        lm_50_ndvicrop <- felm(as.formula(as.formula(paste("ndvi_cropland ~ ",
                                                           "post_improvedroad_50aboveafter_t1 + ",
                                                           "post_improvedroad_50aboveafter_t2 + ",
                                                           "post_improvedroad_50aboveafter_t3 + ",
                                                           "post_improvedroad_below50after_t1 + ",
                                                           "post_improvedroad_below50after_t2 + ",
                                                           "post_improvedroad_below50after_t3 ",
                                                           control_vars,
                                                           "| cell_id + year | 0 | cluster_var"))), data=data_temp)
        
        #### VIIRS
        data_temp <- data_temp[data_temp$year %in% 2012:2019,]
        data_temp <- data_temp[data_temp$year_improvedroad %in% 2013:2016,]
        
        lm_viirs <- felm(as.formula(as.formula(paste("viirs_mean ~ ",
                                                     "post_improvedroad_t1 + ",
                                                     "post_improvedroad_t2 + ",
                                                     "post_improvedroad_t3 ",
                                                     control_vars,
                                                     "| cell_id + year | 0 | cluster_var"))), data=data_temp)
        
        lm_50_viirs <- felm(as.formula(as.formula(paste("viirs_mean ~ ",
                                                        "post_improvedroad_50aboveafter_t1 + ",
                                                        "post_improvedroad_50aboveafter_t2 + ",
                                                        "post_improvedroad_50aboveafter_t3 + ",
                                                        "post_improvedroad_below50after_t1 + ",
                                                        "post_improvedroad_below50after_t2 + ",
                                                        "post_improvedroad_below50after_t3 ",
                                                        control_vars,
                                                        "| cell_id + year | 0 | cluster_var"))), data=data_temp)
        
        
        stargazer(lm_dmsp, lm_50_dmsp, 
                  lm_urban, lm_50_urban, 
                  lm_cropland, lm_50_cropland, 
                  lm_ndvicrop, lm_50_ndvicrop, 
                  lm_viirs, lm_50_viirs,
                  dep.var.labels.include = T,
                  dep.var.labels = c("DMSP", "DMSP",
                                     "Urban", "Urban",
                                     "Crop", "Crop",
                                     "NDVI-Crop", "NDVI-Crop",
                                     "VIIRS", "VIIRS"),
                  keep = c("post_improvedroad_t1",
                           "post_improvedroad_t2",
                           "post_improvedroad_t3",
                           "post_improvedroad_below50after_t1",
                           "post_improvedroad_below50after_t2",
                           "post_improvedroad_below50after_t3",
                           "post_improvedroad_50aboveafter_t1",
                           "post_improvedroad_50aboveafter_t2",
                           "post_improvedroad_50aboveafter_t3"),
                  order = c("post_improvedroad_t1",
                            "post_improvedroad_t2",
                            "post_improvedroad_t3",
                            "post_improvedroad_below50after_t1",
                            "post_improvedroad_below50after_t2",
                            "post_improvedroad_below50after_t3",
                            "post_improvedroad_50aboveafter_t1",
                            "post_improvedroad_50aboveafter_t2",
                            "post_improvedroad_50aboveafter_t3"),
                  covariate.labels =   c(
                    "Near Improved Rd: Yr 1",
                    "Near Improved Rd: Yr 2",
                    "Near Improved Rd: Yr 3",
                    "Near Improved Rd. $<$50km/hr: Yr 1",
                    "Near Improved Rd. $<$50km/hr: Yr 2",
                    "Near Improved Rd. $<$50km/hr: Yr 3",
                    "Near Improved Rd. $>=$50km/hr: Yr 1",
                    "Near Improved Rd. $>=$50km/hr: Yr 2",
                    "Near Improved Rd. $>=$50km/hr: Yr 3"),
                  omit.stat = c("f","ser"),
                  align=TRUE,
                  no.space=TRUE,
                  float=FALSE,
                  column.sep.width="-15pt",
                  digits=2,
                  add.lines = list(
                    c("Cell FE", rep("Y", 10)),
                    c("Year FE", rep("Y", 10))),
                  out = file.path(tables_file_path, paste0("results_did_t123_addisdistance",addis_distance,"_clustervar",cluster_var,"_unit",unit,time_period_name,"_basentl",base_ntl,".tex")))
        print(paste0("results_did_t123_addisdistance",addis_distance,"_clustervar",cluster_var,"_unit",unit,time_period_name,"_basentl",base_ntl,".tex"))
        
      }
    }
  }
}


