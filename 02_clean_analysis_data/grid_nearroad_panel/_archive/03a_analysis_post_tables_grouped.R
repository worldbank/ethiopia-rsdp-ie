# Exploratory Analysis

# APPROACH: Somewhat follow the Haiti paper. There it seems road improvement
# kinda random throughout their years. Here that's not the case, but maybe
# can assume that within an RDSP phase?

#Sys.setenv('R_MAX_VSIZE'=32000000000)
#source("~/Documents/Github/Ethiopia-Corridors-IE/Code/_ethiopia_ie_master.R")

road_years_group <- "all"
dv <- "viirs_mean"
addis_distance <- "Far"
cluster_var <- "woreda_hdx_z_code"
time_period <- "all"

lm_results_all_df <- data.frame(NULL)

# Load Data --------------------------------------------------------------------
for(road_years_group in c("all", 
                          "dmspols",
                          "viirs",
                          "phase1",
                          "phase2",
                          "phase3",
                          "phase4")){
  
  #### Where are we?
  print(paste(road_years_group, "============================================"))
  
  #### Load year_group dataset
  data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", paste0("grid_data_clean_",road_years_group,".Rds")))
  
  # All Phases Together ----------------------------------------------------------
  for(dv in c("viirs_mean_ihs", "viirs_mean_2","ndvi_cropland", "ndvi", "globcover_urban", "globcover_cropland", "dmspols_zhang_ihs", "dmspols_zhang_2")){ 
    for(addis_distance in c("All", "Far")){ # "All", "Far"
      for(cluster_var in c("woreda_hdx_w_uid")){      
        
        #### Skip some cases
        # If in viirs time period, skip dmspols variables - and vice-versa
        if(road_years_group %in% "viirs"){
          if(grepl("dmspols", dv)){
            next
          } 
        }
        
        if(road_years_group %in% "dmspols"){
          if(grepl("viirs", dv)){
            next
          } 
        }
        
        #### Where are we?
        print(paste(road_years_group, dv, addis_distance, cluster_var, "-----------------"))
        
        #### Create temporary dataset
        if(addis_distance %in% "Far"){
          data_temp <- data[data$far_addis %in% 1,]
        } else{
          data_temp <- data
        }
        
        #### Define Dependent Variable and Cluster Variable
        data_temp$dv          <- data_temp[[dv]]
        data_temp$cluster_var <- data_temp[[cluster_var]]
        
        #### Define unit name for output filename
        if(DATASET_TYPE %in% "woreda_panel_hdx_csa"){
          unit <- "woreda"
        } else{
          unit <- "cell"
        }
        
        #### Create Dependent Variable Label
        if(dv %in% "dmspols_zhang_ihs")  dep_var_label <- "DMSP-OLS: IHS"
        if(dv %in% "dmspols_zhang_6")    dep_var_label <- "DMSP-OLS: Above Median"
        if(dv %in% "globcover_urban")    dep_var_label <- "Globcover: Urban"
        if(dv %in% "globcover_cropland") dep_var_label <- "Globcover: Cropland"
        if(dv %in% "ndvi")               dep_var_label <- "NDVI"
        if(dv %in% "ndvi_cropland")      dep_var_label <- "NDVI in Cropland Areas"
        
        if(dv %in% "viirs_mean_ihs")         dep_var_label <- "VIIRS: Mean IHS"
        if(dv %in% "viirs_median_ihs")       dep_var_label <- "VIIRS: Median IHS"
        if(dv %in% "viirs_max_ihs")          dep_var_label <- "VIIRS: Max IHS"
        if(dv %in% "viirs_mean_2")  dep_var_label <- "VIIRS: Mean Above 2"
        if(dv %in% "viirs_mean_6")  dep_var_label <- "VIIRS: Mean Above 6"
        
        #### Control variables
        control_vars <- "+ temp_avg + precipitation"
        
        #### Lagged Variables
        if(road_years_group %in% "viirs"){
          #lm_lagged <- "pre_improvedroad_neg2_5"
          #lm_50_lagged <- "pre_improvedroad_50aboveafter_neg2_5 + pre_improvedroad_below50after_neg2_5"
          lm_lagged <- "pre_improvedroad_neg6_10 + pre_improvedroad_neg2_5"
          lm_50_lagged <- "pre_improvedroad_50aboveafter_neg2_5 + pre_improvedroad_50aboveafter_neg6_10 + pre_improvedroad_below50after_neg2_5 + pre_improvedroad_below50after_neg6_10"
        } else {
          lm_lagged <- "pre_improvedroad_neg6_10 + pre_improvedroad_neg2_5"
          lm_50_lagged <- "pre_improvedroad_50aboveafter_neg2_5 + pre_improvedroad_50aboveafter_neg6_10 + pre_improvedroad_below50after_neg2_5 + pre_improvedroad_below50after_neg6_10"
        }
        
        
        #### Models
        lm             <- felm(as.formula(paste0("dv ~ ",lm_lagged," + post_improvedroad ",control_vars," | cell_id + year | 0 | cluster_var")), data=data_temp)
        lm_baselineNTL <- felm(as.formula(paste0("dv ~ ",lm_lagged," + post_improvedroad + post_improvedroad*dmspols_zhang_1996_group_woreda - dmspols_zhang_1996_group_woreda ",control_vars," | cell_id + year | 0 | cluster_var")), data=data_temp)
        lm_region      <- felm(as.formula(paste0("dv ~ ",lm_lagged," + post_improvedroad + post_improvedroad*region_type - region_type ",control_vars," | cell_id + year | 0 | cluster_var")), data=data_temp)
        
        lm_50          <- felm(as.formula(paste0("dv ~ ",lm_50_lagged," + post_improvedroad_below50after + post_improvedroad_50aboveafter ",control_vars," | cell_id + year | 0 | cluster_var")), data=data_temp)
        
        lm_50_baselineNTL <- felm(as.formula(paste0("dv ~ ",lm_50_lagged," + 
                                  post_improvedroad_below50after + post_improvedroad_50aboveafter +
                                  post_improvedroad_below50after*dmspols_zhang_1996_group_woreda + 
                                  post_improvedroad_50aboveafter*dmspols_zhang_1996_group_woreda -
                                  dmspols_zhang_1996_group_woreda ",control_vars," | cell_id + year | 0 | cluster_var")), data=data_temp)
        
        lm_50_region <- felm(as.formula(paste0("dv ~ ",lm_50_lagged," + 
                             post_improvedroad_below50after + post_improvedroad_50aboveafter +
                             post_improvedroad_below50after*region_type +
                             post_improvedroad_50aboveafter*region_type -
                             region_type ",control_vars," | cell_id + year | 0 | cluster_var")), data=data_temp)
        
        rm(data_temp)
        gc()
        
        lm_results_df <- bind_rows(
          lm_post_confint_tidy(lm) %>% mutate(model_type = "lm"),
          lm_post_confint_tidy(lm_baselineNTL) %>% mutate(model_type = "lm_baselineNTL"),
          lm_post_confint_tidy(lm_region) %>% mutate(model_type = "lm_region"),
          lm_post_confint_tidy(lm_50) %>% mutate(model_type = "lm_50"),
          lm_post_confint_tidy(lm_50_baselineNTL) %>% mutate(model_type = "lm_50_baselineNTL"),
          lm_post_confint_tidy(lm_50_region) %>% mutate(model_type = "lm_50_region")
        ) %>%
          mutate(dv = dv,
                 addis_distance = addis_distance,
                 cluster_var = cluster_var,
                 road_years_group = road_years_group)
        
        lm_results_all_df <- bind_rows(lm_results_all_df, lm_results_df)
        
        stargazer(lm,
                  lm_baselineNTL,
                  lm_region,
                  lm_50,
                  lm_50_baselineNTL,
                  lm_50_region,
                  dep.var.labels.include = F,
                  dep.var.labels = c(dep_var_label),
                  dep.var.caption = "",
                  
                  
                  keep = c("pre_improvedroad_neg6_10",
                           "pre_improvedroad_neg2_5",
                           "pre_improvedroad_below50after_neg2_5",
                           "pre_improvedroad_below50after_neg6_10",
                           "pre_improvedroad_50aboveafter_neg2_5",
                           "pre_improvedroad_50aboveafter_neg6_10",
                           "post_improvedroad",
                           "post_improvedroad:dmspols_zhang_1996_group_woreda",
                           "post_improvedroad:region_type",
                           "post_improvedroad_below50after",
                           "post_improvedroad_below50after:dmspols_zhang_1996_group_woreda",
                           "post_improvedroad_below50after:region_type",
                           "post_improvedroad_50aboveafter",
                           "post_improvedroad_50aboveafter:dmspols_zhang_1996_group_woreda",
                           "post_improvedroad_50aboveafter:region_type"),
                  order = c("^pre_improvedroad_neg6_10$",
                            "^pre_improvedroad_neg2_5$",
                            "^pre_improvedroad_below50after_neg2_5$",
                            "^pre_improvedroad_below50after_neg6_10$",
                            "^pre_improvedroad_50aboveafter_neg2_5$",
                            "^pre_improvedroad_50aboveafter_neg6_10$",
                            "^post_improvedroad$",
                            "^post_improvedroad:dmspols_zhang_1996_group_woreda2$",
                            "^post_improvedroad:dmspols_zhang_1996_group_woreda3$",
                            "^post_improvedroad:region_typeDense$",
                            "^post_improvedroad_below50after$",
                            "^post_improvedroad_below50after:dmspols_zhang_1996_group_woreda2$",
                            "^post_improvedroad_below50after:dmspols_zhang_1996_group_woreda3$",
                            "^post_improvedroad_below50after:region_typeDense$",
                            "^post_improvedroad_50aboveafter$",
                            "^post_improvedroad_50aboveafter:dmspols_zhang_1996_group_woreda2$",
                            "^post_improvedroad_50aboveafter:dmspols_zhang_1996_group_woreda3$",
                            "^post_improvedroad_50aboveafter:region_typeDense$"),
                  covariate.labels =   c(
                    "6-10 Yrs Bfr: Near Improved Rd.", 
                    "2-5 Yrs Bfr: Near Improved Rd.", 
                    
                    "6-10 Yrs Bfr: Near Improved Rd. $<$50km/hr",
                    "2-5 Yrs Bfr: Near Improved Rd. $<$50km/hr",
                    
                    "6-10 Yrs Bfr: Near Improved Rd. $>=$50km/hr",
                    "2-5 Yrs Bfr: Near Improved Rd. $>=$50km/hr",
                    
                    "Near Improved Rd.",
                    "Near Improved Rd. X DMSP Low",
                    "Near Improved Rd. X DMSP High",
                    "Near Improved Rd. X Dense Region",
                    
                    "Near Improved Rd. $<$50km/hr",
                    "Near Improved Rd. $<$50km/hr X DMSP Low",
                    "Near Improved Rd. $<$50km/hr X DMSP High",
                    "Near Improved Rd. $<$50km/hr X Dense Region",
                    
                    "Near Improved Rd. $>=$50km/hr",
                    "Near Improved Rd. $>=$50km/hr X DMSP Low",
                    "Near Improved Rd. $>=$50km/hr X DMSP High",
                    "Near Improved Rd. $>=$50km/hr X Dense Region"),
                  
                  omit.stat = c("f","ser"),
                  align=TRUE,
                  no.space=TRUE,
                  float=FALSE,
                  column.sep.width="-15pt",
                  digits=2,
                  add.lines = list(
                    c("Cell FE", rep("Y", 6)),
                    c("Year FE", rep("Y", 6))),
                  out = file.path(tables_file_path, paste0("results_did_grouped_",dv,"_addisdistance",addis_distance,"_clustervar",cluster_var,"_unit",unit,"_yeargroup_",road_years_group,".tex")))
        
      }
    }
  }
  
  
  # Export results within year group -------------------------------------------
  saveRDS(lm_results_all_df, file.path(finaldata_file_path, DATASET_TYPE, "results", paste0("results_coef_post_yeargroup",road_years_group,".Rds")))
}



