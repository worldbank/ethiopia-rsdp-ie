# Market Access Analysis

unit <- "woreda"
theta <- "3_8"
log <- "_log"
exclude <- "_exclude20km"
dv <- "dmspols_zhang_ihs"
MA_ubanrural <- ""

# Regressions ------------------------------------------------------------------
for(unit in c("clusters_of_ntl", "clusters_of_globcover_urban")){ # "woreda ,"clusters_of_ntl", "clusters_of_globcover_urban"
  for(theta in c("3_8")){ # "1","2","5","8"
    for(log in c("_log")){
      for(exclude in c("_exclude50km")){ # "_exclude20km", "_exclude100km","_exclude100km"
        for(dv in c("globcover_urban_sum_ihs", "dmspols_zhang_sum2_ihs", "dmspols_zhang_sum6_ihs", "dmspols_zhang_ihs", "dmspols_zhang_sum_ihs")){
          for(MA_ubanrural in c("")){ # "_urban2", "_rural2"
            
            if(dv %in% "globcover_urban_sum_ihs") dv_label <- "Urban (as defined by Globcover)"
            if(dv %in% "dmspols_zhang_sum2_ihs")  dv_label <- "NTL$>$2"
            if(dv %in% "dmspols_zhang_sum6_ihs")  dv_label <- "NTL$>$6"
            if(dv %in% "dmspols_zhang_ihs")       dv_label <- "IHS(NTL)"
            if(dv %in% "dmspols_zhang_sum_ihs")   dv_label <- "IHS(NTL), Sum"
            
            ## Load/Subset Data
            data <- readRDS(file.path(panel_rsdp_imp_data_file_path, unit, "merged_datasets", "panel_data_clean.Rds"))
            
            data <- data %>%
              filter(year >= 1996)
            
            ## Prep Vars
            data$MA_var     <- data[[paste0("MA_pop2000_tt_theta",theta,MA_ubanrural, log)]]
            data$MA_var_exc <- data[[paste0("MA_pop2000_tt_theta",theta,exclude,MA_ubanrural, log)]]
            data$dv         <- data[[dv]]
            
            data$distance_city_addisababa <- data$distance_city_addisababa / 1000 / 100
            data$globcover_urban_1996 <- as.numeric(data$globcover_urban_1996 > 0)
            
            ## Prep Interaction Vars
            data$MA_varXdmspols_zhang_ihs_1996   <- data$MA_var * data$dmspols_zhang_ihs_1996
            #data$MA_varXdmspols_2bin_1996        <- data$MA_var * data$dmspols_2bin_1996
            #data$MA_varXdmspols_6bin_1996        <- data$MA_var * data$dmspols_6bin_1996
            data$MA_varXglobcover_urban_1996     <- data$MA_var * data$globcover_urban_1996
            data$MA_varXdistance_city_addisababa <- data$MA_var * data$distance_city_addisababa
            data$MA_varXglobcover_urban_sum_ihs_1996 <- data$MA_var * data$globcover_urban_sum_ihs_1996
            data$MA_varXdmspols_zhang_sum2_ihs_1996 <- data$MA_var * data$dmspols_zhang_sum2_ihs_1996
            data$MA_varXdmspols_zhang_sum6_ihs_1996 <- data$MA_var * data$dmspols_zhang_sum6_ihs_1996
            data$MA_varXdmspols_zhang_ihs_1996 <- data$MA_var * data$dmspols_zhang_ihs_1996
            
            data$MA_var_excXdmspols_zhang_ihs_1996   <- data$MA_var_exc * data$dmspols_zhang_ihs_1996
            #data$MA_var_excXdmspols_2bin_1996        <- data$MA_var_exc * data$dmspols_2bin_1996
            #data$MA_var_excXdmspols_6bin_1996        <- data$MA_var_exc * data$dmspols_6bin_1996
            data$MA_var_excXglobcover_urban_1996     <- data$MA_var_exc * data$globcover_urban_1996
            data$MA_var_excXdistance_city_addisababa <- data$MA_var_exc * data$distance_city_addisababa
            data$MA_var_excXglobcover_urban_sum_ihs_1996 <- data$MA_var_exc * data$globcover_urban_sum_ihs_1996
            data$MA_var_excXdmspols_zhang_sum2_ihs_1996  <- data$MA_var_exc * data$dmspols_zhang_sum2_ihs_1996
            data$MA_var_excXdmspols_zhang_sum6_ihs_1996  <- data$MA_var_exc * data$dmspols_zhang_sum6_ihs_1996
            data$MA_var_excXdmspols_zhang_ihs_1996       <- data$MA_var_exc * data$dmspols_zhang_ihs_1996
            
            ## Regressions
            ols1 <- felm(dv ~ MA_var_exc                                       + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data) %>% adj_ols_var_names()
            ols2 <- felm(dv ~ MA_var_exc + MA_var_excXdmspols_zhang_ihs_1996   + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data) %>% adj_ols_var_names()
            ols3 <- felm(dv ~ MA_var_exc + MA_var_excXdmspols_zhang_sum2_ihs_1996        + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data) %>% adj_ols_var_names()
            ols4 <- felm(dv ~ MA_var_exc + MA_var_excXdmspols_zhang_sum6_ihs_1996        + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data) %>% adj_ols_var_names()
            ols5 <- felm(dv ~ MA_var_exc + MA_var_excXglobcover_urban_sum_ihs_1996     + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data) %>% adj_ols_var_names()
            ols6 <- felm(dv ~ MA_var_exc + MA_var_excXdistance_city_addisababa + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data) %>% adj_ols_var_names()
            
            iv1  <- felm(dv ~ temp_avg + precipitation   | year + cell_id | (MA_var ~ MA_var_exc) |                                                                       Z_CODE, data = data) %>% adj_iv_var_names()
            iv2  <- felm(dv ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdmspols_zhang_ihs_1996   ~ MA_var_exc + MA_var_excXdmspols_zhang_ihs_1996)   | Z_CODE, data = data) %>% adj_iv_var_names()
            iv3  <- felm(dv ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdmspols_zhang_sum2_ihs_1996        ~ MA_var_exc + MA_var_excXdmspols_zhang_sum2_ihs_1996)        | Z_CODE, data = data) %>% adj_iv_var_names()
            iv4  <- felm(dv ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdmspols_zhang_sum6_ihs_1996        ~ MA_var_exc + MA_var_excXdmspols_zhang_sum6_ihs_1996)        | Z_CODE, data = data) %>% adj_iv_var_names()
            iv5  <- felm(dv ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXglobcover_urban_sum_ihs_1996     ~ MA_var_exc + MA_var_excXglobcover_urban_sum_ihs_1996)     | Z_CODE, data = data) %>% adj_iv_var_names()
            iv6  <- felm(dv ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdistance_city_addisababa ~ MA_var_exc + MA_var_excXdistance_city_addisababa) | Z_CODE, data = data) %>% adj_iv_var_names()
            
            ## Regressions
            # ols1 <- felm(dv ~ MA_var_exc                                       + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data) %>% adj_ols_var_names()
            # ols2 <- felm(dv ~ MA_var_exc + MA_var_excXdmspols_zhang_ihs_1996   + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data) %>% adj_ols_var_names()
            # ols3 <- felm(dv ~ MA_var_exc + MA_var_excXdmspols_2bin_1996        + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data) %>% adj_ols_var_names()
            # ols4 <- felm(dv ~ MA_var_exc + MA_var_excXdmspols_6bin_1996        + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data) %>% adj_ols_var_names()
            # ols5 <- felm(dv ~ MA_var_exc + MA_var_excXglobcover_urban_1996     + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data) %>% adj_ols_var_names()
            # ols6 <- felm(dv ~ MA_var_exc + MA_var_excXdistance_city_addisababa + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data) %>% adj_ols_var_names()
            # 
            # iv1  <- felm(dv ~ temp_avg + precipitation   | year + cell_id | (MA_var ~ MA_var_exc) |                                                                       Z_CODE, data = data) %>% adj_iv_var_names()
            # iv2  <- felm(dv ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdmspols_zhang_ihs_1996   ~ MA_var_exc + MA_var_excXdmspols_zhang_ihs_1996)   | Z_CODE, data = data) %>% adj_iv_var_names()
            # iv3  <- felm(dv ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdmspols_2bin_1996        ~ MA_var_exc + MA_var_excXdmspols_2bin_1996)        | Z_CODE, data = data) %>% adj_iv_var_names()
            # iv4  <- felm(dv ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdmspols_6bin_1996        ~ MA_var_exc + MA_var_excXdmspols_6bin_1996)        | Z_CODE, data = data) %>% adj_iv_var_names()
            # iv5  <- felm(dv ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXglobcover_urban_1996     ~ MA_var_exc + MA_var_excXglobcover_urban_1996)     | Z_CODE, data = data) %>% adj_iv_var_names()
            # iv6  <- felm(dv ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdistance_city_addisababa ~ MA_var_exc + MA_var_excXdistance_city_addisababa) | Z_CODE, data = data) %>% adj_iv_var_names()
            # 
            stargazer(ols1,
                      ols2,
                      ols3,
                      ols4,
                      ols5,
                      ols6,
                      iv1,
                      iv2,
                      iv3,
                      iv4,
                      iv5,
                      iv6,
                      dep.var.labels.include = T,
                      dep.var.labels   = dv_label,
                      #keep=c("MA_var"),
                      omit = c("temp_avg", "precipitation"),
                      covariate.labels = c("MA",
                                           "MA X NTL (1996)",
                                           "MA X NTL$>$2 (1996)",
                                           "MA X NTL$>$6 (1996)",
                                           "MA X Urban (1996)",
                                           "MA X Dist Addis (100km)"),
                      dep.var.caption = "",
                      omit.stat = c("f","ser"), 
                      align=TRUE,
                      no.space=TRUE,
                      float=FALSE,
                      column.sep.width = "8pt",
                      digits = 2,
                      add.lines = list(
                        c("Year   FE", rep("Y", 12)),
                        c("Woreda FE", rep("Y", 12))
                      ),
                      out=file.path(paper_tables,
                                    paste0("MA_table",log,"_theta",theta,exclude,"_",unit,"_",dv,MA_ubanrural,".tex")))
          }
        }
      }
    }
  }
}




