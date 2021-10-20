# Market Access Analysis

unit  <- "woreda"
dv = "dmspols_zhang_ihs"
log = "_log"
theta = "3_8"
MA_ubanrural <- ""

# Regressions ------------------------------------------------------------------
for(unit in c("woreda")){ # "woreda", "clusters_of_ntl"
  for(dv in c("globcover_urban_sum_ihs", "dmspols_zhang_sum2_ihs", "dmspols_zhang_sum6_ihs", "dmspols_zhang_ihs", "dmspols_zhang_sum_ihs")){
    for(log in c("_log")){
      for(theta in c("3_8")){ # "1", "2", "3_8", "5", "8"
        for(exclude in c("_exclude20km", "_exclude50km", "_exclude100km")){ 
          for(MA_ubanrural in c("")){ # "_urban2", "_rural2"
            
            dv_name <- ""
            if(dv %in% "globcover_urban_sum") dv_name <- "N Urban Pixels"
            if(dv %in% "globcover_urban_sum_ihs") dv_name <- "N Urban Pixels, IHS Transformation"
            
            if(dv %in% "dmspols_zhang_sum2")  dv_name <- "N Pixels NTL$>$2"
            if(dv %in% "dmspols_zhang_sum2_ihs")  dv_name <- "N Pixels NTL$>$2, IHS Transformation"
            
            if(dv %in% "dmspols_zhang_sum6")  dv_name <- "N Pixels NTL$>$6"
            if(dv %in% "dmspols_zhang_sum6_ihs")  dv_name <- "N Pixels NTL$>$6, IHS Transformation"
            
            if(dv %in% "dmspols_zhang_ihs")   dv_name <- "Average NTL, IHS Transformation"
            if(dv %in% "dmspols_zhang_sum_ihs")   dv_name <- "Sum NTL, IHS Transformation"
            
            ## Load Data
            if(grepl("globcover", dv)){
              data <- readRDS(file.path(panel_rsdp_imp_data_file_path, unit, "merged_datasets",
                                        "longdiff_data_clean_base1996_end2016.Rds"))
            } else{
              data <- readRDS(file.path(panel_rsdp_imp_data_file_path, unit, "merged_datasets",
                                        "longdiff_data_clean_base1996_end2012.Rds"))
            }
            
            ## DV and othr vars
            data$DV <- data[[dv]]
            data$distance_city_addisababa <- data$distance_city_addisababa / 1000 / 100
            
            ## MA Variable
            data$MA_var      <- data[[paste0("MA_pop2000_tt_theta",theta, MA_ubanrural, log)]]
            data$MA_var_1996 <- data[[paste0("MA_pop2000_tt_theta",theta, MA_ubanrural, log, "_1996")]]
            
            data$MA_var_exc      <- data[[paste0("MA_pop2000_tt_theta",theta, exclude, MA_ubanrural, log)]]
            data$MA_var_exc_1996 <- data[[paste0("MA_pop2000_tt_theta",theta, exclude, MA_ubanrural, log, "_1996")]]
            
            ## Interactions
            data$MA_varXdistance_city_addisababa <- data$MA_var * data$distance_city_addisababa
            data$MA_varXdmspols_ihs_1996         <- data$MA_var * data$dmspols_ihs_1996
            data$MA_varXglobcover_urban_1996     <- data$MA_var * data$globcover_urban_1996
            data$MA_varXglobcover_cropland_1996  <- data$MA_var * data$globcover_cropland_1996
            data$MA_varXdmspols_2bin_1996        <- data$MA_var * data$dmspols_2bin_1996
            data$MA_varXdmspols_6bin_1996        <- data$MA_var * data$dmspols_6bin_1996
            
            data$MA_var_excXdistance_city_addisababa <- data$MA_var_exc * data$distance_city_addisababa
            data$MA_var_excXdmspols_ihs_1996         <- data$MA_var_exc * data$dmspols_ihs_1996
            data$MA_var_excXglobcover_urban_1996     <- data$MA_var_exc * data$globcover_urban_1996
            data$MA_var_excXglobcover_cropland_1996  <- data$MA_var_exc * data$globcover_cropland_1996
            data$MA_var_excXdmspols_2bin_1996        <- data$MA_var_exc * data$dmspols_2bin_1996
            data$MA_var_excXdmspols_6bin_1996        <- data$MA_var_exc * data$dmspols_6bin_1996
            
            ## OLS
            ols1   <- felm(DV ~ MA_var_exc                                       + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data) %>% adj_ols_var_names
            ols2   <- felm(DV ~ MA_var_exc + MA_var_excXdmspols_ihs_1996         + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data) %>% adj_ols_var_names
            ols3   <- felm(DV ~ MA_var_exc + MA_var_excXdmspols_2bin_1996        + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data) %>% adj_ols_var_names
            ols4   <- felm(DV ~ MA_var_exc + MA_var_excXdmspols_6bin_1996        + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data) %>% adj_ols_var_names
            ols5   <- felm(DV ~ MA_var_exc + MA_var_excXglobcover_urban_1996     + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data) %>% adj_ols_var_names
            ols6   <- felm(DV ~ MA_var_exc + MA_var_excXdistance_city_addisababa + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data) %>% adj_ols_var_names
            
            ## IV
            iv1  <- felm(DV ~ dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | Z_CODE | (MA_var ~ MA_var_exc) | Z_CODE, data = data) %>% adj_iv_var_names()
            iv2  <- felm(DV ~ dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | Z_CODE | (MA_var|MA_varXdmspols_ihs_1996         ~ MA_var_exc + MA_var_excXdmspols_ihs_1996) | Z_CODE, data = data) %>% adj_iv_var_names()
            iv3  <- felm(DV ~ dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | Z_CODE | (MA_var|MA_varXdmspols_2bin_1996  ~ MA_var_exc + MA_var_excXdmspols_2bin_1996) | Z_CODE, data = data) %>% adj_iv_var_names()
            iv4  <- felm(DV ~ dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | Z_CODE | (MA_var|MA_varXdmspols_6bin_1996  ~ MA_var_exc + MA_var_excXdmspols_6bin_1996) | Z_CODE, data = data) %>% adj_iv_var_names()
            iv5  <- felm(DV ~ dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | Z_CODE | (MA_var|MA_varXglobcover_urban_1996     ~ MA_var_exc + MA_var_excXglobcover_urban_1996) | Z_CODE, data = data) %>% adj_iv_var_names()
            iv6  <- felm(DV ~ dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | Z_CODE | (MA_var|MA_varXdistance_city_addisababa ~ MA_var_exc + MA_var_excXdistance_city_addisababa) | Z_CODE, data = data) %>% adj_iv_var_names()
            
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
                      dep.var.labels   = c(dv_name),
                      omit = c("Z_CODE", "Constant"),
                      #keep=c("MA_var", "MA_var_1996", "dmspols_ihs_1996", "dmspols_ihs_pretnd96_92", "globcover_urban_sum_pretnd96_92.y"),
                      covariate.labels = c("MA",
                                           "MA$\\times$Log mean light, 1996",
                                           "MA$\\times$Prop. NTL$>$2, 1996",
                                           "MA$\\times$Prop. NTL$>$6, 1996",
                                           "MA$\\times$Prop. Urban, 1996",
                                           "MA$\\times$Dist Addis (100km)",
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
                      add.lines = list(
                        #c("Zone FEs", rep("Y", 10)),
                        c("MA IV, 50km Doughnut", rep("N", 5), rep("Y", 5))
                      ),
                      out=file.path(paper_tables,
                                    paste0("MA",MA_ubanrural,"_table_longdiff_theta",theta,log,"_",unit,"_",dv,".tex")))
          }
        }
      }
    }
  }
}



