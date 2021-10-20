# Market Access Analysis

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "woreda", "merged_datasets", "panel_data_clean.Rds"))

data <- data %>%
  filter(year >= 1996)

# MA Transform -----------------------------------------------------------------
data <- data %>%
  mutate(MA_pop2000_theta1_log = log(MA_pop2000_theta1),
         MA_pop2000_theta2_log = log(MA_pop2000_theta2),
         MA_pop2000_theta5_log = log(MA_pop2000_theta5),
         MA_pop2000_theta8_log = log(MA_pop2000_theta8),
         MA_pop2000_theta1_exclude100km_log = log(MA_pop2000_theta1_exclude100km),
         MA_pop2000_theta2_exclude100km_log = log(MA_pop2000_theta2_exclude100km),
         MA_pop2000_theta5_exclude100km_log = log(MA_pop2000_theta5_exclude100km),
         MA_pop2000_theta8_exclude100km_log = log(MA_pop2000_theta8_exclude100km))

data$globcover_cropland_sum_log <- log(data$globcover_cropland_sum + 1)

data$Z_CODE %>% unique() %>% length()

# Regressions ------------------------------------------------------------------
for(theta in c(1,2,5,8)){
  for(log in c("_log")){
    for(exclude100 in c("", "_exclude100km")){
      
      data$MA_var <- data[[paste0("MA_pop2000_theta",theta,exclude100, log)]]
      
      lm_globcover_urban    <- felm(globcover_cropland  ~ MA_var + temp_avg + precipitation | year + woreda_id | 0 | Z_CODE, data = data)
      lm_dmspols_zhang_2    <- felm(dmspols_zhang_2     ~ MA_var + temp_avg + precipitation | year + woreda_id | 0 | Z_CODE, data = data)
      lm_dmspols_zhang_6    <- felm(dmspols_zhang_6     ~ MA_var + temp_avg + precipitation | year + woreda_id | 0 | Z_CODE, data = data)
      lm_dmspols_zhang_ihs  <- felm(dmspols_zhang_ihs   ~ MA_var + temp_avg + precipitation | year + woreda_id | 0 | Z_CODE, data = data)
      lm_globcover_cropland <- felm(globcover_cropland  ~ MA_var + temp_avg + precipitation | year + woreda_id | 0 | Z_CODE, data = data)
      lm_ndvi               <- felm(ndvi                ~ MA_var + temp_avg + precipitation | year + woreda_id | 0 | Z_CODE, data = data)
      lm_ndvi_cropland      <- felm(ndvi_cropland       ~ MA_var + temp_avg + precipitation | year + woreda_id | 0 | Z_CODE, data = data)
      
      stargazer(lm_globcover_urban,
                lm_dmspols_zhang_2,
                lm_dmspols_zhang_6,
                lm_dmspols_zhang_ihs,
                #lm_ndvi,
                #lm_ndvi_cropland,
                dep.var.labels.include = T,
                #dep.var.labels = c("China Influential ","China Positive", "China Most","US Most","China Best","US Best"),
                dep.var.labels   = c("Urban", "NTL$>$2", "NTL$>$6", "IHS(NTL)"),
                keep=c("MA_var"),
                covariate.labels = c("log(MA)"),
                dep.var.caption = "",
                omit.stat = c("f","ser"), 
                align=TRUE,
                no.space=TRUE,
                float=FALSE,
                column.sep.width = "8pt",
                #report="vcs*",
                digits = 2,
                add.lines = list(
                  c("Year   FE", "Y", "Y", "Y","Y", "Y", "Y"),
                  c("Woreda FE", "Y", "Y", "Y","Y", "Y", "Y")
                ),
                out=file.path(paper_tables,
                              paste0("MA_table_theta",theta,log,exclude100,".tex")))
    }
  }
}


