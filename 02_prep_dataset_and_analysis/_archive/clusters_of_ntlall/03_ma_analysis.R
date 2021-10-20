# Market Access Analysis

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "merged_datasets", "panel_data_clean.Rds"))

data <- data %>%
  filter(year == 1996)

data$cluster_n_cells %>% hist()

data <- data %>%
  filter(year %in% c(1996, 2016)) %>%
  



data$globcover_cropland_sum_log <- log(data$globcover_cropland_sum + 1)

#data$Z_CODE %>% unique() %>% length()

# Regressions ------------------------------------------------------------------
for(theta in c(1,2,5,8)){
  for(log in c("_log")){
    for(exclude100 in c("", "_exclude100km")){
      
      data$MA_var <- data[[paste0("MA_pop2000_theta",theta,exclude100, log)]]
      
      lm_globcover_urban    <- felm(globcover_cropland  ~ MA_var + temp_avg + precipitation | year + cell_id | 0 | 0, data = data)
      lm_dmspols_zhang_2    <- felm(dmspols_zhang_2     ~ MA_var + temp_avg + precipitation | year + cell_id | 0 | 0, data = data)
      lm_dmspols_zhang_6    <- felm(dmspols_zhang_6     ~ MA_var + temp_avg + precipitation | year + cell_id | 0 | 0, data = data)
      lm_dmspols_zhang_ihs  <- felm(dmspols_zhang_ihs   ~ MA_var + temp_avg + precipitation | year + cell_id | 0 | 0, data = data)
      lm_globcover_cropland <- felm(globcover_cropland  ~ MA_var + temp_avg + precipitation | year + cell_id | 0 | 0, data = data)
      lm_ndvi               <- felm(ndvi                ~ MA_var + temp_avg + precipitation | year + cell_id | 0 | 0, data = data)
      lm_ndvi_cropland      <- felm(ndvi_cropland       ~ MA_var + temp_avg + precipitation | year + cell_id | 0 | 0, data = data)
      
      stargazer(lm_globcover_urban,
                lm_dmspols_zhang_2,
                lm_dmspols_zhang_6,
                lm_dmspols_zhang_ihs,
                lm_ndvi,
                lm_ndvi_cropland,
                dep.var.labels.include = T,
                #dep.var.labels = c("China Influential ","China Positive", "China Most","US Most","China Best","US Best"),
                dep.var.labels   = c("Urban", "NTL$>$2", "NTL$>$6", "IHS(NTL)",  "NDVI", "NDVI - Crop"),
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
                  c("Cluster FE", "Y", "Y", "Y","Y", "Y", "Y")
                ),
                out=file.path(panel_rsdp_imp_data_file_path, 
                              "clusters_of_ntl", 
                              "outputs", 
                              "tables",
                              paste0("MA_table_theta",theta,log,exclude100,".tex")))
    }
  }
}


