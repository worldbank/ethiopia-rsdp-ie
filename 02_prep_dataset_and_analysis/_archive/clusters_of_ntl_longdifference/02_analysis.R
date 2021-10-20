# Market Access Analysis

# Load Data --------------------------------------------------------------------
data2012 <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "merged_datasets",
                              "longdiff_data_clean_base1996_end2012.Rds"))
data2016 <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "merged_datasets",
                              "longdiff_data_clean_base1996_end2016.Rds"))

data2012$dmspols_1996_bin <- as.numeric(data2012$dmspols_1996 > 0)
data2016$dmspols_1996_bin <- as.numeric(data2016$dmspols_1996 > 0)

# Regressions ------------------------------------------------------------------
for(theta in c(1,2,5,8)){
  for(log in c("_log")){
    for(exclude100 in c("")){
      
      data2012$MA_var <- data2012[[paste0("MA_pop2000_theta",theta,exclude100, log)]]
      data2016$MA_var <- data2016[[paste0("MA_pop2000_theta",theta,exclude100, log)]]
      
      lm_dmspols_zhang_ihs             <- felm(dmspols_zhang_ihs             ~ MA_var | 0 | 0 | 0, data = data2012)
      lm_dmspols_zhang_sum2            <- felm(dmspols_zhang_sum2            ~ MA_var | 0 | 0 | 0, data = data2012)
      lm_dmspols_zhang_sum6            <- felm(dmspols_zhang_sum6            ~ MA_var | 0 | 0 | 0, data = data2012)
      lm_dmspols_zhang_sum0greater_bin <- felm(dmspols_zhang_sum0greater_bin ~ MA_var | 0 | 0 | 0, data = data2012)
      lm_globcover_urban_sum           <- felm(globcover_urban_sum        ~ MA_var | 0 | 0 | 0, data = data2016)
      lm_globcover_cropland_sum        <- felm(globcover_cropland_sum        ~ MA_var | 0 | 0 | 0, data = data2016)
      lm_ndvi                          <- felm(ndvi                          ~ MA_var | 0 | 0 | 0, data = data2016)
      lm_ndvi_cropland                 <- felm(ndvi_cropland                 ~ MA_var | 0 | 0 | 0, data = data2016)
      
      data2012$distance_city_addisababa_log <- data2012$distance_city_addisababa %>% log()
      data2016$distance_city_addisababa_log <- data2016$distance_city_addisababa %>% log()
      
      lm_dmspols_zhang_ihs_NTLBASE             <- felm(dmspols_zhang_ihs             ~ MA_var*distance_city_addisababa_log | 0 | 0 | 0, data = data2012)
      lm_dmspols_zhang_sum2_NTLBASE            <- felm(dmspols_zhang_sum2            ~ MA_var*distance_city_addisababa_log | 0 | 0 | 0, data = data2012)
      lm_dmspols_zhang_sum6_NTLBASE            <- felm(dmspols_zhang_sum6            ~ MA_var*distance_city_addisababa_log | 0 | 0 | 0, data = data2012)
      lm_dmspols_zhang_sum0greater_bin_NTLBASE <- felm(dmspols_zhang_sum0greater_bin ~ MA_var*distance_city_addisababa_log | 0 | 0 | 0, data = data2012)
      lm_globcover_urban_sum_NTLBASE           <- felm(globcover_urban_sum           ~ MA_var*distance_city_addisababa_log | 0 | 0 | 0, data = data2016)
      lm_globcover_cropland_sum_NTLBASE        <- felm(globcover_cropland_sum        ~ MA_var*distance_city_addisababa_log | 0 | 0 | 0, data = data2016)
      lm_ndvi_NTLBASE                          <- felm(ndvi                          ~ MA_var*distance_city_addisababa_log | 0 | 0 | 0, data = data2016)
      lm_ndvi_cropland_NTLBASE                 <- felm(ndvi_cropland                 ~ MA_var*distance_city_addisababa_log | 0 | 0 | 0, data = data2016)
      
      lm_dmspols_zhang_ihs_NTLBASE             <- felm(dmspols_zhang_ihs             ~ MA_var*dmspols_1996_bin | 0 | 0 | 0, data = data2012)
      lm_dmspols_zhang_sum2_NTLBASE            <- felm(dmspols_zhang_sum2            ~ MA_var*dmspols_1996_bin | 0 | 0 | 0, data = data2012)
      lm_dmspols_zhang_sum6_NTLBASE            <- felm(dmspols_zhang_sum6            ~ MA_var*dmspols_1996_bin | 0 | 0 | 0, data = data2012)
      lm_dmspols_zhang_sum0greater_bin_NTLBASE <- felm(dmspols_zhang_sum0greater_bin ~ MA_var*dmspols_1996_bin | 0 | 0 | 0, data = data2012)
      lm_globcover_urban_sum_NTLBASE           <- felm(globcover_urban_sum           ~ MA_var*dmspols_1996_bin | 0 | 0 | 0, data = data2016)
      lm_globcover_cropland_sum_NTLBASE        <- felm(globcover_cropland_sum        ~ MA_var*dmspols_1996_bin | 0 | 0 | 0, data = data2016)
      lm_ndvi_NTLBASE                          <- felm(ndvi                          ~ MA_var*dmspols_1996_bin | 0 | 0 | 0, data = data2016)
      lm_ndvi_cropland_NTLBASE                 <- felm(ndvi_cropland                 ~ MA_var*dmspols_1996_bin | 0 | 0 | 0, data = data2016)
      
      stargazer(lm_dmspols_zhang_ihs,
                lm_dmspols_zhang_sum2,
                lm_dmspols_zhang_sum6,
                lm_dmspols_zhang_sum0greater_bin,
                lm_globcover_urban_sum,
                lm_globcover_cropland_sum,
                lm_ndvi,
                lm_ndvi_cropland,
                dep.var.labels.include = T,
                #dep.var.labels = c("China Influential ","China Positive", "China Most","US Most","China Best","US Best"),
                dep.var.labels   = c("IHS(NTL)", "NTL$>$2", "NTL$>$6", "Clstr. Exists", "Urban", "Crop",  "NDVI", "NDVI - Crop"),
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
                #add.lines = list(
                #  c("Year   FE", "Y", "Y", "Y","Y", "Y", "Y"),
                #  c("Woreda FE", "Y", "Y", "Y","Y", "Y", "Y")
                #),
                out=file.path(panel_rsdp_imp_data_file_path, 
                              "clusters_of_ntl", 
                              "outputs", 
                              "tables",
                              paste0("MA_table_longdiff_theta",theta,log,exclude100,".tex")))
      
      stargazer(lm_dmspols_zhang_ihs_NTLBASE,
                lm_dmspols_zhang_sum2_NTLBASE,
                lm_dmspols_zhang_sum6_NTLBASE,
                lm_dmspols_zhang_sum0greater_bin_NTLBASE,
                lm_globcover_urban_sum_NTLBASE,
                lm_globcover_cropland_sum_NTLBASE,
                lm_ndvi_NTLBASE,
                lm_ndvi_cropland_NTLBASE,
                dep.var.labels.include = T,
                dep.var.labels   = c("IHS(NTL)", "NTL$>$2", "NTL$>$6", "Clstr. Exists", "Urban", "Crop",  "NDVI", "NDVI - Crop"),
                keep=c("MA_var", "dmspols_1996_bin", "MA_var:dmspols_1996_bin"),
                covariate.labels = c("log(MA)", "NTL$>0$ at Base.", "log(MA) $\\times$ NTL$>0$ at Base."),
                dep.var.caption = "",
                omit.stat = c("f","ser"), 
                align=TRUE,
                no.space=TRUE,
                float=FALSE,
                column.sep.width = "8pt",
                #report="vcs*",
                digits = 2,
                #add.lines = list(
                #  c("Year   FE", "Y", "Y", "Y","Y", "Y", "Y"),
                #  c("Woreda FE", "Y", "Y", "Y","Y", "Y", "Y")
                #),
                out=file.path(panel_rsdp_imp_data_file_path, 
                              "clusters_of_ntl", 
                              "outputs", 
                              "tables",
                              paste0("MA_table_longdiff_theta",theta,log,exclude100,"_ntlbase.tex")))
    }
  }
}




