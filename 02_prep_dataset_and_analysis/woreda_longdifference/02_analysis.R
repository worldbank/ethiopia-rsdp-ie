# Analysis

# Load Data --------------------------------------------------------------------
data2012 <- readRDS(file.path(panel_rsdp_imp_data_file_path, "woreda", "merged_datasets",
                          "longdiff_data_clean_base1996_end2012.Rds"))

data2016 <- readRDS(file.path(panel_rsdp_imp_data_file_path, "woreda", "merged_datasets",
                          "longdiff_data_clean_base1996_end2016.Rds"))

data2012$dmspols_1996_bin <- as.numeric(data2012$dmspols_1996 > 0)
data2016$dmspols_1996_bin <- as.numeric(data2016$dmspols_1996 > 0)

# Regressions ------------------------------------------------------------------
for(theta in c(1,2,5,8)){
  for(log in c("_log")){
    for(exclude100 in c("", "_exclude100km")){
      
      data2012$MA_var <- data2012[[paste0("MA_pop2000_tt_theta",theta,exclude100, log)]]
      data2016$MA_var <- data2016[[paste0("MA_pop2000_tt_theta",theta,exclude100, log)]]
      
      lm_globcover_urban    <- felm(globcover_cropland  ~ MA_var | 0 | 0 | Z_CODE, data = data2016)
      lm_dmspols_zhang_2    <- felm(dmspols_zhang_2     ~ MA_var | 0 | 0 | Z_CODE, data = data2012)
      lm_dmspols_zhang_6    <- felm(dmspols_zhang_6     ~ MA_var | 0 | 0 | Z_CODE, data = data2012)
      lm_dmspols_zhang_ihs  <- felm(dmspols_zhang_ihs   ~ MA_var | 0 | 0 | Z_CODE, data = data2012)
      lm_globcover_cropland <- felm(globcover_cropland  ~ MA_var | 0 | 0 | Z_CODE, data = data2016)
      lm_ndvi               <- felm(ndvi                ~ MA_var | 0 | 0 | Z_CODE, data = data2016)
      lm_ndvi_cropland      <- felm(ndvi_cropland       ~ MA_var | 0 | 0 | Z_CODE, data = data2016)
      
      lm_globcover_urban_NTLBASE    <- felm(globcover_cropland  ~ MA_var*dmspols_1996_bin | 0 | 0 | Z_CODE, data = data2016)
      lm_dmspols_zhang_2_NTLBASE    <- felm(dmspols_zhang_2     ~ MA_var*dmspols_1996_bin | 0 | 0 | Z_CODE, data = data2012)
      lm_dmspols_zhang_6_NTLBASE    <- felm(dmspols_zhang_6     ~ MA_var*dmspols_1996_bin | 0 | 0 | Z_CODE, data = data2012)
      lm_dmspols_zhang_ihs_NTLBASE  <- felm(dmspols_zhang_ihs   ~ MA_var*dmspols_1996_bin | 0 | 0 | Z_CODE, data = data2012)
      lm_globcover_cropland_NTLBASE <- felm(globcover_cropland  ~ MA_var*dmspols_1996_bin | 0 | 0 | Z_CODE, data = data2016)
      lm_ndvi_NTLBASE               <- felm(ndvi                ~ MA_var*dmspols_1996_bin | 0 | 0 | Z_CODE, data = data2016)
      lm_ndvi_cropland_NTLBASE      <- felm(ndvi_cropland       ~ MA_var*dmspols_1996_bin | 0 | 0 | Z_CODE, data = data2016)
      
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
                #add.lines = list(
                #  c("Year   FE", "Y", "Y", "Y","Y", "Y", "Y"),
                #  c("Woreda FE", "Y", "Y", "Y","Y", "Y", "Y")
                #),
                out=file.path(paper_tables,
                              paste0("MA_table_longdiff_theta",theta,log,exclude100,".tex")))
      
      stargazer(lm_globcover_urban_NTLBASE,
                lm_dmspols_zhang_2_NTLBASE,
                lm_dmspols_zhang_6_NTLBASE,
                lm_dmspols_zhang_ihs_NTLBASE,
                #lm_ndvi_NTLBASE,
                #lm_ndvi_cropland_NTLBASE,
                dep.var.labels.include = T,
                #dep.var.labels = c("China Influential ","China Positive", "China Most","US Most","China Best","US Best"),
                dep.var.labels   = c("Urban", "NTL$>$2", "NTL$>$6", "IHS(NTL)"),
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
                out=file.path(paper_tables,
                              paste0("MA_table_longdiff_theta",theta,log,exclude100,"_ntlbase.tex")))
    }
  }
}






