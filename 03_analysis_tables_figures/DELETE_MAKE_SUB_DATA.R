
##
data <- readRDS(file.path(panel_rsdp_imp_dir, "kebele", "merged_datasets", "panel_data_clean.Rds"))

data_clean <- data %>%
  dplyr::select("cell_id", "year", "dmspols_harmon", "dmspols_harmon_log", "dmspols_harmon_ihs", 
                "globcover_urban_sum", "globcover_cropland_sum", "year_improvedroad", 
                "year_improvedroad_50aboveafter", "year_improvedroad_below50after", "wor_ntlgroup_2bin",
                "wor_ntlgroup_4bin", "far_addis", "latitude", "longitude", "W_CODE", "Z_CODE",
                "distance_rsdp123",
                "distance_rsdp123_targettedlocs",
                "distance_rsdp123_mst_lc_region",
                "MA_pop2000_tt_theta3_8",
                "MA_pop2000_tt_theta3_8_exclude50km")

names(data_clean) <- names(data_clean) %>%
  str_replace_all("2000", "00")

write_dta(data_clean, "~/Desktop/panel_data_clean.dta")


