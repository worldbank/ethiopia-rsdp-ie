# Trends in DMSP-OLS Over Time for Whole Country

# Load Data --------------------------------------------------------------------
dmsp_df <- map_df(1992:2018, function(year){
  print(year)
  
  if(year <= 2013){
    dmsp <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", paste0("Harmonized_DN_NTL_",year,"_calDMSP.tif"))) %>% crop(eth_adm) %>% mask(eth_adm)
  } else{
    dmsp <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", paste0("Harmonized_DN_NTL_",year,"_simVIIRS.tif"))) %>% crop(eth_adm) %>% mask(eth_adm)
  }
  
  dmsp <- dmsp %>% crop(eth_adm) %>% mask(eth_adm)
  
  dmsp_mean   <- dmsp[] %>% mean(na.rm=T)
  dmsp_median <- dmsp[] %>% median(na.rm=T)
  dmsp_sum <- dmsp[] %>% sum(na.rm=T)
  
  out_df <- data.frame(year = year,
                       dmsp_mean = dmsp_mean,
                       dmsp_median = dmsp_median,
                       dmsp_sum = dmsp_sum)
  
  return(out_df)
  
})

# Figure -----------------------------------------------------------------------
ggplot() +
  geom_line(data = dmsp_df,
            aes(x = year,
                y = dmsp_sum))


ggsave(p, filename = file.path(paper_figures,
                               "dmspols_ethiopia_annual_trends.png"),
       height = 6,
       width = 7)

