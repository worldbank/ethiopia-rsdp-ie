# Determine Cut-Off for Conley Standard Errors

# https://mgimond.github.io/es214_support_tutorials/moranI_distance/MoranI_distance_band.html

nsim <- 100

# Load/prep data ---------------------------------------------------------------
keb_sf <- readRDS(file.path(panel_rsdp_imp_dir, "kebele", "individual_datasets", "points.Rds")) %>%
  st_as_sf()

ntl_df <- readRDS(file.path(panel_rsdp_imp_dir, "kebele", "individual_datasets", "dmspolsharmon.Rds"))
ntl_df <- ntl_df %>%
  dplyr::filter(year == 2016)

keb_sf <- keb_sf %>%
  left_join(ntl_df, by = "cell_id")

## Make centroid
keb_c_sf <- st_centroid(keb_sf)

# Morans i ---------------------------------------------------------------------
distances <- seq(0, 50000, 5000) 

for(i in 2:length(distances)){
  
  print(i)
  
  d1 = 0
  d1 <- distances[i-1]
  d2 <- distances[i]
  
  OUT_PATH <- file.path(panel_rsdp_imp_dir,
                        "kebele", 
                        "results_datasets",
                        "morans_i",
                        paste0("mi_", d1, "_", d2, "_", nsim, ".Rds"))
  
  if(!file.exists(OUT_PATH)){
    
    lw <-  keb_c_sf %>%
      dnearneigh(d1, d2) %>%
      nb2listw(style="W", zero.policy = TRUE)
    
    MI <- moran.mc(keb_c_sf$dmspols_harmon, lw, nsim=nsim, zero.policy = TRUE) 
    
    df_out <- data.frame(
      mi_statistic = MI$statistic %>% as.numeric(),
      mi_pvalue    = MI$p.value,
      distance_start_m = d1,
      distance_end_m = d2,
      nsim = nsim
    )
    
    saveRDS(df_out, OUT_PATH)
  }
  
}

mi_df <- file.path(panel_rsdp_imp_dir,
          "kebele", 
          "results_datasets",
          "morans_i") %>%
  list.files(full.names = T) %>%
  map_df(readRDS)
names(mi_df) <- c("stat", "pvalue", "d1", "d2", "nsim")

mi_df %>%
  ggplot() +
  geom_point(aes(x = d2,
                 y = pvalue))
