# Analysis of Cloud Cover

# Load data --------------------------------------------------------------------
# wor_sf <- read_sf(file.path(woreda_dir, "RawData", "Ethioworeda.shp"))
# 
# wor_sf$uid <- 1:nrow(wor_sf)

wor_sf <- readRDS(file.path(panel_rsdp_imp_dir, "kebele", "individual_datasets", "polygons_no_road_cut.Rds"))
wor_sf <- wor_sf %>% st_as_sf()

# Raster map for each year -----------------------------------------------------
cf_df <- map_df(1:22, function(i){
  print(i)
  r <- raster(file.path(data_dir, "DMSP_OLS", "RawData", "eth_cf_dmspols_allyears.tif"), i)
  
  r_df <- rasterToPoints(r, spatial = TRUE) %>% as.data.frame()
  names(r_df) <- c("value", "x", "y")
  r_df$year <- i + 1991
  
  return(r_df)
})

p <- ggplot() +
  geom_raster(data = cf_df,
              aes(x = x, y = y,
                  fill = value)) +
  scale_fill_distiller(palette = "Spectral") +
  labs(title = "Number of Cloud Free Observations") +
  coord_quickmap() +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "right",
        strip.text = element_text(face = "bold")) +
  labs(fill = "Number of\nCloud Free\nObservations") +
  facet_wrap(~year)

ggsave(p, 
       filename = file.path(paper_figures, "cloud_free_raster.png"),
       height = 5, width = 7)

# Extract data to Woredas ------------------------------------------------------
wor_df <- map_df(1:22, function(i){
  print(i)
  cf_r  <- raster(file.path(data_dir, "DMSP_OLS", "RawData", "eth_cf_dmspols_allyears.tif"), i)
  ntl_r <- raster(file.path(data_dir, "DMSP_OLS", "RawData", "eth_dmspols_allyears.tif"), i)
  
  wor_sf$cf_mean  <- exact_extract(cf_r, wor_sf, "mean")
  wor_sf$ntl_mean <- exact_extract(ntl_r, wor_sf, "mean")
  
  wor_df <- wor_sf %>%
    st_drop_geometry()

  return(wor_df)
})

wor_mean_df <- wor_df %>%
  group_by(cell_id) %>%
  dplyr::summarise(cf_mean = mean(cf_mean),
                   ntl_mean = mean(ntl_mean)) %>%
  ungroup() 

#### Map
wor_sf <- wor_sf %>%
  left_join(wor_mean_df, by = "cell_id")

wor_sf$cf_mean %>% summary()
p_map <- ggplot() +
  geom_sf(data = wor_sf,
          aes(fill = cf_mean),
          color = NA) +
  scale_fill_distiller(palette = "Spectral",
                       limits = c(30, 80),
                       breaks = seq(30, 80, by = 10)) +
  labs(title = "A. Spatial Distribution of Average Number\nof Cloud Free Observations",
       fill  = "N Observations") +
  theme_void() +
  theme(legend.position = "top")

#### Box
p_box <- wor_mean_df %>%
  ggplot() +
  geom_boxplot(aes(x = cf_mean)) +
  labs(title = "B. Distribution of Average Number of Cloud\nFree Observations",
       x = NULL) +
  scale_x_continuous(breaks = seq(30, 80, 10),
                     limits = c(30, 80)) +
  theme_classic2() 

#### Correlation
p_cor <- wor_mean_df %>%
  ggplot() +
  geom_point(aes(x = cf_mean,
                 y = log(ntl_mean+1))) +
  labs(x = "Average Number of Cloud Free Observations",
       y = "Log of Nighttime Lights Radiance",
       title = "C. Cloud Free Observations vs. Nighttime Lights") +
  scale_x_continuous(breaks = seq(30, 80, 10),
                     limits = c(30, 80)) +
  theme_classic2()

#### Arrange and Export
p_right <- ggarrange(p_box, p_cor,
          ncol = 1,
          heights = c(0.3, 0.7))

p <- ggarrange(p_map, p_right,
               widths = c(0.5, 0.5))

ggsave(p, 
       filename = file.path(paper_figures, "kebele_cloud_free.png"),
       height = 5, width = 10)



# wor_mean_df %>%
#   arrange(cf_mean) %>%
#   dplyr::mutate(n = 1:n(),
#                 prop = n/n()) %>%
#   
#   ggplot() +
#   geom_line(aes(x = cf_mean,
#                 y = prop)) +
#   labs(x = "Number of Cloud Free Observations",
#        y = "Proportion\nof\nKebeles") +
#   theme_classic2() +
#   theme(axis.title.y = element_text(angle = 0,
#                                     vjust = 0.5))
