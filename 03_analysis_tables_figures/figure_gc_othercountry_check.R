# Check Globcover with Neighbording Countries

# Load data --------------------------------------------------------------------
eth_sf <- gadm(country = "ETH", level=0, path = tempdir()) %>% st_as_sf()
ken_sf <- gadm(country = "KEN", level=0, path = tempdir()) %>% st_as_sf()
sdn_sf <- gadm(country = "SDN", level=0, path = tempdir()) %>% st_as_sf()
ssd_sf <- gadm(country = "SSD", level=0, path = tempdir()) %>% st_as_sf()
eri_sf <- gadm(country = "ERI", level=0, path = tempdir()) %>% st_as_sf()
dji_sf <- gadm(country = "DJI", level=0, path = tempdir()) %>% st_as_sf()
som_sf <- gadm(country = "SOM", level=0, path = tempdir()) %>% st_as_sf()

all_sf <- bind_rows(
  eth_sf,
  ken_sf,
  sdn_sf,
  ssd_sf,
  eri_sf,
  dji_sf,
  som_sf
)

all_df <- all_sf %>%
  st_drop_geometry()

year <- 2012

gc_df <- map_df(1992:2018, function(year){
  
  if(year <= 2015){
    r <- raster(file.path(gc_dir, "RawData", "1992_2015_data", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), year-1991 )
  } else{
    r <- raster(file.path(gc_dir, "RawData", "2016_2018_data", paste0("C3S-LC-L4-LCCS-Map-300m-P1Y-",year,"-v2.1.1.tif")))
  }
  
  r <- r %>% crop(all_sf)
  
  globcover_urban_fun <- function(r){
    r[] <- as.numeric(r[] %in% c(190))
    return(r)
  }
  
  globcover_cropland_fun <- function(r){
    r[] <- as.numeric(r[] %in% c(10,11,12,20,30))
    return(r)
  }
  
  r_urban    <- calc(r, fun=globcover_urban_fun)
  r_cropland <- calc(r, fun=globcover_cropland_fun)
  
  all_df$urban_mean <- exact_extract(r_urban, all_sf, 'mean')
  all_df$crop_mean <- exact_extract(r_cropland, all_sf, 'mean')
  all_df$year <- year
  
  return(all_df)
})


gc_df %>%
  ggplot() +
  geom_line(aes(x = year, y = urban_mean)) +
  facet_wrap(~COUNTRY,
             scales = "free_y") +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold")) +
  labs(x = NULL,
       y = "Proportion",
       title = "Proportion Urban")

ggsave(filename = file.path(paper_figures, "prop_urban_other_countries.png"),
       height = 3.5, width = 6)

gc_df %>%
  ggplot() +
  geom_line(aes(x = year, y = crop_mean)) +
  facet_wrap(~COUNTRY,
             scales = "free_y") +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold")) +
  labs(x = NULL,
       y = "Proportion",
       title = "Proportion Cropland")

ggsave(filename = file.path(paper_figures, "prop_crop_other_countries.png"),
       height = 3.5, width = 6)
