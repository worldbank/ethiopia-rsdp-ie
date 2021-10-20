# Kebele Map and Trends

# Load Data ------------------------------------------------------------------
kebele <- readRDS(file.path(panel_rsdp_imp_data_file_path, 
                            "kebele", "individual_datasets", 
                            "polygons.Rds"))

# Stats ------------------------------------------------------------------------
#### Area
kebele_sf <- kebele %>% st_as_sf()
kebele_sf$area_m <- kebele_sf %>% st_area()
kebele_sf$area_km <- kebele_sf$area_m / (1000^2)
kebele_sf$area_km <- kebele_sf$area_km %>% as.numeric()

summary(kebele_sf$area_km)

# Map --------------------------------------------------------------------------
kebele_s <- kebele %>% gSimplify(tol = 0.005, topologyPreserve = T)

p <- ggplot() +
  geom_polygon(data = kebele_s,
               aes(x = long, y = lat, group = group),
               fill = NA,
               color = "black",
               size = 0.2) +
  theme_void() +
  coord_quickmap()

ggsave(p, filename = file.path(paper_figures, "kebele_map.png"),
       height = 17, width = 20)


