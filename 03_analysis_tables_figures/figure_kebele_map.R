# Kebele Map and Trends

# Load Data ------------------------------------------------------------------
kebele <- readRDS(file.path(panel_rsdp_imp_dir, 
                            "kebele", "individual_datasets", 
                            "polygons.Rds")) %>%
  st_as_sf()
eth_adm <- readRDS(file.path(wb_boundaries_dir, "FinalData", "ethiopia.Rds")) %>%
  st_as_sf()

# Intersect so map fits within WB approved boundary
kebele <- st_intersection(kebele, eth_adm)

# Map --------------------------------------------------------------------------
#kebele_s <- kebele %>% gSimplify(tol = 0.005, topologyPreserve = T)

p <- ggplot() +
  #geom_polygon(data = eth_adm,
  #             aes(x = long, y = lat, group = group)) +
  geom_sf(data = kebele,
               fill = NA,
               color = "black",
               size = 0.2) +
  theme_void() +
  coord_sf()

ggsave(p, filename = file.path(paper_figures, "kebele_map.png"),
       height = 17, width = 20)


