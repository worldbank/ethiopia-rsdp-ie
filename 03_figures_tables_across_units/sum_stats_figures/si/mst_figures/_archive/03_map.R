# Create Minimal Spanning Tree

# Load Data --------------------------------------------------------------------
mst_cost <- readRDS(file.path(data_file_path, "Hypothetical Road Networks", "least_cost_path_mst.Rds"))
mst_dist <- readRDS(file.path(data_file_path, "Hypothetical Road Networks", "least_euc_distance_path_mst.Rds"))
roads <- readRDS(file.path(data_file_path, "RSDP Roads", "FinalData", "RoadNetworkPanelData_1996_2016.Rds"))

eth <- readRDS(file.path(data_file_path, "GADM", "RawData", "gadm36_ETH_0_sp.rds")) 

improved_roads <- roads[roads$Speed2016 > roads$Speed1996,]

lc_mst <- ggplot() +
  geom_polygon(data = eth,
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = NA, alpha = 0.9) +
  geom_path(data = improved_roads,
            aes(x = long, y = lat, group = group,
                color = "Improved Roads"),
            size = 0.5) +
  geom_path(data = mst_cost,
            aes(x = long, y = lat, group = group,
                color = "MST"),
            size = 0.35) +
  scale_color_manual(values = c("gray40", "red")) +
  labs(color = NULL,
       title = "Least Cost MST") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size=11, face = "bold")) + 
  coord_quickmap()

ld_mst <- ggplot() +
  geom_polygon(data = eth,
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = NA, alpha = 0.9) + # cornsilk1
  geom_path(data = improved_roads,
            aes(x = long, y = lat, group = group,
                color = "Improved Roads"),
            size = 0.5) +
  geom_path(data = mst_dist,
            aes(x = long, y = lat, group = group,
                color = "MST"),
            size = 0.35) +
  scale_color_manual(values = c("gray40", "red")) +
  labs(color = NULL,
       title = "Minimum Distance MST") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size=11, face = "bold")) + 
  coord_quickmap()

fig <- ggarrange(ld_mst, lc_mst, common.legend = T, legend = "right")

ggsave(fig, filename = file.path(paper_figures, "mst_maps.png"),
       height = 2.5, width = 7)
