# Map of Clusters

# Load / Prep Data -------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_globcover_urban", "merged_datasets", "panel_data_clean.Rds"))
clusters <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_globcover_urban", "individual_datasets", "points.Rds"))
eth <- readRDS(file.path(data_file_path, "GADM", "RawData", "gadm36_ETH_0_sp.rds")) 

data <- data %>%
  dplyr::select(cell_id, first_year_urban) %>%
  distinct()

## Merge data with clusters
clusters <- merge(clusters, data, by = "cell_id")

## Subset to clusters in sample
clusters <- clusters[clusters$cell_id %in% data$cell_id,]

## Buffer 
clusters <- gBuffer(clusters, width = 2/111.12, byid=T)

# Map --------------------------------------------------------------------------
clusters$id <- row.names(clusters)
clusters_tidy <- tidy(clusters)
clusters_tidy <- merge(clusters_tidy,clusters@data, by = "id")

ggplot() +
  geom_polygon(data = eth,
               aes(x = long, y = lat, group = group),
               fill = "black") +
  geom_polygon(data = clusters_tidy,
               aes(x = long, y = lat, group = group, fill = first_year_urban)) +
  labs(fill = "Year Urban\nArea Appears") +
  scale_fill_distiller(palette = "Spectral") +
  coord_quickmap() +
  theme_void() +
  ggsave(filename = file.path(panel_rsdp_imp_data_file_path, "clusters_of_globcover_urban",
                              "outputs", "figures", "clusters_map.png"),
         height = 3.5, width = 5)





