# Map of Clusters

set.seed(42)

# Load Data --------------------------------------------------------------------
eth <- readRDS(file.path(data_file_path, "GADM", "RawData", "gadm36_ETH_0_sp.rds")) 

# NTL Clusters -----------------------------------------------------------------
data_ntl <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "merged_datasets", "panel_data_clean.Rds"))
clusters_ntl <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "individual_datasets", "points.Rds"))

data_ntl <- data_ntl %>%
  dplyr::select(cell_id, first_year_lit) %>%
  distinct()

## Merge data with clusters
clusters_ntl <- merge(clusters_ntl, data_ntl, by = "cell_id")

## Subset to clusters in sample
clusters_ntl <- clusters_ntl[clusters_ntl$cell_id %in% data_ntl$cell_id,]

## Buffer 
clusters_ntl <- gBuffer(clusters_ntl, width = 0.5/111.12, byid=T)

## Tidy
clusters_ntl$id <- row.names(clusters_ntl)
clusters_ntl_tidy <- tidy(clusters_ntl)
clusters_ntl_tidy <- merge(clusters_ntl_tidy, clusters_ntl@data, by = "id")

# GC-Urban Clusters ------------------------------------------------------------
data_urban <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_globcover_urban", "merged_datasets", "panel_data_clean.Rds"))
clusters_urban <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_globcover_urban", "individual_datasets", "points.Rds"))
eth <- readRDS(file.path(data_file_path, "GADM", "RawData", "gadm36_ETH_0_sp.rds")) 

data_urban <- data_urban %>%
  dplyr::select(cell_id, first_year_urban) %>%
  distinct()

## Merge data with clusters
clusters_urban <- merge(clusters_urban, data_urban, by = "cell_id")

## Subset to clusters in sample
clusters_urban <- clusters_urban[clusters_urban$cell_id %in% data_urban$cell_id,]

## Buffer 
clusters_urban <- gBuffer(clusters_urban, width = 2/111.12, byid=T)

clusters_urban$id <- row.names(clusters_urban)
clusters_urban_tidy <- tidy(clusters_urban)
clusters_urban_tidy <- merge(clusters_urban_tidy, clusters_urban@data, by = "id")

# Map --------------------------------------------------------------------------
p_ntl <- ggplot() +
  geom_polygon(data = eth,
               aes(x = long, y = lat, group = group),
               fill = "black") +
  geom_polygon(data = clusters_ntl_tidy,
               aes(x = long, y = lat, group = group, fill = first_year_lit)) +
  labs(title = "NTL Clusters",
       fill = "First Year\nCluster Appears") +
  scale_fill_distiller(palette = "Spectral", limits = c(1992, 2015)) +
  coord_quickmap() +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 

p_urban <- ggplot() +
  geom_polygon(data = eth,
               aes(x = long, y = lat, group = group),
               fill = "black") +
  geom_polygon(data = clusters_urban_tidy,
               aes(x = long, y = lat, group = group, fill = first_year_urban)) +
  labs(title = "Urban Clusters",
       fill = "First Year\nCluster Appears") +
  scale_fill_distiller(palette = "Spectral", limits = c(1992, 2015)) +
  coord_quickmap() +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

p_all <- ggarrange(p_ntl, p_urban,
                   nrow = 1,
                   common.legend = T,
                   legend = "right")

ggsave(p_all, filename = file.path(paper_figures, "clusters_map.png"),
       height = 3.5, width = 9)



 




