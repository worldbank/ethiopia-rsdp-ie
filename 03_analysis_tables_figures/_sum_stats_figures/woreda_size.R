# Woreda Summary Trends

# Woreda Data ------------------------------------------------------------------
woreda_blank <- readRDS(file.path(panel_rsdp_imp_data_file_path, "woreda", "individual_datasets", "polygons_no_road_cut.Rds"))

woreda_blank_sf <- woreda_blank %>%
  st_as_sf()

woreda_blank_sf$area_m <- woreda_blank_sf %>% st_area
woreda_blank_sf$area_km <- woreda_blank_sf$area_m /(1000^2) # meters to kilometers

woreda_blank_sf$area_km %>% summary()

## Number of Zones
data_panel <- readRDS(file.path(panel_rsdp_imp_data_file_path, "woreda", "merged_datasets", "panel_data_clean.Rds"))
data_panel$cell_id %>% unique() %>% length()
data_panel$Z_CODE %>% unique() %>% length()
