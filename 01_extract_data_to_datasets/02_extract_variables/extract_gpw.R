# Extract Population from GPW

# Load Data --------------------------------------------------------------------
sdf <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "points.Rds"))

gpw <- raster(file.path(gpw_dir, "RawData", "gpw-v4-population-density_2000.tif"))
gpw <- crop(gpw, sdf)

# Extract Population -----------------------------------------------------------
# Add woreda population from gpw. First, use geometry, then centroid. If population
# from geometry is NA, then use centroid

## Population using geometry
gpw <- crop(gpw, sdf)
sdf$pop_geom <- velox(gpw)$extract(sp = sdf, fun = function(x) sum(x, na.rm=T)) %>% as.vector()

## Population using centroid
# Use when geometry returns NA
sdf_pop_centroid <- gCentroid(sdf, byid=T)
sdf$pop_centroid <- raster::extract(gpw, sdf_pop_centroid)

## If population using geometry is NA, use from centroid
sdf$pop_geom[is.na(sdf$pop_geom)] <- sdf$pop_centroid[is.na(sdf$pop_geom)]

## Cleanup
sdf@data <- sdf@data %>%
  dplyr::rename(gpw2000 = pop_geom) %>%
  dplyr::select(cell_id, gpw2000)

# Export -----------------------------------------------------------------------
saveRDS(sdf@data, file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "gpw.Rds"))

