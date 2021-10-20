# Create Points at DMSPOLS Level

# Load Data --------------------------------------------------------------------
dmspols <- raster(file.path(ntl_harmon_dir, "RawData", "Harmonized_DN_NTL_1992_calDMSP.tif"))
eth_adm0 <- readRDS(file.path(gadm_dir, "RawData", "gadm36_ETH_0_sp.rds"))

## Crop - NTL originally at global level
dmspols <- crop(dmspols, eth_adm0)

# Create Sample Area -----------------------------------------------------------
# Area to keep grid cells

sample_area <- eth_adm0
sample_area$id <- 1

# Create Points ----------------------------------------------------------------
## Create spatial points object
dmspols_points <- dmspols %>%
  coordinates() %>%
  as.data.frame() %>%
  dplyr::rename(long = x,
                lat = y)
coordinates(dmspols_points) <- ~long+lat
crs(dmspols_points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
dmspols_points$temp_id <- 1:length(dmspols_points)

## Create spatial polygon object
dmspols_poly <- polygonize(dmspols, na.rm=F)

## Determine which cells to keep
sample_area@data <- sample_area@data %>%
  dplyr::select(id)
points_OVER_samplearea <- over_chunks(dmspols_points, sample_area, "sum", chunk_size=5000)
  
cells_to_keep <- !is.na(points_OVER_samplearea$id)

# Subset and prep objects ------------------------------------------------------

## Subset
dmspols_points <- dmspols_points[cells_to_keep,]
dmspols_poly   <- dmspols_poly[cells_to_keep,]

# Polygon sf to sp
dmspols_poly <- dmspols_poly %>% as("Spatial")

## Add Cell ID
dmspols_poly$cell_id <- 1:nrow(dmspols_poly)
dmspols_poly@data <- dmspols_poly@data %>%
  dplyr::select(cell_id)

dmspols_points$cell_id <- 1:nrow(dmspols_points)
dmspols_points@data <- dmspols_points@data %>%
  dplyr::select(cell_id)

# Export -----------------------------------------------------------------------
saveRDS(dmspols_points, file.path(panel_rsdp_imp_dir, "dmspols_grid_ethiopia", "individual_datasets","points.Rds"))
saveRDS(dmspols_poly,   file.path(panel_rsdp_imp_dir, "dmspols_grid_ethiopia", "individual_datasets","polygons.Rds"))

