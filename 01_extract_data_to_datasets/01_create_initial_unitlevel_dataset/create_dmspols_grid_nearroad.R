# Create Points at DMSPOLS Level

# Load Data --------------------------------------------------------------------
dmspols <- raster(file.path(ntl_harmon_dir, "RawData", "Harmonized_DN_NTL_1992_calDMSP.tif"))
roads <- readRDS(file.path(rsdp_dir, "RawData", "RoadNetworkPanelData_1996_2016.Rds"))
eth_adm0 <- readRDS(file.path(gadm_dir, "RawData", "gadm36_ETH_0_sp.rds"))

# Create Sample Area -----------------------------------------------------------
# Create sample area to restrict DMSP points. Sample area is:
# (1) Locations within 10km from a road and (2) within Ethiopia

## Road buffer
# Simplify to speed up then buffer by chunks
roads_s <- gSimplify(roads, tol=.001)
roads_s$id <- 1:length(roads_s)
roads_buffer <- gBuffer_chunks(roads_s, width = 10/111.12, chunk_size = 100)

# Make one row
roads_buffer$id <- 1
roads_buffer <- raster::aggregate(roads_buffer, by = "id")

## Road buffer and ethiopia overlap
sample_area <- raster::intersect(roads_buffer, eth_adm0)
sample_area$id <- 1

# Create Points ----------------------------------------------------------------
## Raster originally for whole world; crop to Ethiopia
dmspols <- dmspols %>% crop(eth_adm0)

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
saveRDS(dmspols_points, file.path(panel_rsdp_imp_dir, "dmspols_grid_nearroad", "individual_datasets","points.Rds"))
saveRDS(dmspols_poly,   file.path(panel_rsdp_imp_dir, "dmspols_grid_nearroad", "individual_datasets","polygons.Rds"))

