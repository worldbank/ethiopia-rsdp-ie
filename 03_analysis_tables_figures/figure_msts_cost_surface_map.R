# Map of Cost Surface

# Load Data --------------------------------------------------------------------
## Elevation/Slope
elevation <- raster(file.path(elev_dir, "RawData", "eth_elevation_1000m.tif"))
slope <- terrain(elevation, opt="slope", unit="degrees",neighbors=8)

## Ethiopia ADM boundary
# The "woreda" file has holes for water bodies
eth <- readRDS(file.path(wb_boundaries_dir, "FinalData", "ethiopia.Rds"))

#eth <- readRDS(file.path(gadm_dir, "RawData", "gadm36_ETH_0_sp.rds")) %>%
#  gBuffer(width = 10/111.12)

## Globcover
land_cover <- raster(file.path(gc_dir, "RawData", "1992_2015_data", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), 1) %>%
  crop(eth)

land_cover_urban   <- calc(land_cover, function(x) x %in% 190)
land_cover_wetland <- calc(land_cover, function(x) x %in% 180)
land_cover_water   <- calc(land_cover, function(x) x %in% 210)

land_cover_urban   <- resample(land_cover_urban, slope)
land_cover_wetland <- resample(land_cover_wetland, slope)
land_cover_water   <- resample(land_cover_water, slope)

# To 0/1
land_cover_urban   <- calc(land_cover_urban, function(x) as.numeric(x > 0))
land_cover_wetland <- calc(land_cover_wetland, function(x) as.numeric(x > 0))
land_cover_water   <- calc(land_cover_water, function(x) as.numeric(x > 0))

# Cost Surface -----------------------------------------------------------------
cost_r <- 1 + slope + 25*land_cover_urban + 25*land_cover_wetland + 25*land_cover_water
cost_r <- cost_r %>% mask(eth)

# Figure -----------------------------------------------------------------------
cost_r_spdf <- as(cost_r, "SpatialPixelsDataFrame")
cost_r_df <- as.data.frame(cost_r_spdf)
colnames(cost_r_df) <- c("value", "x", "y")

# https://ggplot2-book.org/scale-colour.html
p <- ggplot() +  
  geom_tile(data=cost_r_df, aes(x=x, y=y, fill=log(value+1)), alpha=1) + 
  scale_fill_viridis_c() +
  coord_equal() +
  theme_void() +
  theme(legend.position="none") 

ggsave(p, 
       filename = file.path(paper_figures, "cost_surface.png"),
       height = 2.5, width = 3.5)

