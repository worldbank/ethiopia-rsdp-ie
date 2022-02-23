# Make Ethiopia Spatial File

# Load data --------------------------------------------------------------------
world_sp <- readOGR(file.path(wb_boundaries_dir, "RawData", 
                              "ne_50m_WB2019_admin_0_countries.shp"))

# Clean data -------------------------------------------------------------------
eth_sp <- world_sp[world_sp$NAME %in% "Ethiopia",]

# Export -----------------------------------------------------------------------
saveRDS(eth_sp, file.path(wb_boundaries_dir, "FinalData", "ethiopia.Rds"))
