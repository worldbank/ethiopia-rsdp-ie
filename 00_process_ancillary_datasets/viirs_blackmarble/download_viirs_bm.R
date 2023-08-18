# Download VIIRS BlackMarble

# Setup ------------------------------------------------------------------------
# Load bearer token
bearer <- file.path("~/Desktop", "bearer_bm.csv") %>%
  read_csv() %>%
  pull("token")

# Load Ethiopia boundary
roi_sp <- readRDS(file.path(gadm_dir, "RawData", "gadm36_ETH_0_sp.rds")) 
roi_buff_sp <- roi_sp %>% gBuffer(width = 50/111.12, byid = T)
roi_sf <- roi_buff_sp %>% st_as_sf()

# Download data ----------------------------------------------------------------
bm_raster(roi_sf = roi_sf,
          product_id = "VNP46A4",
          date =2012:2022,
          bearer = bearer,
          quality_flag_rm = c(255,1),
          output_location_type = "file",
          file_dir = file.path(ntl_bm_dir, "RawData"))
