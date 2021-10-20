# Extract GADM to Points

# Load Data --------------------------------------------------------------------
## Load Units
points <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "points.Rds"))

if(DATASET_TYPE %in% "kebele"){
  points_center <- points %>% gCentroid(byid=T)
  points_center$id_temp <- 1:length(points_center)
  points_center@data <- points@data
  points <- points_center
}

## Load Woreda
woreda <- readRDS(file.path(woreda_dir, "FinalData", "woreda.Rds"))
woreda <- spTransform(woreda, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Add Data ---------------------------------------------------------------------
#### Add woreda_id
points_OVER_woreda <- sp::over(points, woreda)
points$woreda_id <- points_OVER_woreda$woreda_id

# Some points don't exactly intersect with Woreda; for these, use closest
points_nooverlap <- points[is.na(points$woreda_id),]

if(nrow(points_nooverlap) > 0){
  
  points_nooverlap$woreda_id_closest <- lapply(1:nrow(points_nooverlap), function(i){
    print(paste(i, "/", nrow(points_nooverlap)))
    points_nooverlap_i <- points_nooverlap[i,]
    
    wordea_closest <- which.min(as.vector(gDistance(points_nooverlap_i, woreda, byid=T)))
    return(woreda$woreda_id[wordea_closest])
  }) %>%
    unlist()
  
  points_nooverlap$woreda_id <- NULL
  
  points <- merge(points, points_nooverlap@data, by = "cell_id",all.x=T)
  
  points$woreda_id[is.na(points$woreda_id)] <- points$woreda_id_closest[is.na(points$woreda_id)]
  points$woreda_id_closest <- NULL
}

#### Merge in data
woreda_df <- woreda@data %>%
  dplyr::select(woreda_id, W_CODE, Z_CODE, R_CODE, Pop2007, Density,
                woreda_dmspols96_max, wor_ntlgroup_2bin, wor_ntlgroup_4bin) %>%
  dplyr::rename(woreda_pop2007 = Pop2007,
                woreda_density2007 = Density)

points <- merge(points, woreda_df, by = "woreda_id")

# Export -----------------------------------------------------------------------
saveRDS(points@data, file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "adm_units.Rds"))

