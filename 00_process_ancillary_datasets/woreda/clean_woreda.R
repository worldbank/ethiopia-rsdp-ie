# Clean Woreda Data

# Add unique ID, extract nighttime lights, and divide nighttime lights into
# different groupings

# Load data --------------------------------------------------------------------
woreda <- readOGR(file.path(woreda_dir, "RawData", "Ethioworeda.shp"))
crs_original <- crs(woreda)

# Prep Data --------------------------------------------------------------------
#### WGS84 Version
woreda_wgs84 <- woreda %>% spTransform(CRS("+init=epsg:4326"))

#### Add unique ID
woreda$woreda_id <- 1:nrow(woreda)

#### Add Baseline NTL
## Load & Prep NTL
dmsp96 <- raster(file.path(ntl_harmon_dir, "RawData", "Harmonized_DN_NTL_1996_calDMSP.tif"))
dmsp96 <- crop(dmsp96, woreda_wgs84)
dmsp96 <- velox(dmsp96)

## Extract Lights
woreda$woreda_dmspols96_max <- dmsp96$extract(sp=woreda_wgs84, 
                                              small = T,
                                              fun = max) %>% as.vector()

#### Woreda NTL Groupings
ntl_values_pos <- woreda$woreda_dmspols96_max[woreda$woreda_dmspols96_max > 0]

## 2 bins (above/below median)
ntl_values_pos_med <- median(ntl_values_pos)

woreda$wor_ntlgroup_2bin <- NA
woreda$wor_ntlgroup_2bin[woreda$woreda_dmspols96_max >= ntl_values_pos_med] <- 2
woreda$wor_ntlgroup_2bin[woreda$woreda_dmspols96_max < ntl_values_pos_med] <- 1

## 4 bins
ntl_values_pos_q2 <- quantile(ntl_values_pos, c(1/3, 2/3))

woreda$wor_ntlgroup_4bin <- NA
woreda$wor_ntlgroup_4bin[woreda$woreda_dmspols96_max < ntl_values_pos_q2[1]] <- 2
woreda$wor_ntlgroup_4bin[woreda$woreda_dmspols96_max >= ntl_values_pos_q2[1]] <- 3
woreda$wor_ntlgroup_4bin[woreda$woreda_dmspols96_max > ntl_values_pos_q2[2]] <- 4
woreda$wor_ntlgroup_4bin[woreda$woreda_dmspols96_max %in% 0:1] <- 1

# Export -----------------------------------------------------------------------
saveRDS(woreda, file.path(woreda_dir, "FinalData", "woreda.Rds"))


