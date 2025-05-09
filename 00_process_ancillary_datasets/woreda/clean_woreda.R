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

# Add Baseline NTL: 1996 -------------------------------------------------------
## Load & Prep NTL
dmsp96 <- raster(file.path(ntl_harmon_dir, "RawData", "Harmonized_DN_NTL_1996_calDMSP.tif"))
dmsp96 <- crop(dmsp96, woreda_wgs84)
dmsp96 <- velox(dmsp96)

## Extract Lights
woreda$woreda_dmspols96_max <- dmsp96$extract(sp=woreda_wgs84, 
                                              small = T,
                                              fun = max) %>% as.vector()

woreda$woreda_dmspols96_mean <- dmsp96$extract(sp=woreda_wgs84, 
                                              small = T,
                                              fun = mean) %>% as.vector()

woreda$woreda_dmspols96_sum <- dmsp96$extract(sp=woreda_wgs84, 
                                               small = T,
                                               fun = sum) %>% as.vector()

#### Woreda NTL Groupings
ntl_values_pos <- woreda$woreda_dmspols96_max[woreda$woreda_dmspols96_max > 0]

## 4 bins
ntl_values_pos_q2 <- quantile(ntl_values_pos, c(1/3, 2/3))

woreda$wor_ntlgroup_4bin <- NA
woreda$wor_ntlgroup_4bin[woreda$woreda_dmspols96_max < ntl_values_pos_q2[1]] <- 2
woreda$wor_ntlgroup_4bin[woreda$woreda_dmspols96_max >= ntl_values_pos_q2[1]] <- 3
woreda$wor_ntlgroup_4bin[woreda$woreda_dmspols96_max > ntl_values_pos_q2[2]] <- 4
woreda$wor_ntlgroup_4bin[woreda$woreda_dmspols96_max %in% 0:1] <- 1

## 2 bins (dark vs lit)
woreda$wor_ntlgroup_2bin <- as.numeric(woreda$wor_ntlgroup_4bin %in% c(2,3,4))

# Add Baseline NTL: 1996 -------------------------------------------------------
## Load & Prep NTL
dmsp11 <- raster(file.path(ntl_harmon_dir, "RawData", "Harmonized_DN_NTL_2011_calDMSP.tif"))
dmsp11 <- crop(dmsp11, woreda_wgs84)
dmsp11 <- velox(dmsp11)

## Extract Lights
woreda$woreda_dmspols11_max <- dmsp11$extract(sp=woreda_wgs84, 
                                              small = T,
                                              fun = max) %>% as.vector()

#### Woreda NTL Groupings
ntl_values_pos <- woreda$woreda_dmspols11_max[woreda$woreda_dmspols11_max > 0]

## 4 bins
ntl_values_pos_q2 <- quantile(ntl_values_pos, c(1/3, 2/3))

woreda$wor_ntlgroup_2011_4bin <- NA
woreda$wor_ntlgroup_2011_4bin[woreda$woreda_dmspols11_max < ntl_values_pos_q2[1]] <- 2
woreda$wor_ntlgroup_2011_4bin[woreda$woreda_dmspols11_max >= ntl_values_pos_q2[1]] <- 3
woreda$wor_ntlgroup_2011_4bin[woreda$woreda_dmspols11_max > ntl_values_pos_q2[2]] <- 4
woreda$wor_ntlgroup_2011_4bin[woreda$woreda_dmspols11_max %in% 0:1] <- 1

## 2 bins (dark vs lit)
woreda$wor_ntlgroup_2011_2bin <- as.numeric(woreda$wor_ntlgroup_2011_4bin %in% c(2,3,4))

# ID ---------------------------------------------------------------------------
woreda$cell_id <- 1:nrow(woreda)

# Add average and sum ----------------------------------------------------------
saveRDS(woreda, file.path(woreda_dir, "FinalData", "woreda_mean_sum.Rds"))

# Export -----------------------------------------------------------------------
# Don't use these later on; don't need to be merged into main data
woreda$woreda_dmspols96_mean <- NULL
woreda$woreda_dmspols96_sum <- NULL

saveRDS(woreda, file.path(woreda_dir, "FinalData", "woreda.Rds"))

