# When land cover transitions to Urban, which land class did it transition from?

# Load Data --------------------------------------------------------------------
## Woredas
woreda <- readRDS(file.path(woreda_dir, "FinalData", "woreda.Rds")) %>%
  st_as_sf() %>%
  st_transform(4326)

## Globcover
# Create raster that is 1 if urban in any time period 
urban_constant <- raster(file.path(gc_dir, "RawData", "1992_2015_data", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), band=(1992-1991)) %>% crop(extent(woreda))
urban_constant[] <- as.numeric(urban_constant[] %in% c(190))

for(year in 1993:2015){
  print(year)
  urban_yyyy <- raster(file.path(gc_dir, "RawData", "1992_2015_data", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), band=(year-1991)) %>% crop(extent(woreda))
  urban_constant[] <- as.numeric((urban_constant[] %in% 1) | (urban_yyyy[] %in% c(190))) 
}

for(year in 2016:2018){
  print(year)
  urban_yyyy <- raster(file.path(gc_dir, "RawData", "2016_2018_data", paste0("C3S-LC-L4-LCCS-Map-300m-P1Y-",year,"-v2.1.1.tif"))) %>% crop(extent(woreda))
  urban_constant[] <- as.numeric((urban_constant[] %in% 1) | (urban_yyyy[] %in% c(190))) 
}

urban_constant <- urban_constant %>% mask(woreda)

# Create Urban Points ----------------------------------------------------------
coords <- coordinates(urban_constant) %>%
  as.data.frame() %>%
  dplyr::rename(lon = x,
                lat = y)
coords$value <- urban_constant[]

coords_urban <- coords %>%
  filter(value %in% 1)

coordinates(coords_urban) <- ~lon+lat
crs(coords_urban) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Extract Land Cover Values ----------------------------------------------------
for(year in 1992:2015){
  print(year)
  gc_yyyy <- raster(file.path(gc_dir, "RawData", "1992_2015_data", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), band=(year-1991)) %>% crop(extent(woreda))
  coords_urban[[paste0("gc_", year)]] <- terra::extract(gc_yyyy, coords_urban)
}

for(year in 2016:2018){
  print(year)
  gc_yyyy <- raster(file.path(gc_dir, "RawData", "2016_2018_data", paste0("C3S-LC-L4-LCCS-Map-300m-P1Y-",year,"-v2.1.1.tif"))) %>% crop(extent(woreda))
  coords_urban[[paste0("gc_", year)]] <- terra::extract(gc_yyyy, coords_urban)
}


lc_year_before <- lapply(1993:2018, function(year){
  
  coords_urban$year_i <- coords_urban[[paste0("gc_", year)]]
  coords_urban$year_lag <- coords_urban[[paste0("gc_", year - 1)]]
  
  lc <- coords_urban@data %>%
    filter(year_i == 190,
           year_lag != 190) %>%
    pull(year_lag)
  
  return(lc)
}) %>%
  unlist()

lc_year_before_df <- lc_year_before %>%
  table() %>%
  as.data.frame() %>%
  dplyr::rename(lc_class = ".",
                N = Freq) %>%
  mutate(lc_class_name = case_when(lc_class %in% 10 ~ "Cropland, rainfed",
                                   lc_class %in% 11 ~ "Cropland, rainfed - Herbaceous cover",
                                   lc_class %in% 20 ~ "Cropland, irrigated or post‐flooding",
                                   lc_class %in% 30 ~ "Mosaic cropland ($>$50\\%); natural vegetation (tree, shrub, herbaceous cover) ($<$50\\%)",
                                   lc_class %in% 40 ~ "Mosaic natural vegetation ($>$50\\%); cropland ($<$50\\%)",
                                   lc_class %in% 50 ~ "Tree cover, broadleaved, evergreen, closed to open ($>$15\\%)",
                                   lc_class %in% 60 ~ "Tree cover, broadleaved, deciduous, closed to open ($>$15\\%)",
                                   lc_class %in% 62 ~ "Tree cover, broadleaved, deciduous, open (15‐40\\%)",
                                   lc_class %in% 100 ~ "Mosaic tree and shrub ($>$50\\%); herbaceous cover ($<$50\\%)",
                                   lc_class %in% 110 ~ "Mosaic herbaceous cover ($>$50\\%); tree and shrub ($<$50\\%)",
                                   lc_class %in% 120 ~ "Shrubland",
                                   lc_class %in% 122 ~ "Deciduous shrubland",
                                   lc_class %in% 130 ~ "Grassland",
                                   lc_class %in% 150 ~ "Sparse vegetation (tree, shrub, herbaceous cover) ($<$15\\%)",
                                   lc_class %in% 152 ~ "Sparse shrub ($<$15\\%)",
                                   lc_class %in% 153 ~ "Sparse herbaceous cover ($<$15\\%)",
                                   lc_class %in% 170 ~ "Tree cover, flooded, saline water",
                                   lc_class %in% 180 ~ "Shrub or herbaceous cover, flooded - fresh, saline or brakish water",
                                   lc_class %in% 200 ~ "Bare areas",
                                   lc_class %in% 201 ~ "Consolidated bare areas",
                                   lc_class %in% 210 ~ "Water bodies")) %>%
  mutate(lc_class_simp_name = case_when(lc_class %in% 10 ~ "Cropland",
                                        lc_class %in% 11 ~ "Cropland",
                                        lc_class %in% 20 ~ "Cropland",
                                        lc_class %in% 30 ~ "Mosaic cropland and natural vegetation",
                                        lc_class %in% 40 ~ "Mosaic cropland and natural vegetation",
                                        lc_class %in% 50 ~ "Tree cover, shrubland, and/or herbaceous cover",
                                        lc_class %in% 60 ~ "Tree cover, shrubland, and/or herbaceous cover",
                                        lc_class %in% 62 ~ "Tree cover, shrubland, and/or herbaceous cover",
                                        lc_class %in% 100 ~ "Tree cover, shrubland, and/or herbaceous cover",
                                        lc_class %in% 110 ~ "Tree cover, shrubland, and/or herbaceous cover",
                                        lc_class %in% 120 ~ "Tree cover, shrubland, and/or herbaceous cover",
                                        lc_class %in% 122 ~ "Tree cover, shrubland, and/or herbaceous cover",
                                        lc_class %in% 130 ~ "Grassland",
                                        lc_class %in% 150 ~ "Tree cover, shrubland, and/or herbaceous cover",
                                        lc_class %in% 152 ~ "Tree cover, shrubland, and/or herbaceous cover",
                                        lc_class %in% 153 ~ "Tree cover, shrubland, and/or herbaceous cover",
                                        lc_class %in% 170 ~ "Tree cover, shrubland, and/or herbaceous cover",
                                        lc_class %in% 180 ~ "Tree cover, shrubland, and/or herbaceous cover",
                                        lc_class %in% 200 ~ "Bare areas",
                                        lc_class %in% 201 ~ "Bare areas",
                                        lc_class %in% 210 ~ "Water bodies")) %>%
  mutate(prop = N / sum(N),
         latex = paste(" ", lc_class_name, "&", 
                       N, "&",
                       round(prop,3), "\\\\ \n" )) %>%
  arrange(-N)

# Full -------------------------------------------------------------------------
sink(file.path(paper_tables, "lc_before_urban.tex"))
cat("\\begin{tabular}{l | ll} \n")
cat("\\hline \n")
cat("Land Cover Class & N 300m Pixels & Proportion \\\\ \n")
cat("\\hline \n")

for(i in 1:nrow(lc_year_before_df)){
  cat(lc_year_before_df$latex[i])
}

cat("\\hline \n")

cat(" TOTAL & ",
    sum(lc_year_before_df$N), 
    " & ", 1, "\\\\ " )

cat("\\hline \n")
cat("\\end{tabular} ")
sink()

# Simplified -------------------------------------------------------------------
lc_year_before_simp_df <- lc_year_before_df %>%
  group_by(lc_class_simp_name) %>%
  dplyr::summarise(N = sum(N),
                   prop = sum(prop)) %>%
  ungroup() %>%
  mutate(latex = paste(" ", lc_class_simp_name, "&", 
                       N, "&",
                       round(prop,3), "\\\\ \n" )) %>%
  arrange(-N) 

sink(file.path(paper_tables, "lc_before_urban_simpl.tex"))
cat("\\begin{tabular}{l | ll} \n")
cat("\\hline \n")
cat("Land Cover Class & N 300m Pixels & Proportion \\\\ \n")
cat("\\hline \n")

for(i in 1:nrow(lc_year_before_simp_df)){
  cat(lc_year_before_simp_df$latex[i])
}

cat("\\hline \n")

cat(" TOTAL & ",
    sum(lc_year_before_simp_df$N), 
    " & ", 1, "\\\\ " )

cat("\\hline \n")
cat("\\end{tabular} ")
sink()
