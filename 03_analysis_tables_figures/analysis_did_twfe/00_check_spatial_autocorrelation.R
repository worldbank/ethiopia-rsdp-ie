# Check Spatial Autocorrelation

# Load data --------------------------------------------------------------------
#### Analysis Data
data <- readRDS(file.path(panel_rsdp_imp_dir, "kebele", "merged_datasets", "panel_data_clean.Rds"))

data <- data %>%
  dplyr::filter(!is.na(years_since_improvedroad))

#### Kebele Data
kebele_clean <- readRDS(file.path(panel_rsdp_imp_dir, "kebele", "individual_datasets", "points.Rds"))
#kebele_clean <- kebele_clean %>% spTransform(CRS(UTM_ETH))
kebele_clean_c <- gCentroid(kebele_clean, byid = T)
kebele_clean_c_df <- kebele_clean_c %>%
  coordinates() %>%
  as.data.frame() %>%
  dplyr::rename(lon = x, 
                lat = y)
kebele_clean_c_df$cell_id <- kebele_clean$cell_id
kebele_clean_c$cell_id <- kebele_clean$cell_id

#### Woreda Data
# woreda_clean <- readRDS(file.path(panel_rsdp_imp_dir, "woreda", "individual_datasets", "points.Rds"))
# woreda_clean <- woreda_clean %>% spTransform(CRS(UTM_ETH))
# kebele_clean_c <- gCentroid(kebele_clean, byid = T)
# kebele_clean_c_df <- kebele_clean_c %>%
#   coordinates() %>%
#   as.data.frame() %>%
#   dplyr::rename(lon = x, 
#                 lat = y)
# kebele_clean_c_df$cell_id <- kebele_clean$cell_id

# TWFE Regressions -------------------------------------------------------------

## Glovcover
data_gc <- data %>%
  dplyr::filter(!is.na(globcover_urban_sum_ihs))

lm_urban <- felm(globcover_urban_sum_ihs ~ years_since_improvedroad | year + cell_id | 0 | woreda_id, data = data_gc)
lm_crop  <- felm(globcover_urban_sum_ihs ~ years_since_improvedroad | year + cell_id | 0 | woreda_id, data = data_gc)

data_gc$lm_urban_resid <- lm_urban$residuals %>% as.numeric()
data_gc$lm_crop_resid  <- lm_crop$residuals  %>% as.numeric()

data_sum_gc <- data_gc %>%
  group_by(cell_id) %>%
  dplyr::summarise(lm_urban_resid = sum(lm_urban_resid),
                   lm_crop_resid = sum(lm_crop_resid)) %>%
  ungroup()

## Nighttime lights
data_ntl <- data %>%
  dplyr::filter(!is.na(dmspols_harmon_ihs))

lm_ntl <- felm(dmspols_harmon_ihs ~ years_since_improvedroad | year + cell_id | 0 | woreda_id, data = data_ntl)

data_ntl$lm_ntl_resid <- lm_ntl$residuals %>% as.numeric()

data_sum_ntl <- data_ntl %>%
  group_by(cell_id) %>%
  dplyr::summarise(lm_ntl_resid = sum(lm_ntl_resid)) %>%
  ungroup()

## Merge
data_sum <- data_sum_gc %>%
  left_join(data_sum_ntl, by = "cell_id")

# Check Morans I ---------------------------------------------------------------
# https://mgimond.github.io/Spatial/spatial-autocorrelation-in-r.html
# 
# nb <- poly2nb(s1, queen=TRUE)
# lw <- nb2listw(nb, style="W", zero.policy=TRUE)

kebele_clean_c_ntl <- kebele_clean_c[kebele_clean_c$cell_id %in% data_sum_ntl$cell_id,]
kebele_clean_c_ntl@data <- kebele_clean_c_ntl@data %>%
  left_join(data_sum_ntl, by = "cell_id")

kebele_clean_c_ntl.dist  <-  dnearneigh(kebele_clean_c_ntl, 0, 100) # km 
lw <- nb2listw(kebele_clean_c_ntl.dist, style="W",zero.policy=T) 
moran.test(kebele_clean_c_ntl$lm_ntl_resid, lw, zero.policy=T)
moran.mc(kebele_clean_c_ntl$lm_ntl_resid, lw, nsim=1000, zero.policy=T)

# i <- 1
# 
# leaflet() %>%
#   addTiles() %>%
#   addCircles(data = kebele_clean_c_ntl[i,]) %>%
#   addCircles(data = kebele_clean_c_ntl[kebele_clean_c_ntl.dist[[i]],], color = "red")


mi2

data_sum_ntl <- data_sum_ntl %>%
  left_join(kebele_clean_c_df, by = "cell_id")
data_sum_gc <- data_sum_gc %>%
  left_join(kebele_clean_c_df, by = "cell_id")

ntl.dists <- as.matrix(dist(cbind(data_sum_ntl$lon, data_sum_ntl$lat)))
gc.dists  <- as.matrix(dist(cbind(data_sum_gc$lon, data_sum_gc$lat)))

ntl.dists.inv <- 1/ntl.dists
diag(ntl.dists.inv) <- 0

gc.dists.inv <- 1/gc.dists
diag(gc.dists.inv) <- 0

library(spdep)
library(ape)
Moran.I(data_sum_ntl$lm_ntl_resid,  ntl.dists.inv)
Moran.I(data_sum_gc$lm_urban_resid, gc.dists.inv)
Moran.I(data_sum_gc$lm_crop_resid, gc.dists.inv)






