# Check Spatial Autocorrelation

# conleyreg
# https://cran.r-project.org/web/packages/conleyreg/vignettes/conleyreg_introduction.html

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
woreda_clean <- readRDS(file.path(panel_rsdp_imp_dir, "woreda", "individual_datasets", "points.Rds"))
#woreda_clean <- woreda_clean %>% spTransform(CRS(UTM_ETH))
woreda_clean_c <- woreda_clean %>% gBuffer(width = 0, byid = T) %>% gCentroid(byid = T)
woreda_clean_c_df <- woreda_clean_c %>%
  coordinates() %>%
  as.data.frame() %>%
  dplyr::rename(lon = x,
                lat = y)
woreda_clean_c_df$woreda_id <- woreda_clean$cell_id
woreda_clean_c$woreda_id    <- woreda_clean$cell_id

# TWFE Regressions -------------------------------------------------------------

## Glovcover
data_gc <- data %>%
  dplyr::filter(!is.na(globcover_urban_sum_ihs))

lm_urban <- felm(globcover_urban_sum_ihs    ~ years_since_improvedroad | year + cell_id | 0 | woreda_id, data = data_gc)
lm_crop  <- felm(globcover_cropland_sum_ihs ~ years_since_improvedroad | year + cell_id | 0 | woreda_id, data = data_gc)

data_gc$lm_urban_resid <- lm_urban$residuals %>% as.numeric()
data_gc$lm_crop_resid  <- lm_crop$residuals  %>% as.numeric()

# data_w_gc <- data_gc %>%
#   group_by(woreda_id, year) %>%
#   dplyr::summarise(lm_urban_resid = sum(lm_urban_resid),
#                    lm_crop_resid = sum(lm_crop_resid)) %>%
#   ungroup()

## Nighttime lights
data_ntl <- data %>%
  dplyr::filter(!is.na(dmspols_harmon_ihs))

lm_ntl <- felm(dmspols_harmon_ihs ~ years_since_improvedroad | year + cell_id | 0 | woreda_id, data = data_ntl)

data_ntl$lm_ntl_resid <- lm_ntl$residuals %>% as.numeric()

# data_w_ntl <- data_ntl %>%
#   group_by(woreda_id, year) %>%
#   dplyr::summarise(lm_ntl_resid = sum(lm_ntl_resid)) %>%
#   ungroup()

## Merge
# data_w <- data_gc %>%
#   dplyr::select(cell_id, year, lm_crop_resid, lm_urban_resid) %>%
#   left_join(data_ntl, by = c("cell_id", "year"))

# Check Morans I ---------------------------------------------------------------
# https://mgimond.github.io/Spatial/spatial-autocorrelation-in-r.html
# 
# nb <- poly2nb(s1, queen=TRUE)
# lw <- nb2listw(nb, style="W", zero.policy=TRUE)

data_ntl_yr <- data_ntl[data_ntl$year %in% 2000,]

kebele_clean_c_ntl <- kebele_clean_c[kebele_clean_c$cell_id %in% data_ntl$cell_id,]
kebele_clean_c_ntl@data <- kebele_clean_c_ntl@data %>%
  left_join(data_ntl_yr, by = c("cell_id"))

kebele_clean_c_ntl.dist  <-  dnearneigh(kebele_clean_c_ntl, 0, 5) # km 
lw <- nb2listw(kebele_clean_c_ntl.dist, style="W",zero.policy=T) 
moran.test(kebele_clean_c_ntl$lm_ntl_resid, lw, zero.policy=T)
moran.mc(kebele_clean_c_ntl$lm_ntl_resid, lw, nsim=1000, zero.policy=T)

kebele_clean_c_ntl_sf <- kebele_clean_c_ntl %>% st_as_sf()
ggplot() +
  geom_sf(data = kebele_clean_c_ntl_sf,
             aes(color = lm_ntl_resid)) +
  scale_color_distiller(palette = "Spectral")





































year_i <- 2000

data_w_i <- data_w[data_w$year == year_i,]

woreda_clean$woreda_id <- woreda_clean$cell_id
woreda_clean@data <- woreda_clean@data %>%
  left_join(data_w_i, by = "woreda_id")

word_sp <- woreda_clean[!is.na(woreda_clean$lm_ntl_resid),]
nb <- poly2nb(word_sp, queen=TRUE)
lw <- nb2listw(nb, style="W",zero.policy=T) 

moran.test(word_sp$lm_crop_resid, lw, zero.policy=T)
moran.mc(word_sp$lm_ntl_resid, lw, nsim=1000, zero.policy=T)






nb <- poly2nb(woreda_clean, queen=TRUE)
lw <- nb2listw(nb, style="W",zero.policy=T) 

moran.test(woreda_clean_c$lm_ntl_resid, lw, zero.policy=T)



year_i <- 2000

data_w_i <- data_w[data_w$year == year_i,]

woreda_clean_c@data <- woreda_clean_c@data %>%
  left_join(data_w_i, by = "woreda_id")

word_sp <- woreda_clean_c[!is.na(woreda_clean_c$lm_ntl_resid),]
word_sp.dist <- dnearneigh(word_sp, 0, 10) # km 
lw <- nb2listw(word_sp.dist, style="W",zero.policy=T) 

nb <- poly2nb(s, queen=TRUE)

moran.test(word_sp$lm_ntl_resid, lw, zero.policy=T)
moran.mc(word_sp$lm_ntl_resid, lw, nsim=1000, zero.policy=T)

lw$neighbours[[100]]



kebele_clean_c_ntl <- kebele_clean_c[kebele_clean_c$cell_id %in% data_ntl$cell_id,]

year_i <- 2000

kebele_clean_c_ntl@data <- kebele_clean_c_ntl@data %>%
  left_join(data_ntl[data_ntl$year == year_i,], by = "cell_id")











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






