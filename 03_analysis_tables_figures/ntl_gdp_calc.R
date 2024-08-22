# NTL GDP Calc

eth_gdp <- 8548000000

#### Setup
keb_df <- readRDS(file.path(panel_rsdp_imp_dir, "kebele", "merged_datasets", "panel_data_clean.Rds"))

keb_df <- keb_df %>%
  dplyr::filter(year %in% 1996)

ntl_all  <- keb_df$dmspols_harmon %>% sum()

#### All Kebeles
## All roads
ntl_near <- keb_df$dmspols_harmon[keb_df$distance_anyimproved_ever <= 5000] %>% sum()
ntl_near / ntl_all

## RSDP 123
ntl_near <- keb_df$dmspols_harmon[keb_df$distance_rsdp123 <= 5000] %>% sum()
prop_ntl <- ntl_near / ntl_all
prop_ntl

prop_ntl * 
  eth_gdp * 
  0.1 * # effect
  1.3 # elasticity

247 865 583

#### Remove incidentally connected locations
keb_inc_df <- keb_df %>%
  dplyr::filter(distance_rsdp123_targettedlocs > 5000) %>%
  dplyr::filter(distance_anyimproved_ever <= 5000)

keb_inc_df$distance_rsdp123_targettedlocs %>% sum()
