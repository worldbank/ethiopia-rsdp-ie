# Stats: Urban in Woredas and Kebeles

keb_df <- readRDS(file.path(panel_rsdp_imp_dir, "kebele", "merged_datasets", "panel_data_clean.Rds"))
wor_df <- readRDS(file.path(panel_rsdp_imp_dir, "woreda", "merged_datasets", "panel_data_clean.Rds"))

keb_df <- keb_df %>%
  dplyr::filter(year %in% 1996)

wor_df <- wor_df %>%
  dplyr::filter(year %in% 1996)

mean(keb_df$globcover_urban_sum > 0) %>% round(2)
mean(wor_df$globcover_urban_sum > 0) %>% round(2)

mean(keb_df$dmspols_harmon > 0.5)
mean(wor_df$dmspols_harmon > 0.5)
