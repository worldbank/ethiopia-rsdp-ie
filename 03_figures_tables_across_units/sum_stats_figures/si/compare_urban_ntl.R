# Compare Urban and NTL

# Load Data --------------------------------------------------------------------
unit <- "kebele" # "clusters_of_ntl", "clusters_of_globcover_urban"
data_panel <- readRDS(file.path(panel_rsdp_imp_data_file_path, unit, "merged_datasets", 
                                "panel_data_clean.Rds"))
data_2016 <- data_panel %>%
  dplyr::filter(year %in% 2016)
data_1996 <- data_panel %>%
  dplyr::filter(year %in% 1996)

data_ld <- readRDS(file.path(panel_rsdp_imp_data_file_path, unit, "merged_datasets",
                             "longdiff_data_clean_base1996_end2016.Rds"))

cor.test(data_ld$dmspols_harmon_ihs,
         data_ld$globcover_urban_sum_ihs)

cor.test(data_2016$dmspols_harmon_ihs,
         data_2016$globcover_urban_sum_ihs)

data_1996 %>%
  dplyr::filter(dmspols_harmon_ihs > 0) %>%
  ggplot() +
  geom_histogram(aes(x=globcover_urban_sum_ihs))

data_1996 %>%
  dplyr::filter(globcover_urban_sum_ihs > 0) %>%
  ggplot() +
  geom_histogram(aes(x=dmspols_harmon_ihs))


