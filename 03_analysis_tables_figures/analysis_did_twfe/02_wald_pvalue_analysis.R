# Analysis of P-Value of Wald Test

wald_df <- file.path(panel_rsdp_imp_dir,
                     "all_units",
                     "results_datasets",
                     "individual_datasets") %>%
  list.files(full.names = T) %>%
  str_subset("wald_test_pvalue") %>%
  map_df(readRDS)

wald_df_sub <- wald_df %>%
  dplyr::filter(ntl_num_groups %in% 4,
                controls %in% "none",
                dataset %in% "kebele") %>%
  arrange(indep_var, ntl_group)

wald_df_sub

wald_df[wald_df$dep_var %in% "dmspols_harmon_ihs",]
wald_df_sub[wald_df_sub$dep_var %in% "globcover_cropland_sum_ihs",]

  