# Long Diff Stats

NEAR_TARGETTED_LOCATION <- 5000
NEAR_ROAD <- 5000

df <- readRDS(file.path(panel_rsdp_imp_dir, "kebele", "merged_datasets", 
                        paste0("longdiff_data_clean_base", 
                               1996,
                               "_end",
                               2009,
                               ".Rds")))

df <- df %>%
  dplyr::mutate(near_rsdp123 = as.numeric(distance_rsdp123 <= NEAR_ROAD))

df_sub <- df %>%
  dplyr::filter(distance_rsdp123_targettedlocs > NEAR_TARGETTED_LOCATION)

nrow(df)
table(df$near_rsdp123)

nrow(df_sub)
table(df_sub$near_rsdp123)

