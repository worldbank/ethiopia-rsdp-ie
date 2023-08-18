# Urban Increase

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(panel_rsdp_imp_dir, "kebele", "merged_datasets", "panel_data_clean.Rds"))

u1996 <- df %>%
  dplyr::filter(year == 1996) %>%
  pull(globcover_urban_sum) %>%
  sum()

u2016 <- df %>%
  dplyr::filter(year == 2016) %>%
  pull(globcover_urban_sum) %>%
  sum()

u1996*0.3 #sq km
u2016*0.3 #sq km

(u2016 - u1996)/u1996 * 100

