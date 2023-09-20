# Completion Year vs Randomly Assigned Completion Year

# Load data --------------------------------------------------------------------
rsdp_rand_sp  <- readRDS(file.path(rsdp_dir, "FinalData", "RoadNetworkPanelData_1996_2016_rand_year.Rds"))
rsdp_yr_rt_sp <- readRDS(file.path(rsdp_dir, "FinalData", "RoadNetworkPanelData_1996_2016_rand_year_restrict.Rds"))

# Cleanup ----------------------------------------------------------------------
rsdp_rand_sp$Complete_G_orig[rsdp_rand_sp$Complete_G_orig <= 1996]   <- 1996
rsdp_yr_rt_sp$Complete_G_orig[rsdp_yr_rt_sp$Complete_G_orig <= 1996] <- 1996

rsdp_rand_sp$Complete_G[rsdp_rand_sp$Complete_G <= 1996]   <- 1996
rsdp_yr_rt_sp$Complete_G[rsdp_yr_rt_sp$Complete_G <= 1996] <- 1996

diff_rand <- (rsdp_rand_sp$Complete_G - rsdp_rand_sp$Complete_G_orig)
diff_yr_rt <- (rsdp_yr_rt_sp$Complete_G - rsdp_yr_rt_sp$Complete_G_orig)

# Figures ----------------------------------------------------------------------
mean(diff_rand == 0)

table(rsdp_rand_sp$Complete_G_orig) / nrow(rsdp_rand_sp)

diff_rand %>%
  table() %>%
  as.data.frame() %>%
  dplyr::rename(diff = ".",
                n = Freq) %>%
  ggplot() +
  geom_col(aes(x = diff,
               y = n)) +
  labs(x = "Year difference between original and randomly assigned road completion year",
       y = "N roads") +
  theme_classic2()

ggsave(filename = file.path(paper_figures, "completion_year_rand.png"),
       height = 2.5, width = 9)

diff_yr_rt %>%
  table() %>%
  as.data.frame() %>%
  dplyr::rename(diff = ".",
                n = Freq) %>%
  ggplot() +
  geom_col(aes(x = diff,
               y = n)) +
  labs(x = "Year difference between original and randomly assigned road completion year",
       y = "N roads") +
  theme_classic2()

ggsave(filename = file.path(paper_figures, "completion_year_randrestrict.png"),
       height = 2.5, width = 9)




