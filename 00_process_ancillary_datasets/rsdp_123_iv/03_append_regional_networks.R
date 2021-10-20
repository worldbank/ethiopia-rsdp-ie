# Create Minimal Spanning Tree

mst_lc <- file.path(rsdp123_iv_dir, "FinalData", 
                    "mst_by_region") %>%
  list.files(pattern = "*.Rds", full.names = T) %>%
  str_subset("rsdpi_iii_targetted_loc_leastcost_mst_") %>%
  lapply(readRDS) %>%
  do.call(what = "rbind")

mst_ed <- file.path(rsdp123_iv_dir, "FinalData", 
                    "mst_by_region") %>%
  list.files(pattern = "*.Rds", full.names = T) %>%
  str_subset("rsdpi_iii_targetted_loc_eucdist_mst_") %>%
  lapply(readRDS) %>%
  do.call(what = "rbind")

saveRDS(mst_lc, file.path(rsdp123_iv_dir, "FinalData",
                          "rsdpi_iii_targetted_loc_leastcost_mst_region_appended.Rds"))

saveRDS(mst_ed, file.path(rsdp123_iv_dir, "FinalData",
                          "rsdpi_iii_targetted_loc_eucdist_mst_region_appended.Rds"))


