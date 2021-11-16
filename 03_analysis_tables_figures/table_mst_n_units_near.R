# N Units Near MSTs

# Load Data --------------------------------------------------------------------
kebele_df <- readRDS(file.path(panel_rsdp_imp_dir, "kebele", "merged_datasets", 
                               "panel_data_clean.Rds"))

grid_df <- readRDS(file.path(panel_rsdp_imp_dir, "dmspols_grid_ethiopia", "merged_datasets", 
                             "panel_data_clean.Rds"))

# Summarise Data ---------------------------------------------------------------
summarise_data <- function(df){
  
  df_sum <- df %>%
    filter(year == 2016) %>%
    mutate(n_original = n()) %>%
    filter(distance_rsdp123_targettedlocs > 5000) %>%
    mutate(near_rsdp123 = distance_rsdp123 <= 5000,
           near_rsdp123_mst_euc = distance_rsdp123_mst_euc <= 5000,
           near_rsdp123_mst_lc = distance_rsdp123_mst_lc <= 5000,
           near_rsdp123_mst_euc_region = distance_rsdp123_mst_euc_region <= 5000,
           near_rsdp123_mst_lc_region = distance_rsdp123_mst_lc_region <= 5000) %>%
    dplyr::summarise(n_original                  = n_original[1],
                     n_treated                   = n(),
                     near_rsdp123                = sum(near_rsdp123),
                     near_rsdp123_mst_euc        = sum(near_rsdp123_mst_euc),
                     near_rsdp123_mst_lc         = sum(near_rsdp123_mst_lc),
                     near_rsdp123_mst_euc_region = sum(near_rsdp123_mst_euc_region),
                     near_rsdp123_mst_lc_region  = sum(near_rsdp123_mst_lc_region))
  
  return(df_sum)
}

kebele_sum_df <- kebele_df %>%
  summarise_data %>%
  mutate(dataset = "kebele")

grid_sum_df <- grid_df %>%
  summarise_data %>%
  mutate(dataset = "dmspols_grid_ethiopia")

sum_df <- bind_rows(
  kebele_sum_df,
  grid_sum_df
)

# Summary Tables ----------------------------------------------------------------
sum_df <- sum_df %>%
  arrange(dataset) %>%
  mutate(dataset = case_when(
    dataset %in% "kebele" ~ "Kebeles",
    dataset %in% "dmspols_grid_ethiopia" ~ "1x1km Grid"
  )) %>%
  mutate(tex = paste(dataset, "&",
                     n_original                  %>% prettyNum(big.mark=",",scientific=FALSE), "&",
                     near_rsdp123                %>% prettyNum(big.mark=",",scientific=FALSE), "&",
                     n_treated                   %>% prettyNum(big.mark=",",scientific=FALSE), "&",
                     near_rsdp123_mst_euc        %>% prettyNum(big.mark=",",scientific=FALSE), "&",
                     near_rsdp123_mst_euc_region %>% prettyNum(big.mark=",",scientific=FALSE), "&",
                     near_rsdp123_mst_lc         %>% prettyNum(big.mark=",",scientific=FALSE), "&",
                     near_rsdp123_mst_lc_region  %>% prettyNum(big.mark=",",scientific=FALSE), "\\\\ \n")) 

sink(file.path(paper_tables, "iv_sumstat_allunits.tex"))

cat("\\begin{tabular}{lcc | ccccc} \n")
cat("\\hline \n")
cat(" &  \\multicolumn{2}{c|}{Sample Size} & \\multicolumn{5}{c}{N Units Near RSDP and Different MSTs} \\\\ \n")
cat("\\hline \n")
cat(" Unit & N Total & N, Targeted   & RSDP  & MST Least & MST Least      & MST Least & MST Least  \\\\ \n")
cat("      &         & Areas Removed &       & Dist      & Dist: Regional & Cost      & Cost: Regional \\\\ \n")
cat("\\hline \n")

for(i in 1:nrow(sum_df)) cat(sum_df$tex[i])

cat("\\hline \n")
cat("\\end{tabular}")

sink()




