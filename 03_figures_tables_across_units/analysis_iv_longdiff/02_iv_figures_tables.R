# Figures and Tables Summarizing IV Results

# Load Data --------------------------------------------------------------------
dataset_types <- c("dmspols_grid_ethiopia", "kebele")

sum_df <- map_df(dataset_types, function(DATASET_TYPE){
  
  bind_rows(
    readRDS(file.path(project_file_path, "Data", "Panel Data RSDP Impacts",
                      "Data", DATASET_TYPE, "results_datasets", 
                      "iv_rsdp123_summary_stats.Rds")),
    readRDS(file.path(project_file_path, "Data", "Panel Data RSDP Impacts",
                      "Data", DATASET_TYPE, "results_datasets", 
                      "iv_rsdp1234_summary_stats.Rds"))
  )
  
})

# Summary Tables ----------------------------------------------------------------
sum_df <- sum_df %>%
  arrange(rsdp, dataset) %>%
  mutate(dataset = case_when(
    dataset %in% "kebele" ~ "Kebeles",
    dataset %in% "dmspols_grid_ethiopia" ~ "1x1km Grid"
  )) %>%
  mutate(rsdp = case_when(
    rsdp %in% "rsdp123" ~ "RSDP I-III",
    rsdp %in% "rsdp1234" ~ "RSDP I-V"
  )) %>%
  mutate(latex_table1 = paste(rsdp, "&",
                              dataset, "&",
                              n_original %>% prettyNum(big.mark=",",scientific=FALSE), "&",
                              n          %>% prettyNum(big.mark=",",scientific=FALSE), "&",
                              n_treated  %>% prettyNum(big.mark=",",scientific=FALSE), "&",
                              near_mst_euc_1  %>% prettyNum(big.mark=",",scientific=FALSE), "&",
                              near_mst_euc_region_1  %>% prettyNum(big.mark=",",scientific=FALSE), "&",
                              near_mst_lc_1  %>% prettyNum(big.mark=",",scientific=FALSE), "&",
                              near_mst_lc_region_1  %>% prettyNum(big.mark=",",scientific=FALSE), "\\\\ \n")) 


sink(file.path(paper_tables, "iv_sumstat_allunits.tex"))

sum_rsdp123_df <- sum_df[sum_df$rsdp %in% "RSDP I-III",]
sum_rsdp1234_df <- sum_df[sum_df$rsdp %in% "RSDP I-V",]

cat("\\begin{tabular}{llcc | ccccc} \n")
cat("\\hline \n")
cat(" &  & \\multicolumn{2}{c|}{Sample Size} & \\multicolumn{5}{c}{N Units Near RSDP and Different MSTs} \\\\ \n")
cat("\\hline \n")
cat(" RSDP & Unit & N Total & N, Targeted   & RSDP  & MST Least & MST Least      & MST Least & MST Least  \\\\ \n")
cat("      &      &         & Areas Removed &       & Dist      & Dist: Regional & Cost      & Cost: Regional \\\\ \n")
cat("\\hline \n")

for(i in 1:nrow(sum_rsdp123_df)) cat(sum_rsdp123_df$latex_table1[i])
cat("\\hline \n")
for(i in 1:nrow(sum_rsdp1234_df)) cat(sum_rsdp1234_df$latex_table1[i])

cat("\\hline \n")
cat("\\end{tabular}")

sink()

