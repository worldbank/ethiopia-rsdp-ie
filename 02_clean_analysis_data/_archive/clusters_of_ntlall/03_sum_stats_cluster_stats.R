# Summary Statistics

ROUND_NUM <- 2

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "merged_datasets", "panel_data_clean.Rds"))

data <- data %>%
  dplyr::select(cell_id, first_year_lit, cluster_n_cells) %>%
  distinct()

# Cluster Size Summary Stats ---------------------------------------------------
sink(file.path(panel_rsdp_imp_data_file_path, 
               "clusters_of_ntl", 
               "outputs",
               "tables",
               "cluster_size_sumStats.tex"))

cat("\\begin{tabular}{cccccc} \n")
cat("\\hline \n")
cat("Min & 25th Percentile & Median & Mean & 75th Percentile & Max \\\\")
cat("\\hline \n")

data$cluster_n_cells %>% min() %>% cat()
cat(" & ")
data$cluster_n_cells %>% quantile(.25) %>% round(ROUND_NUM) %>% cat()
cat(" & ")
data$cluster_n_cells %>% quantile(.50) %>% round(ROUND_NUM) %>% cat()
cat(" & ")
data$cluster_n_cells %>% mean() %>% round(ROUND_NUM) %>% cat()
cat(" & ")
data$cluster_n_cells %>% quantile(.75) %>% round(ROUND_NUM) %>% cat()
cat(" & ")
data$cluster_n_cells %>% max() %>% round(ROUND_NUM) %>% cat()
cat(" \\\\ ")

cat("\\hline \n")
cat("\\end{tabular} ")
sink()

# Cluster Size First Year Lit --------------------------------------------------
year_df <- data %>%
  group_by(first_year_lit) %>%
  summarise(N = n()) %>%
  mutate(prop = N/sum(N)) %>%
  mutate(prop_cumsum = cumsum(prop)) %>%
  mutate(latex = paste(first_year_lit, " & ", N, " & ", 
                       prop %>% round(2), 
                       " & ", 
                       prop_cumsum  %>% round(2), 
                       " \\\\ "))

sink(file.path(panel_rsdp_imp_data_file_path, 
               "clusters_of_ntl", 
               "outputs",
               "tables",
               "cluster_firstyear_sumStats.tex"))

cat("\\begin{tabular}{cccc} \n")

cat("\\hline \n")
cat("Year & N & Prop & Cumul. Prop \\\\ \n")
cat("\\hline \n")

for(i in 1:nrow(year_df)){
  year_df$latex[i] %>% cat()
}

cat("\\hline \n")
cat("\\end{tabular} ")
sink()



