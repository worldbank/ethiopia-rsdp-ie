# Number of Projects Near

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data_clean.Rds"))

data <- data %>%
  mutate(near_improvedroad_Nprjs = near_improvedroad_all_years %>% 
           str_replace_all(";", "") %>% 
           na_if("NA") %>%
           nchar() %>% 
           divide_by(4),
         near_improvedroad_below50after_Nprjs = near_improvedroad_below50after_all_years %>% 
           str_replace_all(";", "") %>% 
           na_if("NA") %>%
           nchar() %>% 
           divide_by(4) %>%
           replace_na(0),
         near_improvedroad_50aboveafter_Nprjs = near_improvedroad_50aboveafter_all_years %>% 
           str_replace_all(";", "") %>% 
           na_if("NA") %>%
           nchar() %>% 
           divide_by(4) %>%
           replace_na(0)) %>%
  filter(year == 2016)

N_all <- data$near_improvedroad_Nprjs %>% table() %>%
  as.data.frame() %>%
  dplyr::rename(N_roads = ".",
                N_all = Freq) %>%
  mutate(prop_all = N_all / sum(N_all))

N_below50after <- data$near_improvedroad_below50after_Nprjs %>% table() %>%
  as.data.frame() %>%
  dplyr::rename(N_roads = ".",
                N_below50after = Freq) %>%
  mutate(prop_below50after = N_below50after / sum(N_below50after))

N_50aboveafter <- data$near_improvedroad_50aboveafter_Nprjs %>% table() %>%
  as.data.frame() %>%
  dplyr::rename(N_roads = ".",
                N_50aboveafter = Freq) %>%
  mutate(prop_50aboveafter = N_50aboveafter / sum(N_50aboveafter))

N_all <- N_all %>%
  full_join(N_50aboveafter, by="N_roads", all=T) %>%
  full_join(N_below50after, by="N_roads", all=T) %>%
  mutate(N_roads = N_roads %>% as.character %>% as.numeric) %>%
  arrange(N_roads)

for(var in names(N_all)){
  N_all[[var]][is.na(N_all[[var]])] <- 0
}

N_all <- N_all %>%
  mutate(tex = paste(N_roads, 
                     N_all, 
                     prop_all %>% round(3),
                     N_50aboveafter, 
                     prop_50aboveafter %>% round(3),
                     N_below50after, 
                     prop_below50after %>% round(3), 
                     sep=" & ")) %>%
  mutate(tex = paste(tex, " \\\\ "))

# Make Table -------------------------------------------------------------------
# \\geq
sink(file.path(tables_file_path, "wards_N_years_improved.tex"))
cat("\\begin{tabular}{c | cc | cc | cc}  \n")
cat("\\hline \n")
cat("N years where road & \\multicolumn{2}{c|}{All Roads} & \\multicolumn{2}{c|}{$\\geq$ 50km/hr} & \\multicolumn{2}{c}{$<$ 50km/hr} \\\\  \n")
cat("improved in woreda & N Woredas & Prop & N Woredas & Prop & N Woredas & Prop \\\\ \n")
cat("\\hline \n")

for(i in 1:nrow(N_all)){
  cat(N_all$tex[i], " \n")
}

cat("\\hline \n")
cat("\\end{tabular} ")
sink()

