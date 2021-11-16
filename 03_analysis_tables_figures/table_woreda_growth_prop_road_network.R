# Trends in Road Variables using Woredas
library(wesanderson)

# Woreda Data ------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_dir, "woreda", "merged_datasets", "panel_data_clean.Rds"))

road_length_vars <- names(data) %>% str_subset("road_length_")
for(var in road_length_vars) data[[var]][ is.na(data[[var]]) ] <- 0

data_sum <- data %>%
  dplyr::filter(year %in% c(1996, 2016)) %>%
  dplyr::mutate(road_length_50above_prop = road_length_50above / road_length_10above) %>%
  dplyr::group_by(year, dmspols_harmon_1996_bin4) %>%
  dplyr::summarise(mean = mean(road_length_50above_prop, na.rm=T)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(dmspols_harmon_1996_bin4),
              names_from = year,
              values_from = mean) %>%
  dplyr::rename(prop_5kmabove_1996 = "1996",
                prop_5kmabove_2016 = "2016") %>%
  dplyr::mutate(per_change = 100*(prop_5kmabove_2016 - prop_5kmabove_1996)/prop_5kmabove_1996) %>%
  dplyr::mutate(category = case_when(
    dmspols_harmon_1996_bin4 %in% 1 ~ "Dark: Max NTL in Woreda, 0",
    dmspols_harmon_1996_bin4 %in% 2 ~ "Low: Max NTL in Woreda, 1 - 5",
    dmspols_harmon_1996_bin4 %in% 3 ~ "Medium: Max NTL in Woreda, 6 - 8",
    dmspols_harmon_1996_bin4 %in% 4 ~ "High: Max NTL in Woreda, 9+"
  )) %>%
  mutate(tex = paste0(category, " & ",
                      round(prop_5kmabove_1996, 2), " & ",
                      round(prop_5kmabove_2016, 2), " & ",
                      round(per_change, 2), " \\\\ "))

sink(file.path(paper_tables, "woreda_50kmabove_change.tex"))
cat("\\begin{tabular}{l  cc  c} \n")
cat("\\hline \n")
cat("Category & \\multicolumn{2}{c}{Prop. Road Network 50km and Above} & Per. Change \\\\ \n")
cat(" & 1996 & 2016 & \\\\ \n")
cat("\\hline \n")
for(i in 1:nrow(data_sum)) cat(data_sum$tex[i])
cat("\\hline \n")
cat("\\end{tabular} ")
sink()


