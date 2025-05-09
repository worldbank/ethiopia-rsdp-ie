# Woreda Summary Trends

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_dir, "woreda", "merged_datasets",
                          "panel_data_clean.Rds"))

data_diff <- readRDS(file.path(panel_rsdp_imp_dir, "woreda", "merged_datasets", 
                               "longdiff_data_clean_base1996_end2016.Rds"))

N1 <- data %>% filter(year == 1996, dmspols_harmon_1996_bin4 == 1) %>% nrow()
N2 <- data %>% filter(year == 1996, dmspols_harmon_1996_bin4 == 2) %>% nrow()
N3 <- data %>% filter(year == 1996, dmspols_harmon_1996_bin4 == 3) %>% nrow()
N4 <- data %>% filter(year == 1996, dmspols_harmon_1996_bin4 == 4) %>% nrow()

data_diff <- data_diff %>%
  dplyr::select(cell_id, dmspols_harmon_1996_bin4, globcover_urban_sum_ihs, 
                globcover_cropland_sum_ihs, dmspols_harmon_ihs)

data_sum <- data_diff %>%
  group_by(dmspols_harmon_1996_bin4, cell_id) %>%
  pivot_longer(cols = -c(dmspols_harmon_1996_bin4, cell_id)) %>%
  dplyr::filter(!is.na(value))

data_sum$name[data_sum$name %in% "dmspols_harmon_ihs"] <- "Average NTL"
data_sum$name[data_sum$name %in% "globcover_urban_sum_ihs"] <- "Urban"
data_sum$name[data_sum$name %in% "globcover_cropland_sum_ihs"] <- "Cropland"

data_sum$name <- data_sum$name %>% factor(levels = c("Average NTL", "Urban", "Cropland"))

make_figure <- function(df, title){
  
  df %>%
    ggplot() +
    geom_histogram(aes(x = value),
              #size = 1,
              fill = "dodgerblue3",
              bins = 20) +
    labs(x = "Growth Rate",
         y = NULL,
         title = title) +
    theme_minimal() + 
    theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
    facet_wrap(~name,
               scales = "free_y",
               nrow = 1) 
  
}

p1 <- make_figure(data_sum %>% filter(dmspols_harmon_1996_bin4 %in% 1),
            paste0("Woredas where maximum nighttime light value is 0 [N = ",N1,"]"))

p2 <- make_figure(data_sum %>% filter(dmspols_harmon_1996_bin4 %in% 2),
            paste0("Woredas where maximum nighttime light value is 1-5 [N = ",N2,"]"))

p3 <- make_figure(data_sum %>% filter(dmspols_harmon_1996_bin4 %in% 3),
            paste0("Woredas where maximum nighttime light value is 6-8 [N = ",N3,"]"))

p4 <- make_figure(data_sum %>% filter(dmspols_harmon_1996_bin4 %in% 4),
            paste0("Woredas where maximum nighttime light value is 9+ [N = ",N4,"]"))

p <- ggarrange(p1, p2, p3, p4,
               ncol = 1) %>%
  annotate_figure(top = text_grob("Growth rate from baseline to endline, by baseline levels of nighttime lights", color = "black", face = "bold", size = 14))

ggsave(p, filename = file.path(paper_figures, "ntl_histogram_by_baseline.png"), height = 8, width = 8.5)




