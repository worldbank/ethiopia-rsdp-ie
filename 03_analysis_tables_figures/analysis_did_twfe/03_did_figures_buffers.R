# DiD Results: Buffers

# Load data --------------------------------------------------------------------
did_df <- file.path(panel_rsdp_imp_dir,
                    "all_units",
                    "results_datasets",
                    "individual_datasets") %>%
  list.files(full.names = T,
             pattern = "*.Rds") %>%
  str_subset("dynamic_did_attgt") %>%
  str_subset("to") %>%
  str_subset("km") %>%
  map_df(readRDS) %>%
  mutate(est_type = "did") %>%
  dplyr::rename(years_since_improved = time,
                b = att) %>%
  mutate(p025 = b - se * critical_value_95p,
         p975 = b + se * critical_value_95p) %>%
  mutate(buff_dist = indep_var %>%
           str_replace_all(".*_", "") %>%
           str_replace_all("to", " to ") %>%
           str_replace_all("km", " km")) %>%
  mutate(indep_var_base = indep_var %>%
           str_replace_all("[:digit:]", "") %>%
           str_replace_all("to", "") %>% 
           str_replace_all("_km$", "")) %>%
  mutate(indep_var_name = case_when(
    indep_var_base == "year_improvedroad" ~ "All",
    indep_var_base == "year_improvedroad_50aboveafter" ~ "Federal and\nAsphalt Roads\n ", # ">=50 km/h",
    indep_var_base == "year_improvedroad_below50after" ~ "URRAP and\nLow to\nIntermediate\nClass Gravel\nRoads\n "
  )) %>%
  dplyr::filter(!(buff_dist %in% c("0 to 5 km",
                                   "20 to 25 km",
                                   "25 to 30 km"))) %>%
  mutate(buff_dist = buff_dist %>%
           factor(levels = c("0 to 1 km",
                             "1 to 2 km",
                             "2 to 3 km",
                             "3 to 4 km",
                             "4 to 5 km",
                             "5 to 10 km",
                             "10 to 15 km",
                             "15 to 20 km",
                             "20 to 25 km",
                             "25 to 30 km"))) %>%
  dplyr::filter(abs(years_since_improved) <= 10)

did_df <- did_df %>%
  dplyr::filter(ntl_num_groups %in% 4,
                addis_distance %in% "All")

# Figures ----------------------------------------------------------------------
did_df$dep_var %>% unique()

p_list <- list()
for(var in c("dmspols_harmon_ihs",
             "globcover_urban_sum_ihs",
             "globcover_urban_sum_log",
             "globcover_cropland_sum_ihs")){
  for(indep_var_base_i in unique(did_df$indep_var_base)){
    
    ##
    # if(var %in% "dmspols_harmon_ihs"){
    #   title = "Dependent Variable: Nighttime Lights"
    # }
    # 
    # if(var %in% c("globcover_urban_sum_ihs",
    #               "globcover_urban_sum_log")){
    #   title = "Dependent Variable: Urban"
    # }
    # 
    # if(var == "globcover_cropland_sum_ihs"){
    #   title = "Dependent Variable: Cropland"
    # }
    
    ## 
    if(indep_var_base_i %in% "year_improvedroad"){
      title = "All Roads"
    }
    
    if(indep_var_base_i %in% c("year_improvedroad_aboveafter")){
      title = "Federal and Asphalt Roads"
    }
    
    if(indep_var_base_i == "year_improvedroad_belowafter"){
      title = "URRAP and Low to Intermidate Class Gravel Roads"
    }
    
    p_list[[var]][[indep_var_base_i]] <- did_df %>%
      dplyr::filter(indep_var_base == indep_var_base_i,
                    ntl_group %in% "all",
                    dep_var == var) %>%
      ggplot(aes(x = years_since_improved, 
                 y = b, 
                 ymin = p025, 
                 ymax=p975)) +
      geom_point(position = position_dodge(width = 1),size=1) + 
      geom_linerange(position = position_dodge(width = 1),size=0.5) +
      geom_vline(xintercept=0,linewidth=.5,alpha=0.5) +
      geom_hline(yintercept=0,linewidth=.5,alpha=0.5) +
      labs(x="Years Since Road Improved",
           y="Coefficient (+/- 95% CI)",
           color="Baseline\nNighttime\nLights",
           title = title) +
      facet_wrap(~buff_dist, nrow = 1) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"))
    
  }
}

#### Cropland
p <- ggarrange(
  p_list$dmspols_harmon_ihs$year_improvedroad,
  p_list$dmspols_harmon_ihs$year_improvedroad_aboveafter,
  p_list$dmspols_harmon_ihs$year_improvedroad_belowafter,
  ncol = 1
)
p <- annotate_figure(p, top = text_grob("Dependent Variable: Nighttime Lights", 
                                        color = "black", face = "bold", size = 14))

ggsave(p, filename = file.path(paper_figures, "did_buffer_ntl.png"),
       height = 7, width = 9)

#### Urban
p <- ggarrange(
  p_list$globcover_urban_sum_ihs$year_improvedroad,
  p_list$globcover_urban_sum_ihs$year_improvedroad_aboveafter,
  p_list$globcover_urban_sum_ihs$year_improvedroad_belowafter,
  ncol = 1
)
p <- annotate_figure(p, top = text_grob("Dependent Variable: Urban", 
                                      color = "black", face = "bold", size = 14))

ggsave(p, filename = file.path(paper_figures, "did_buffer_urban.png"),
       height = 7, width = 9)

#### Cropland
p <- ggarrange(
  p_list$globcover_cropland_sum_ihs$year_improvedroad,
  p_list$globcover_cropland_sum_ihs$year_improvedroad_aboveafter,
  p_list$globcover_cropland_sum_ihs$year_improvedroad_belowafter,
  ncol = 1
)
p <- annotate_figure(p, top = text_grob("Dependent Variable: Cropland", 
                                        color = "black", face = "bold", size = 14))

ggsave(p, filename = file.path(paper_figures, "did_buffer_crop.png"),
       height = 7, width = 9)
