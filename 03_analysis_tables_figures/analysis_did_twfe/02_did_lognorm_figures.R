# DID Main Figures

p_dodge_width <- 0.5

did_df <- file.path(panel_rsdp_imp_dir,
                    "all_units",
                    "results_datasets",
                    "individual_datasets") %>%
  list.files(full.names = T,
             pattern = "*.Rds") %>%
  str_subset("dynamic_did_attgt") %>%
  map_df(readRDS) %>%
  mutate(est_type = "did") %>%
  dplyr::rename(years_since_improved = time,
                b = att) %>%
  mutate(p025 = b - se * critical_value_95p,
         p975 = b + se * critical_value_95p) %>%
  dplyr::filter(!(indep_var %>% str_detect("km")))

did_df <- did_df %>%
  dplyr::filter(dataset %in% c("kebele"),
                addis_distance %in% "All",
                controls == "none",
                ntl_num_groups %in% 4,
                indep_var %in% c("year_improvedroad",
                                 "year_improvedroad_50aboveafter",
                                 "year_improvedroad_below50after"),
                dep_var %>% str_detect("lognorm"),
                years_since_improved >= -10,
                years_since_improved <= 10) %>%
  dplyr::mutate(indep_var_clean = case_when(
    indep_var == "year_improvedroad" ~ "All RSDP\nRoads",
    indep_var == "year_improvedroad_50aboveafter" ~ "Federal and\nAsphalt Roads",
    indep_var == "year_improvedroad_below50after" ~ "URRAP and Low to Intermidate\nClass Gravel Roads"
  )) %>%
  dplyr::mutate(ntl_group_clean = case_when(
    ntl_group == "1" ~ "Dark, no initial nighttime lights",
    ntl_group == "2" ~ "Low initial nighttime lights",
    ntl_group == "3" ~ "Medium initial nighttime lights",
    ntl_group == "4" ~ "High initial nighttime lights",
  ) %>%
    factor(levels = c("Dark, no initial nighttime lights",
                      "Low initial nighttime lights",
                      "Medium initial nighttime lights",
                      "High initial nighttime lights")))

# Road -------------------------------------------------------------------------
for(x_i in c("0", "0_1", "1", "3")){
  
  did_all_df <- did_df %>%
    dplyr::filter(ntl_group %in% c("all"))
  
  make_roads_fig <- function(dep_var_i,
                             title){
    did_all_df %>%
      dplyr::filter(dep_var %in% dep_var_i) %>%
      ggplot(aes(x = years_since_improved, 
                 y = b, 
                 ymin = p025, 
                 ymax=p975)) +
      geom_point(position = position_dodge(width = p_dodge_width),size=1) + 
      geom_linerange(position = position_dodge(width = p_dodge_width),size=0.5) +
      geom_vline(xintercept=0,size=.5,alpha=0.5) +
      geom_hline(yintercept=0,size=.5,alpha=0.5) +
      labs(x="Years Since Road Improved",
           y="Coefficient (+/- 95% CI)",
           color="Road Type",
           title = title) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 10,
                                      color = "black", hjust = 0.5),
            strip.text = element_text(color = "black", size = 9),
            axis.title = element_text(size = 9),
            axis.text = element_text(size = 9)) +
      facet_wrap(~indep_var_clean)
  }
  
  p_road <- ggarrange(
    make_roads_fig(paste0("dmspols_harmon_lognorm_x", x_i),
                   "Dependent Variable: Nighttime Lights") +
      labs(x = NULL),
    
    make_roads_fig(paste0("globcover_urban_sum_lognorm_x",x_i),
                   "Dependent Variable: Urban") +
      labs(x = NULL),
    
    make_roads_fig(paste0("globcover_cropland_sum_lognorm_x", x_i),
                   "Dependent Variable: Cropland"),
    
    ncol = 1
  )
  
  # NTL -------------------------------------------------------------------------
  did_ntlgroup_df <- did_df %>%
    dplyr::filter(!(ntl_group %in% c("all")),
                  indep_var %in% "year_improvedroad")
  
  make_ntl_fig <- function(dep_var_i,
                           title){
    did_ntlgroup_df %>%
      dplyr::filter(dep_var %in% dep_var_i) %>%
      ggplot(aes(x = years_since_improved, 
                 y = b, 
                 ymin = p025, 
                 ymax=p975)) +
      geom_point(position = position_dodge(width = p_dodge_width),size=1) + 
      geom_linerange(position = position_dodge(width = p_dodge_width),size=0.5) +
      geom_vline(xintercept=0,size=.5,alpha=0.5) +
      geom_hline(yintercept=0,size=.5,alpha=0.5) +
      labs(x="Years Since Road Improved",
           y="Coefficient (+/- 95% CI)",
           color="Road Type",
           title = title) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 10,
                                      color = "black", hjust = 0.5),
            strip.text = element_text(color = "black", size = 9),
            axis.title = element_text(size = 9),
            axis.text = element_text(size = 9)) +
      facet_wrap(~ntl_group_clean,
                 nrow = 1)
  }
  
  p_ntl <- ggarrange(
    make_ntl_fig(paste0("dmspols_harmon_lognorm_x", x_i),
                 "Dependent Variable: Nighttime Lights") +
      labs(x = NULL),
    
    make_ntl_fig(paste0("globcover_urban_sum_lognorm_x",x_i),
                 "Dependent Variable: Urban") +
      labs(x = NULL),
    
    make_ntl_fig(paste0("globcover_cropland_sum_lognorm_x", x_i),
                 "Dependent Variable: Cropland"),
    
    ncol = 1
  )
  
  # Export -----------------------------------------------------------------------
  ggsave(p_road,
         filename = file.path(paper_figures, paste0("did_main_roads_lognorm_x",x_i,".png")),
         height = 8, width = 8)
  
  ggsave(p_ntl,
         filename = file.path(paper_figures, paste0("did_main_ntl_lognorm_x",x_i,".png")),
         height = 8, width = 8)
}

