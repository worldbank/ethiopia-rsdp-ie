# Event Study Figures: Urban

#### Parameters
p_dodge_width <- 1

# Load Data --------------------------------------------------------------------
data_woreda <- readRDS(file.path(panel_rsdp_imp_data_file_path, "woreda", "results_datasets",
                                 "did_coef_every_year.Rds")) %>%
  filter(dep_var %in% c("dmspols_harmon_ihs",
                        #"dmspols_harmon_ihs2013",
                        #"dmspols_zhang_sum2_ihs",
                        #"dmspols_zhang_sum6_ihs",
                        "globcover_cropland_sum_ihs",
                        "globcover_urban_sum_ihs"))

data_urban <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_globcover_urban", "results_datasets",
                                "did_coef_every_year.Rds")) %>%
  filter(dep_var %in% c("dmspols_harmon_ihs",
                        #"dmspols_harmon_ihs2013",
                        #"dmspols_harmon_sum2_ihs",
                        #"dmspols_harmon_sum6_ihs",
                        "globcover_urban_sum_ihs",
                        "globcover_cropland_sum_ihs"))

data_ntl <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntlall", "results_datasets",
                              "did_coef_every_year.Rds")) %>%
  filter(dep_var %in% c("dmspols_harmon_ihs",
                        #"dmspols_harmon_ihs2013",
                        #"dmspols_harmon_sum2_ihs",
                        #"dmspols_harmon_sum6_ihs",
                        #"dmspols_zhang_ihs",
                        #"dmspols_zhang_sum2_ihs",
                        #"dmspols_zhang_sum6_ihs",
                        "globcover_cropland_sum_ihs",
                        "globcover_urban_sum_ihs"))

data_grid <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_nearroad", "results_datasets",
                               "did_coef_every_year.Rds")) %>%
  filter(dep_var %in% c(#"dmspols_zhang_ihs",
                        #"dmspols_zhang_2",
                        #"dmspols_zhang_6",
                        "dmspols_harmon_ihs",
                        #"dmspols_harmon_ihs2013",
                        #"dmspols_harmon_2",
                        #"dmspols_harmon_6",
                        "globcover_cropland",
                        "globcover_urban"))

# Prep Data --------------------------------------------------------------------
data <- bind_rows(
  data_woreda %>%
    mutate(unit = "Woreda"),
  data_urban %>%
    mutate(unit = "Urban Cluster"),
  data_ntl %>%
    mutate(unit = "NTL Cluster"),
  data_grid %>%
    mutate(unit = "1x1km Grid")
)

data <- data %>%
  dplyr::filter(indep_var %>% str_detect("years_since_")) %>%
  dplyr::filter(controls %in% "+temp_avg+precipitation") %>%
  
  ## Rename/Factor Dep Var
  mutate(dep_var = case_when(
    dep_var == "dmspols_harmon_ihs" ~ "NTL",
    dep_var == "dmspols_harmon_ihs2013" ~ "NTL [2013]",
    dep_var == "dmspols_zhang_ihs" ~ "NTL",
    dep_var == "dmspols_harmon_sum2_ihs" ~ "NTL \u2265 2",
    dep_var == "dmspols_zhang_sum2_ihs" ~ "NTL \u2265 2",
    dep_var == "dmspols_harmon_2" ~ "NTL \u2265 2",
    dep_var == "dmspols_zhang_2" ~ "NTL \u2265 2",
    dep_var == "dmspols_harmon_sum6_ihs" ~ "NTL \u2265 6",
    dep_var == "dmspols_zhang_sum6_ihs" ~ "NTL \u2265 6",
    dep_var == "dmspols_harmon_6" ~ "NTL \u2265 6",
    dep_var == "dmspols_zhang_6" ~ "NTL \u2265 6",
    dep_var == "dmspols_harmon_sum0greater_bin" ~ "Cluster Exists",
    dep_var == "dmspols_zhang_sum0greater_bin" ~ "Cluster Exists",
    dep_var == "globcover_urban_sum_above0" ~ "Cluster Exists",
    dep_var == "globcover_urban_sum_ihs" ~ "Urban",
    dep_var == "globcover_urban" ~ "Urban",
    dep_var == "globcover_cropland_sum_ihs" ~ "Cropland",
    dep_var == "globcover_cropland" ~ "Cropland"
  )) %>%
  mutate(indep_var = case_when(
    indep_var == "years_since_improvedroad" ~ "All",
    indep_var == "years_since_improvedroad_50aboveafter" ~ ">=50 km/hr",
    indep_var == "years_since_improvedroad_below50after" ~ "<50 km/hr"
  )) %>%
  dplyr::filter(dep_var != "Cluster Exists")

# Figures ----------------------------------------------------------------------
ntl_group_i <- "All"
addis_distance_i <- "All"
dep_var_i <- "Urban"

title <- "Impact of Roads"

make_1_figure <- function(ntl_group_i,
                          unit_i,
                          addis_distance_i,
                          data){
  
  title <- ""
  
  #if(addis_distance_i %in% "Far") title <- paste0(title, ", Areas >100km Addis Ababa")
  if(ntl_group_i %in% "All") title <- paste0(title, "All Units")
  # if(ntl_group_i %in% "1") title <- paste0(title, "Baseline NTL Below Median")
  # if(ntl_group_i %in% "2") title <- paste0(title, "Baseline NTL Above Median")
  if(ntl_group_i %in% "1") title <- paste0(title, "Below Median Baseline Nighttime Lights")
  if(ntl_group_i %in% "2") title <- paste0(title, "Above Median Baseline Nighttime Lights")

  p <- data %>%
    filter(unit %in% all_of(unit_i),
           addis_distance %in% all_of(addis_distance_i),
           ntl_group %in% all_of(ntl_group_i)) %>%
    ggplot(aes(x = years_since_improved, y = b, ymin = p025, ymax=p975,
               group = indep_var, color=indep_var)) +
    geom_point(position = position_dodge(width = p_dodge_width),size=1) + 
    geom_linerange(position = position_dodge(width = p_dodge_width),size=0.5) +
    geom_vline(xintercept=0,size=.5,alpha=0.5) +
    geom_hline(yintercept=0,size=.5,alpha=0.5) +
    labs(x="Years Since Road Improved",
         y="Coefficient",
         color="Road Type",
         title = title) +
    scale_alpha_manual(values = c(0.1, 1)) +
    scale_color_manual(values = c("dodgerblue1", "darkorange", "black"),
                       guide = guide_legend(reverse = TRUE)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size=10)) +
    facet_wrap(~dep_var,
               scales = "free_y",
               nrow = 1)
}

make_figures_by_base_ntl <- function(unit_i,
                                     addis_distance_i,
                                     data){
  
  p_all <- lapply(c("All", "1", "2"),
                  make_1_figure,
                  unit_i,
                  addis_distance_i,
                  data)
  
  p_arrange <- ggarrange(p_all[[1]],
                         p_all[[2]],
                         p_all[[3]],
                         nrow = 3,
                         common.legend = T,
                         legend = "bottom")
  
  return(p_arrange)
}

addis_dist <- "All"
for(addis_dist in c("All")){ # "All", "Far"
  
  p <- make_figures_by_base_ntl("Woreda", addis_dist, data)
  ggsave(p,
         filename = file.path(paper_figures, paste0("eventstudy_woreda_",addis_dist,".png")),
         height = 7, width = 8.5)
  rm(p)
  
  p <- make_figures_by_base_ntl("NTL Cluster", addis_dist, data)
  ggsave(p,
         filename = file.path(paper_figures, paste0("eventstudy_ntlcluster_",addis_dist,".png")),
         height = 6.5, width = 8.5)
  rm(p)
  
  p <- make_figures_by_base_ntl("1x1km Grid", addis_dist, data)
  ggsave(p,
         filename = file.path(paper_figures, paste0("eventstudy_1kmgrid_",addis_dist,".png")),
         height = 6.5, width = 8.5)
  rm(p)
  
  p <- make_figures_by_base_ntl("Urban Cluster", addis_dist, data)
  ggsave(p,
         filename = file.path(paper_figures, paste0("eventstudy_urbancluster_",addis_dist,".png")),
         height = 6.5, width = 8.5)
  rm(p)
  
  
  data_city <- data[((data$unit %in% "NTL Cluster") & (data$dep_var %in% c("NTL", "NTL \u2265 6"))) |
                      ((data$unit %in% "Urban Cluster") & (data$dep_var %in% c("Urban"))),]
  p <- make_figures_by_base_ntl(c("NTL Cluster", "Urban Cluster"), addis_dist, data_city)
  ggsave(p,
         filename = file.path(paper_figures, paste0("eventstudy_cities_",addis_dist,".png")),
         height = 6.5, width = 8.5)
  rm(p)
  
}






