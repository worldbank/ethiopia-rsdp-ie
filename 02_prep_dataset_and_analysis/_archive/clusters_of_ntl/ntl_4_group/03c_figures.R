# Event Study Figures: Urban

#### Parameters
p_dodge_width <- 1

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "results_datasets",
                              "did_coef_every_year4.Rds")) %>%
  filter(dep_var %in% c("dmspols_zhang_ihs",
                        "dmspols_zhang_sum2_ihs",
                        "dmspols_zhang_sum6_ihs",
                        "globcover_urban_sum_ihs",
                        "dmspols_zhang_sum0greater_bin"))

data$unit <- "NTL Cluster"

# Prep Data --------------------------------------------------------------------
data <- data %>%
  dplyr::filter(indep_var %>% str_detect("years_since_")) %>%
  dplyr::filter(controls %in% "+temp_avg+precipitation") %>%
  
  ## Rename/Factor Dep Var
  mutate(dep_var = case_when(
    dep_var == "dmspols_zhang_ihs" ~ "IHS(NTL)",
    dep_var == "dmspols_zhang_sum2_ihs" ~ "NTL > 2",
    dep_var == "dmspols_zhang_2" ~ "NTL > 2",
    dep_var == "dmspols_zhang_sum6_ihs" ~ "NTL > 6",
    dep_var == "dmspols_zhang_6" ~ "NTL > 6",
    dep_var == "dmspols_zhang_sum0greater_bin" ~ "Cluster Exists",
    dep_var == "globcover_urban_sum_above0" ~ "Cluster Exists",
    dep_var == "globcover_urban_sum_ihs" ~ "Urban (Globcover)",
    dep_var == "globcover_urban" ~ "Urban (Globcover)"
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
                          addis_distance_i){
  
  title <- ""
  
  #if(addis_distance_i %in% "Far") title <- paste0(title, ", Areas >100km Addis Ababa")
  if(ntl_group_i %in% "All") title <- paste0(title, "All Units")
  if(ntl_group_i %in% "1") title <- paste0(title, "Zero")
  if(ntl_group_i %in% "2") title <- paste0(title, "Low")
  if(ntl_group_i %in% "3") title <- paste0(title, "Med")
  if(ntl_group_i %in% "4") title <- paste0(title, "High")
  
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
    scale_color_manual(values = c("dodgerblue1", "darkorange", "black"),
                       guide = guide_legend(reverse = TRUE)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size=10)) +
    facet_wrap(~dep_var,
               scales = "free_y",
               nrow = 1)
}

make_figures_by_base_ntl <- function(unit_i,
                                     addis_distance_i){
  
  p_all <- lapply(c("All", "1", "2", "3", "4"),
                  make_1_figure,
                  unit_i,
                  addis_distance_i)
  
  p_arrange <- ggarrange(p_all[[1]],
                         p_all[[2]],
                         p_all[[3]],
                         p_all[[4]],
                         p_all[[5]],
                         nrow = 5,
                         common.legend = T,
                         legend = "bottom")
  
  return(p_arrange)
}

for(addis_dist in c("All", "Far")){
  
  p <- make_figures_by_base_ntl("NTL Cluster", addis_dist)
  ggsave(p,
         filename = file.path(paper_figures, paste0("eventstudy4_ntlcluster_",addis_dist,".png")),
         height = 10.5, width = 12)
  rm(p)
  
  # p <- make_figures_by_base_ntl("1x1km Grid", addis_dist)
  # ggsave(p,
  #        filename = file.path(paper_figures, paste0("eventstudy4_1kmgrid_",addis_dist,".png")),
  #        height = 10.5, width = 12)
  # rm(p)
  # 
  # p <- make_figures_by_base_ntl("Urban Cluster", addis_dist)
  # ggsave(p,
  #        filename = file.path(paper_figures, paste0("eventstudy4_urbancluster_",addis_dist,".png")),
  #        height = 10.5, width = 12)
  # rm(p)
  
}






