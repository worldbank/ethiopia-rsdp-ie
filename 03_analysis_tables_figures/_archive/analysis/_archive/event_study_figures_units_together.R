# Event Study Figures: Urban

#### Parameters
p_dodge_width <- 1

# Load Data --------------------------------------------------------------------
data_urban <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_globcover_urban", "results_datasets",
                                "did_coef_every_year.Rds")) %>%
  filter(dep_var %in% c("dmspols_zhang_ihs",
                        "dmspols_zhang_sum2",
                        "dmspols_zhang_sum6",
                        "globcover_urban_sum",
                        "globcover_urban_sum_above0"))

data_ntl <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "results_datasets",
                              "did_coef_every_year.Rds")) %>%
  filter(dep_var %in% c("dmspols_zhang_ihs",
                        "dmspols_zhang_sum2",
                        "dmspols_zhang_sum6",
                        "globcover_urban_sum",
                        "dmspols_zhang_sum0greater_bin"))

data_grid <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_nearroad", "results_datasets",
                               "did_coef_every_year.Rds")) %>%
  filter(dep_var %in% c("dmspols_zhang_ihs",
                        "dmspols_zhang_2",
                        "dmspols_zhang_6",
                        "globcover_urban"))

# Prep Data --------------------------------------------------------------------
data <- bind_rows(
  data_urban %>%
    mutate(unit = "Urban Cluster"),
  data_ntl %>%
    mutate(unit = "NTL Cluster"),
  data_grid %>%
    mutate(unit = "1x1km Grid")
)

data <- data %>%
  filter(indep_var %>% str_detect("years_since_")) %>%
  filter(controls %in% "+temp_avg+precipitation") %>%
  
  ## Rename/Factor Dep Var
  mutate(dep_var = case_when(
    dep_var == "dmspols_zhang_ihs" ~ "IHS(NTL)",
    dep_var == "dmspols_zhang_sum2" ~ "NTL > 2",
    dep_var == "dmspols_zhang_2" ~ "NTL > 2",
    dep_var == "dmspols_zhang_sum6" ~ "NTL > 6",
    dep_var == "dmspols_zhang_6" ~ "NTL > 6",
    dep_var == "dmspols_zhang_sum0greater_bin" ~ "Cluster Exists",
    dep_var == "globcover_urban_sum_above0" ~ "Cluster Exists",
    dep_var == "globcover_urban_sum" ~ "Urban",
    dep_var == "globcover_urban" ~ "Urban"
  )) %>%
  mutate(indep_var = case_when(
    indep_var == "years_since_improvedroad" ~ "All",
    indep_var == "years_since_improvedroad_50aboveafter" ~ ">=50 km/hr",
    indep_var == "years_since_improvedroad_below50after" ~ "<50 km/hr"
  ))

# Figures ----------------------------------------------------------------------
ntl_group_i <- "All"
addis_distance_i <- "All"
dep_var_i <- "Urban"

title <- "Impact of Roads"

make_1_figure <- function(ntl_group_i,
                          dep_var_i,
                          addis_distance_i){
  
  title <- ""
  
  #if(addis_distance_i %in% "Far") title <- paste0(title, ", Areas >100km Addis Ababa")
  if(ntl_group_i %in% "All") title <- paste0(title, "All Units")
  if(ntl_group_i %in% "1") title <- paste0(title, "Baseline NTL Below Median")
  if(ntl_group_i %in% "2") title <- paste0(title, "Baseline NTL Above Median")
  
  p <- data %>%
    filter(dep_var %in% all_of(dep_var_i),
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
    facet_wrap(~unit,
               scales = "free_y",
               nrow = 1)
}

make_figures_by_base_ntl <- function(dep_var_i,
                                     addis_distance_i){
  
  p_all <- lapply(c("All", "1", "2"),
                  make_1_figure,
                  dep_var_i,
                  addis_distance_i)
  
  p_arrange <- ggarrange(p_all[[1]],
                         p_all[[2]],
                         p_all[[3]],
                         nrow = 3,
                         common.legend = T,
                         legend = "right")
  
  return(p_arrange)
}

for(addis_dist in c("All", "Far")){
  
  p <- make_figures_by_base_ntl("Urban", addis_dist)
  p <- annotate_figure(p, top = text_grob("Dependent Variable: Urban", color = "black", face = "bold", size = 14))
  ggsave(p,
         filename = file.path(paper_figures, paste0("eventstudy_Urban_",addis_dist,".png")),
         height = 6.5, width = 12)
  rm(p)
  
  p <- make_figures_by_base_ntl("NTL > 2", addis_dist)
  p <- annotate_figure(p, top = text_grob("Dependent Variable: NTL > 2", color = "black", face = "bold", size = 14))
  ggsave(p,
         filename = file.path(paper_figures, paste0("eventstudy_NTL_gt_2_",addis_dist,".png")),
         height = 6.5, width = 12)
  rm(p)
  
  p <- make_figures_by_base_ntl("NTL > 6", addis_dist)
  p <- annotate_figure(p, top = text_grob("Dependent Variable: NTL > 6", color = "black", face = "bold", size = 14))
  ggsave(p,
         filename = file.path(paper_figures, paste0("eventstudy_NTL_gt_6_",addis_dist,".png")),
         height = 6.5, width = 12)
  rm(p)
  
  p <- make_figures_by_base_ntl("Cluster Exists", addis_dist)
  p <- annotate_figure(p, top = text_grob("Dependent Variable: Cluster Exists", color = "black", face = "bold", size = 14))
  ggsave(p,
         filename = file.path(paper_figures, paste0("eventstudy_Cluster_Exists_",addis_dist,".png")),
         height = 6.5, width = 9)
  rm(p)
  
  p <- make_figures_by_base_ntl("IHS(NTL)", addis_dist)
  p <- annotate_figure(p, top = text_grob("Dependent Variable: IHS(NTL)", color = "black", face = "bold", size = 14))
  ggsave(p,
         filename = file.path(paper_figures, paste0("eventstudy_ihs_ntl_",addis_dist,".png")),
         height = 6.5, width = 12)
  rm(p)
  
}


