# Event Study Figures: Urban

#### Parameters
p_dodge_width <- 1

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_data_file_path,
                          "all_units", "results_datasets",
                          "appended_datasets",
                          "twowayFE_results.Rds"))

# Prep Data --------------------------------------------------------------------
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
  dplyr::filter(dep_var %in% c("Cropland", "NTL", "Urban")) %>%
  dplyr::mutate(dep_var = dep_var %>% factor(levels = c("NTL", "Urban", "Cropland")))

# Functions for Figures --------------------------------------------------------
make_1_figure <- function(ntl_group_i,
                          dataset_i,
                          addis_distance_i,
                          data,
                          title = NULL,
                          facet_nrow = 1){
  
  if(is.null(title)){
    title <- ""
    if(ntl_group_i %in% "All") title <- paste0(title, "All Units")
    if(ntl_group_i %in% "1") title <- paste0(title, "Units with Zero NTL at Baseline")
    if(ntl_group_i %in% "2") title <- paste0(title, "Units with Low NTL at Baseline")
    if(ntl_group_i %in% "3") title <- paste0(title, "Units with Medium NTL at Baseline")
    if(ntl_group_i %in% "4") title <- paste0(title, "Units with High NTL at Baseline")
  }
  
  p <- data %>%
    dplyr::filter(dataset %in% all_of(dataset_i),
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
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size=10),
          axis.title.x = element_text(size = 10)) +
    facet_wrap(~dep_var,
               scales = "free_y",
               nrow = facet_nrow)
  
  return(p)
}

make_figures_by_base_ntl <- function(dataset_i,
                                     addis_distance_i,
                                     data){
  
  p_all <- lapply(c("1", "2", "3", "4"), # c("All", "1", "2"),
                  make_1_figure,
                  dataset_i,
                  addis_distance_i,
                  data)
  
  p_arrange <- ggarrange(p_all[[1]],
                         p_all[[2]],
                         p_all[[3]],
                         p_all[[4]],
                         nrow = 4, 
                         common.legend = T,
                         legend = "bottom")
  
  return(p_arrange)
}

make_figures_all_and_by_base_ntl <- function(dataset_i,
                                     addis_distance_i,
                                     data){
  
  p_all <- lapply(c("All", "1", "2", "3", "4"), # c("All", "1", "2"),
                  make_1_figure,
                  dataset_i,
                  addis_distance_i,
                  data)
  
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

# Make Figures -----------------------------------------------------------------
HEIGHT_ALLUNITS = 6.5
WIDTH_ALLUNITS = 6

HEIGHT_BYBASENTL = 9
WIDTH_BYBASENTL = 9

HEIGHT_ALL_BYBASENTL <- 11
WIDTH_ALL_BYBASENTL <- 9

addis_dist <- "All"
for(addis_dist in c("All", "Far")){ # "All", "Far"
  
  # All Units ------------------------------------------------------------------
  p <- make_1_figure(ntl_group_i = "All", 
                     dataset_i = "kebele", 
                     addis_dist = addis_dist, 
                     data = data %>%
                       dplyr::mutate(dep_var = case_when(
                         dep_var == "NTL" ~ "Dependent Variable: Nighttime Lights",
                         dep_var == "Urban" ~ "Dependent Variable: Urban",
                         dep_var == "Cropland" ~ "Dependent Variable: Cropland",
                       ) %>%
                         factor(levels = c("Dependent Variable: Nighttime Lights",
                                           "Dependent Variable: Urban",
                                           "Dependent Variable: Cropland"))), 
                     title = "",
                     facet_nrow = 3)
  ggsave(p,
         filename = file.path(paper_figures, paste0("eventstudy_kebele_",addis_dist,"_allunits.png")),
         height = HEIGHT_ALLUNITS, width = WIDTH_ALLUNITS)
  rm(p)
  
  p <- make_1_figure(ntl_group_i = "All", 
                     dataset_i = "dmspols_grid_nearroad", 
                     addis_dist = addis_dist, 
                     data = data %>%
                       dplyr::mutate(dep_var = case_when(
                         dep_var == "NTL" ~ "Dependent Variable: Nighttime Lights",
                         dep_var == "Urban" ~ "Dependent Variable: Urban",
                         dep_var == "Cropland" ~ "Dependent Variable: Cropland",
                       ) %>%
                         factor(levels = c("Dependent Variable: Nighttime Lights",
                                           "Dependent Variable: Urban",
                                           "Dependent Variable: Cropland"))), 
                     title = "",
                     facet_nrow = 3)
  ggsave(p,
         filename = file.path(paper_figures, paste0("eventstudy_1kmgrid_",addis_dist,"_allunits.png")),
         height = HEIGHT_ALLUNITS, width = WIDTH_ALLUNITS)
  rm(p)
  
  # By Baseline NTL ------------------------------------------------------------
  p <- make_figures_by_base_ntl("kebele", 
                                addis_dist, 
                                data = data %>%
                                  dplyr::mutate(dep_var = dep_var %>% as.character,
                                                dep_var = case_when(
                                    dep_var == "NTL" ~ "Nighttime Lights",
                                    TRUE ~ dep_var
                                  ) %>%
                                    factor(levels = c("Nighttime Lights",
                                                      "Urban",
                                                      "Cropland"))))
  ggsave(p,
         filename = file.path(paper_figures, paste0("eventstudy_kebele_",addis_dist,"_bybasentl.png")),
         height = HEIGHT_BYBASENTL, width = WIDTH_BYBASENTL)
  rm(p)
  
  p <- make_figures_by_base_ntl("dmspols_grid_nearroad", 
                                addis_dist, 
                                data = data %>%
                                  dplyr::mutate(dep_var = dep_var %>% as.character,
                                                dep_var = case_when(
                                    dep_var == "NTL" ~ "Nighttime Lights",
                                    TRUE ~ dep_var
                                  ) %>%
                                    factor(levels = c("Nighttime Lights",
                                                      "Urban",
                                                      "Cropland"))))
  ggsave(p,
         filename = file.path(paper_figures, paste0("eventstudy_1kmgrid_",addis_dist,"_bybasentl.png")),
         height = HEIGHT_BYBASENTL, width = WIDTH_BYBASENTL)
  rm(p)
  
  # All Units & By Baseline NTL ------------------------------------------------
  p <- make_figures_all_and_by_base_ntl("kebele", 
                                addis_dist, 
                                data = data %>%
                                  dplyr::mutate(dep_var = dep_var %>% as.character,
                                                dep_var = case_when(
                                                  dep_var == "NTL" ~ "Nighttime Lights",
                                                  TRUE ~ dep_var
                                                ) %>%
                                                  factor(levels = c("Nighttime Lights",
                                                                    "Urban",
                                                                    "Cropland"))))
  ggsave(p,
         filename = file.path(paper_figures, paste0("eventstudy_kebele_",addis_dist,"_all_and_bybasentl.png")),
         height = HEIGHT_ALL_BYBASENTL, width = WIDTH_ALL_BYBASENTL)
  rm(p)
  
  p <- make_figures_all_and_by_base_ntl("dmspols_grid_nearroad", 
                                addis_dist, 
                                data = data %>%
                                  dplyr::mutate(dep_var = dep_var %>% as.character,
                                                dep_var = case_when(
                                                  dep_var == "NTL" ~ "Nighttime Lights",
                                                  TRUE ~ dep_var
                                                ) %>%
                                                  factor(levels = c("Nighttime Lights",
                                                                    "Urban",
                                                                    "Cropland"))))
  ggsave(p,
         filename = file.path(paper_figures, paste0("eventstudy_1kmgrid_",addis_dist,"_all_and_bybasentl.png")),
         height = HEIGHT_ALL_BYBASENTL, width = WIDTH_ALL_BYBASENTL)
  rm(p)
  
  
}



