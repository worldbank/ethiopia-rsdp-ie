# Analysis: Coefficient Each Year - Figures

#### Parameters
p_dodge_width <- 1

# Load Data --------------------------------------------------------------------
dynamic_df <- file.path(panel_rsdp_imp_dir,
                        "all_units",
                        "results_datasets",
                        "individual_datasets") %>%
  list.files(full.names = T,
             pattern = "*.Rds") %>%
  str_subset("dynamic_did_attgt") %>%
  map_df(readRDS)

# Clean Data -------------------------------------------------------------------
dynamic_df <- dynamic_df %>%
  dplyr::filter(dep_var %in% c("dmspols_harmon_ihs",
                               "globcover_urban",
                               "globcover_urban_sum_ihs",
                               "globcover_cropland",
                               "globcover_cropland_sum_ihs")) %>%
  
  ## Rename/Factor Dep Var
  mutate(dep_var = case_when(
    dep_var == "dmspols_harmon_ihs" ~ "NTL",
    dep_var == "globcover_urban_sum_ihs" ~ "Urban",
    dep_var == "globcover_urban" ~ "Urban",
    dep_var == "globcover_cropland_sum_ihs" ~ "Cropland",
    dep_var == "globcover_cropland" ~ "Cropland"
  )) %>%
  mutate(indep_var = case_when(
    indep_var == "year_improvedroad" ~ "All",
    indep_var == "year_improvedroad_50aboveafter" ~ ">=50 km/hr",
    indep_var == "year_improvedroad_below50after" ~ "<50 km/hr"
  )) %>%
  dplyr::rename(years_since_improved = time,
                b = att) %>%
  mutate(p025 = b - se * critical_value_95p,
         p975 = b + se * critical_value_95p) %>%
  dplyr::filter(abs(years_since_improved) <= 10)

# Functions for Figures --------------------------------------------------------
make_1_figure <- function(ntl_group_i,
                          unit_i,
                          data){
  
  title <- ""
  
  if(ntl_group_i %in% "all") title <- paste0(title, "All Units")
  if(ntl_group_i %in% "1") title <- paste0(title, "Units with Zero NTL at Baseline")
  if(ntl_group_i %in% "2") title <- paste0(title, "Units with Low NTL at Baseline")
  if(ntl_group_i %in% "3") title <- paste0(title, "Units with Medium NTL at Baseline")
  if(ntl_group_i %in% "4") title <- paste0(title, "Units with High NTL at Baseline")
  
  p <- data %>%
    dplyr::filter(dataset %in% all_of(unit_i),
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
                                     data){
  
  p_all <- lapply(c("all", "1", "2", "3", "4"),
                  make_1_figure,
                  unit_i,
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
addis_dist <- "all"

HEIGHT <- 11
WIDTH <- 9

p <- make_figures_by_base_ntl("kebele", dynamic_df)
ggsave(p,
       filename = file.path(paper_figures, paste0("eventstudy_attgt_kebele_",addis_dist,".png")),
       height = HEIGHT, width = WIDTH)
rm(p)










