# Analysis: Coefficient Each Year - Figures

#### Parameters
p_dodge_width <- 1

# Load and Prep Data -----------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "results_datasets",
                          "did_coef_every_year.Rds"))

data <- data %>%
  filter(controls %in% "+temp_avg+precipitation")

# a <- data[data$dep_var %in% "dmspols_zhang_ihs",]
# b <- a %>%
#   dplyr::select(dep_var, addis_distance, ntl_group, controls) %>%
#   distinct()


data <- data %>%
  
  ## Only include relevant independent variables
  filter(indep_var %>% str_detect("years_since_")) %>%
  
  ## Only include relevant dependent variables
  filter(dep_var %in% c("dmspols_zhang_ihs",
                        "dmspols_zhang_sum2",
                        "dmspols_zhang_sum6",
                        "dmspols_zhang_sum0greater_bin",
                        "globcover_urban_sum",
                        "globcover_cropland_sum",
                        "ndvi",
                        "ndvi_cropland")) %>%
  
  ## Rename/Factor Dep Var
  mutate(dep_var = case_when(
    #dep_var == "dmspols_zhang" ~ "NTL",
    dep_var == "dmspols_zhang_ihs" ~ "NTL (IHS)",
    dep_var == "dmspols_zhang_sum2" ~ "NTL > 2",
    dep_var == "dmspols_zhang_sum6" ~ "NTL > 6",
    dep_var == "dmspols_zhang_sum0greater_bin" ~ "Cluster Exists",
    #dep_var == "dmspols_zhang_base0na" ~ "NTL, Areas Lit at Baseline",
    #dep_var == "dmspols_zhang_ihs_base0na" ~ "NTL (IHS), Areas Lit at Baseline",
    dep_var == "globcover_urban_sum" ~ "Urban",
    dep_var == "globcover_cropland_sum" ~ "Cropland",
    dep_var == "ndvi" ~ "NDVI",
    dep_var == "ndvi_cropland" ~ "NDVI, Cropland Areas"
  ) %>%
    factor(levels = c("NTL (IHS)", 
                      "NTL (IHS), Areas Lit at Baseline",
                      "NTL > 2", "NTL > 6", "Cluster Exists",  "Urban",
                      "Cropland", "NDVI", "NDVI, Cropland Areas"))) %>%
  
  ## Rename Indep Var
  mutate(indep_var = case_when(
    indep_var == "years_since_improvedroad" ~ "All",
    indep_var == "years_since_improvedroad_50aboveafter" ~ ">=50 km/hr",
    indep_var == "years_since_improvedroad_below50after" ~ "<50 km/hr"
  ))

# Figures ----------------------------------------------------------------------
for(ntl_group_i in c("All", "1", "2")){
  for(addis_distance_i in c("All", "Far")){
    
    title <- "Impact of Roads"
    
    print(paste(ntl_group_i, addis_distance_i))
    
    if(addis_distance_i %in% "Far") title <- paste0(title, ", Areas >100km Addis Ababa")
    if(ntl_group_i %in% "1") title <- paste0(title, ", Baseline NTL Below Median")
    if(ntl_group_i %in% "2") title <- paste0(title, ", Baseline NTL Above Median")

    data %>%
      filter(addis_distance %in% all_of(addis_distance_i),
             #dep_var %in% "globcover_urban",
             ntl_group %in% all_of(ntl_group_i),
             #indep_var %in% "years_since_improvedroad",
             controls %in% "+temp_avg+precipitation") %>%
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
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      facet_wrap(~dep_var,
                 scales = "free_y",
                 nrow = 2) +
      ggsave(filename = file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl",
                                  "outputs", "figures", 
                                  paste0("gridpanel_coefeachyear_addis",addis_distance_i,"_ntl",ntl_group_i,".png")),
             height = 5, width = 18)
    
  }
}







