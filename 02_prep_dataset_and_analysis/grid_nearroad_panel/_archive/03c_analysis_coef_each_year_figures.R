# Analysis: Coefficient Each Year - Figures

#### Parameters
p_dodge_width <- 1

# Load and Prep Data -----------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_nearroad", "results_datasets",
                          "did_coef_every_year.Rds"))

data <- data %>%
  
  ## Only include relevant independent variables
  filter(indep_var %>% str_detect("years_since_")) %>%
  
  ## Only include relevant dependent variables
  filter(dep_var %in% c(#"dmspols_zhang", 
                        "dmspols_zhang_ihs",
                        #"dmspols_zhang_base0na",
                        "dmspols_zhang_ihs_base0na",
                        "dmspols_zhang_2",
                        "dmspols_zhang_6",
                        "globcover_urban",
                        "globcover_cropland",
                        "ndvi",
                        "ndvi_cropland")) %>%
  
  ## Rename/Factor Dep Var
  mutate(dep_var = case_when(
    #dep_var == "dmspols_zhang" ~ "NTL",
    dep_var == "dmspols_zhang_ihs" ~ "NTL (IHS)",
    dep_var == "dmspols_zhang_2" ~ "NTL > 2",
    dep_var == "dmspols_zhang_6" ~ "NTL > 6",
    #dep_var == "dmspols_zhang_base0na" ~ "NTL, Areas Lit at Baseline",
    dep_var == "dmspols_zhang_ihs_base0na" ~ "NTL (IHS), Areas Lit at Baseline",
    dep_var == "globcover_urban" ~ "Urban",
    dep_var == "globcover_cropland" ~ "Cropland",
    dep_var == "ndvi" ~ "NDVI",
    dep_var == "ndvi_cropland" ~ "NDVI, Cropland Areas"
  ) %>%
    factor(levels = c("NTL (IHS)", 
                      "NTL (IHS), Areas Lit at Baseline",
                      "NTL > 2", "NTL > 6", "Urban",
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
      filter(addis_distance %in% addis_distance_i,
             #dep_var %in% "globcover_urban",
             ntl_group %in% ntl_group_i,
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
      ggsave(filename = file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_nearroad",
                                  "outputs", "figures", 
                                  paste0("gridpanel_coefeachyear_addis",addis_distance_i,"_ntl",ntl_group_i,".png")),
             height = 5, width = 16)
    
  }
}







