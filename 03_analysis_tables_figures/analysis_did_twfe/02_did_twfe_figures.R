# Analysis: Coefficient Each Year - Figures

#### Figure parameters

## Main
p_dodge_width_main <- 1
fig_height_main    <- 8
fig_width_main     <- 7
fig_nrow_main      <- 3

## SI
p_dodge_width_si <- 1
fig_height_si    <- 3
fig_width_si     <- 12
fig_nrow_si      <- 1

## General params
dpi = 600

# Load Data --------------------------------------------------------------------
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
         p975 = b + se * critical_value_95p) 

# did_df <- did_df %>%
#   dplyr::filter(dataset == "kebele",
#                 addis_distance == "All",
#                 controls == "none",
#                 est_type == "did",
#                 dep_var == "dmspols_harmon_ihs")

tw_df <- file.path(panel_rsdp_imp_dir,
                   "all_units",
                   "results_datasets",
                   "individual_datasets") %>%
  list.files(full.names = T,
             pattern = "*.Rds") %>%
  str_subset("twowayFE") %>%
  map_df(readRDS) %>%
  mutate(est_type = "tw") %>%
  mutate(controls = case_when(
    controls == "" ~ "none",
    controls == "+temp_avg+precipitation" ~ "temp_precip",
    TRUE ~ controls
  ))

df <- bind_rows(
  did_df,
  tw_df
)

# Clean Data -------------------------------------------------------------------
df <- df %>%
  dplyr::filter(dep_var %in% c("dmspols_harmon_ihs",
                               "dmspols_harmon_log",
                               
                               "dmspols_harmon_viirs_ihs",
                               "dmspols_harmon_viirs_log",
                               
                               #"viirs_bm_ihs",
                               #"viirs_bm_log",
                               
                               "globcover_urban",
                               "globcover_urban_sum_ihs",
                               "globcover_urban_sum_log",
                               
                               "globcover_cropland",
                               "globcover_cropland_sum_ihs",
                               "globcover_cropland_sum_log")) %>%
  
  dplyr::mutate(dep_var_raw = dep_var) %>%
  
  ## Rename/Factor Dep Var
  mutate(dep_var = case_when(
    dep_var == "dmspols_harmon_ihs" ~ "Dependent Variable: Nighttime Lights",
    dep_var == "dmspols_harmon_log" ~ "Dependent Variable: Nighttime Lights",
    
    dep_var == "dmspols_harmon_viirs_ihs" ~ "Dependent Variable: Nighttime Lights",
    dep_var == "dmspols_harmon_viirs_log" ~ "Dependent Variable: Nighttime Lights",
    
    dep_var == "viirs_bm_ihs" ~ "Dependent Variable: Nighttime Lights",
    dep_var == "viirs_bm_log" ~ "Dependent Variable: Nighttime Lights",
    
    dep_var == "globcover_urban_sum_log" ~ "Dependent Variable: Urban",
    dep_var == "globcover_urban_sum_ihs" ~ "Dependent Variable: Urban",
    dep_var == "globcover_urban" ~ "Dependent Variable: Urban",
    
    dep_var == "globcover_cropland_sum_log" ~ "Dependent Variable: Cropland",
    dep_var == "globcover_cropland_sum_ihs" ~ "Dependent Variable: Cropland",
    dep_var == "globcover_cropland" ~ "Dependent Variable: Cropland"
  )) %>%
  mutate(dep_var = dep_var %>% 
           factor(levels = c("Dependent Variable: Nighttime Lights", 
                             "Dependent Variable: Urban", 
                             "Dependent Variable: Cropland"))) %>%
  
  dplyr::mutate(indep_var_type = case_when(
    indep_var %>% str_detect("_rand$") ~ "_rand",
    indep_var %>% str_detect("_randrestrict$") ~ "_randrestrict",
    indep_var %>% str_detect("_randtreat$") ~ "_randtreat",
    indep_var %>% str_detect("improvedroad_p1to3") ~ "_p1to3",
    indep_var %>% str_detect("improvedroad_p4") ~ "_p4",
    TRUE ~ ""
  )) %>%
  
  mutate(indep_var = case_when(
    ## RSDP
    indep_var == "year_improvedroad" ~ "All",
    indep_var == "year_improvedroad_50aboveafter" ~ "Federal and\nAsphalt Roads\n ", # ">=50 km/h",
    indep_var == "year_improvedroad_below50after" ~ "URRAP and\nLow to\nIntermediate\nClass Gravel\nRoads\n ", # "<50 km/h",
    
    indep_var == "years_since_improvedroad" ~ "All",
    indep_var == "years_since_improvedroad_50aboveafter" ~ "Federal and\nAsphalt Roads\n ", #  ">=50 km/h",
    indep_var == "years_since_improvedroad_below50after" ~ "URRAP and\nLow to\nIntermediate\nClass Gravel\nRoads\n ", #  "<50 km/h",
    
    ## RSDP I to III
    indep_var == "year_improvedroad_p1to3" ~ "All",
    indep_var == "year_improvedroad_p1to3_50aboveafter" ~ "Federal and\nAsphalt Roads\n ", # ">=50 km/h",
    indep_var == "year_improvedroad_p1to3_below50after" ~ "URRAP and\nLow to\nIntermediate\nClass Gravel\nRoads\n ", # "<50 km/h",
    
    indep_var == "years_since_improvedroad_p1to3" ~ "All",
    indep_var == "years_since_improvedroad_p1to3_50aboveafter" ~ "Federal and\nAsphalt Roads\n ", #  ">=50 km/h",
    indep_var == "years_since_improvedroad_p1to3_below50after" ~ "URRAP and\nLow to\nIntermediate\nClass Gravel\nRoads\n ", #  "<50 km/h",
    
    ## Rand
    indep_var == "year_improvedroad_rand" ~ "All",
    indep_var == "year_improvedroad_50aboveafter_rand" ~ "Federal and\nAsphalt Roads\n ", # ">=50 km/h",
    indep_var == "year_improvedroad_below50after_rand" ~ "URRAP and\nLow to\nIntermediate\nClass Gravel\nRoads\n ", # "<50 km/h",
    
    indep_var == "years_since_improvedroad_rand" ~ "All",
    indep_var == "years_since_improvedroad_50aboveafter_rand" ~ "Federal and\nAsphalt Roads\n ", #  ">=50 km/h",
    indep_var == "years_since_improvedroad_below50after_rand" ~ "URRAP and\nLow to\nIntermediate\nClass Gravel\nRoads\n ", #  "<50 km/h",
    
    ## Rand Treat
    indep_var == "year_improvedroad_randtreat" ~ "All",
    indep_var == "year_improvedroad_50aboveafter_randtreat" ~ "Federal and\nAsphalt Roads\n ", # ">=50 km/h",
    indep_var == "year_improvedroad_below50after_randtreat" ~ "URRAP and\nLow to\nIntermediate\nClass Gravel\nRoads\n ", # "<50 km/h",
    
    indep_var == "years_since_improvedroad_randtreat" ~ "All",
    indep_var == "years_since_improvedroad_50aboveafter_randtreat" ~ "Federal and\nAsphalt Roads\n ", #  ">=50 km/h",
    indep_var == "years_since_improvedroad_below50after_randtreat" ~ "URRAP and\nLow to\nIntermediate\nClass Gravel\nRoads\n ", #  "<50 km/h",
    
    ## Rand Restrict
    indep_var == "year_improvedroad_randrestrict" ~ "All",
    indep_var == "year_improvedroad_50aboveafter_randrestrict" ~ "Federal and\nAsphalt Roads\n ", # ">=50 km/h",
    indep_var == "year_improvedroad_below50after_randrestrict" ~ "URRAP and\nLow to\nIntermediate\nClass Gravel\nRoads\n ", # "<50 km/h",
    
    indep_var == "years_since_improvedroad_randrestrict" ~ "All",
    indep_var == "years_since_improvedroad_50aboveafter_randrestrict" ~ "Federal and\nAsphalt Roads\n ", #  ">=50 km/h",
    indep_var == "years_since_improvedroad_below50after_randrestrict" ~ "URRAP and\nLow to\nIntermediate\nClass Gravel\nRoads\n ", #  "<50 km/h",
    TRUE ~ indep_var
  )) %>%
  
  mutate(ntl_group = case_when(
    ntl_group == "1" & ntl_num_groups == 4 ~ "Dark",
    ntl_group == "2" & ntl_num_groups == 4 ~ "Low",
    ntl_group == "3" & ntl_num_groups == 4 ~ "Medium",
    ntl_group == "4" & ntl_num_groups == 4 ~ "High",
    
    ntl_group == "0" & ntl_num_groups == 2 ~ "NTL = 0",
    ntl_group == "1" & ntl_num_groups == 2 ~ "NTL > 0",
    TRUE ~ ntl_group
  )) %>%
  mutate(ntl_group = ntl_group %>%
           factor(levels = c("all",
                             "Dark",
                             "Low",
                             "Medium",
                             "High",
                             "NTL = 0",
                             "NTL > 0"))) %>%
  
  dplyr::filter(abs(years_since_improved) <= 10)

df$indep_var[df$indep_var_type == ""] %>% is.na %>% table()
df$indep_var[df$indep_var_type == "_rand"] %>% is.na %>% table()
df$indep_var[df$indep_var_type == "_randrestrict"] %>% is.na %>% table()

df$indep_var[df$indep_var_type == ""] %>% table()
df$indep_var[df$indep_var_type == "_rand"] %>% table()
df$indep_var[df$indep_var_type == "_randrestrict"] %>% table()

# Individual Figure ------------------------------------------------------------
# if(F){
#   df_sub <- df %>%
#     dplyr::filter(dataset %in% "kebele",
#                   addis_distance %in% "All",
#                   controls %in% "none",
#                   est_type %in% "did",
#                   indep_var_type %in% "_randrestrict") # did, tw
#   
#   p_dodge_width <- 1
#   df_sub %>%
#     dplyr::filter(indep_var %in% "All",
#                   ntl_num_groups %in% 4,
#                   ntl_group != "all",
#                   dep_var_raw %in% c("dmspols_harmon_ihs",
#                                      "globcover_urban_sum_ihs",
#                                      "globcover_cropland_sum_ihs")) %>%
#     ggplot(aes(x = years_since_improved,
#                y = b,
#                ymin = p025,
#                ymax=p975,
#                group = ntl_group,
#                color = ntl_group)) +
#     geom_point(position = position_dodge(width = p_dodge_width),size=1) +
#     geom_linerange(position = position_dodge(width = p_dodge_width),size=0.5) +
#     geom_vline(xintercept=0,size=.5,alpha=0.5) +
#     geom_hline(yintercept=0,size=.5,alpha=0.5) +
#     labs(x="Years Since Road Improved",
#          y="Coefficient (+/- 95% CI)",
#          color="Baseline\nNighttime\nLights") +
#     scale_alpha_manual(values = c(0.1, 1)) +
#     # scale_color_manual(values = c("gray20", "gray60", "darkorange", "firebrick2"),
#     #                    guide = guide_legend(reverse = TRUE)) +
#     theme_minimal() +
#     theme(strip.text = element_text(face = "bold", color = "black")) +
#     facet_wrap(dep_var~ntl_group,
#                scales = "free_y",
#                ncol = 4)
#   
#   
#   
#   p_dodge_width <- 1
#   df_sub %>%
#     dplyr::filter(indep_var %in% "year_improvedroad_p4",
#                   ntl_num_groups %in% 4,
#                   ntl_group != "all",
#                   dep_var_raw %in% c("viirs_bm_ihs",
#                                      "globcover_urban_sum_ihs",
#                                      "globcover_cropland_sum_ihs")) %>%
#     ggplot(aes(x = years_since_improved,
#                y = b,
#                ymin = p025,
#                ymax=p975,
#                group = ntl_group,
#                color = ntl_group)) +
#     geom_point(position = position_dodge(width = p_dodge_width),size=1) +
#     geom_linerange(position = position_dodge(width = p_dodge_width),size=0.5) +
#     geom_vline(xintercept=0,size=.5,alpha=0.5) +
#     geom_hline(yintercept=0,size=.5,alpha=0.5) +
#     labs(x="Years Since Road Improved",
#          y="Coefficient (+/- 95% CI)",
#          color="Baseline\nNighttime\nLights") +
#     scale_alpha_manual(values = c(0.1, 1)) +
#     # scale_color_manual(values = c("gray20", "gray60", "darkorange", "firebrick2"),
#     #                    guide = guide_legend(reverse = TRUE)) +
#     theme_minimal() +
#     theme(strip.text = element_text(face = "bold", color = "black")) +
#     facet_wrap(dep_var~ntl_group,
#                scales = "free_y",
#                ncol = 4)
#   
#   
#   
#   p_dodge_width <- 1
#   df_sub %>%
#     dplyr::filter(indep_var %in% "All",
#                   ntl_num_groups %in% 4,
#                   ntl_group != "all",
#                   dep_var_raw %in% c("dmspols_harmon_ihs",
#                                      "globcover_urban_sum_ihs",
#                                      "globcover_cropland_sum_ihs")) %>%
#     ggplot(aes(x = years_since_improved,
#                y = b,
#                ymin = p025,
#                ymax=p975,
#                group = ntl_group,
#                color = ntl_group)) +
#     geom_point(position = position_dodge(width = p_dodge_width),size=1) +
#     geom_linerange(position = position_dodge(width = p_dodge_width),size=0.5) +
#     geom_vline(xintercept=0,size=.5,alpha=0.5) +
#     geom_hline(yintercept=0,size=.5,alpha=0.5) +
#     labs(x="Years Since Road Improved",
#          y="Coefficient (+/- 95% CI)",
#          color="Baseline\nNighttime\nLights") +
#     scale_alpha_manual(values = c(0.1, 1)) +
#     # scale_color_manual(values = c("gray20", "gray60", "darkorange", "firebrick2"),
#     #                    guide = guide_legend(reverse = TRUE)) +
#     theme_minimal() +
#     theme(strip.text = element_text(face = "bold", color = "black")) +
#     facet_wrap(dep_var~ntl_group,
#                scales = "free_y",
#                ncol = 4)
# }

# Loop over figures ------------------------------------------------------------
dataset_i = "kebele"
addis_dist_i = "All"
controls_i = "none"
est_type_i = "did"
trans_type = "ihs"
indep_var_type_i = ""

dataset_i
addis_dist_i
controls_i
est_type_i
trans_type
indep_var_type_i

for(dataset_i in c("kebele", "dmspols_grid_nearroad")){ # 
  for(addis_dist_i in c("All", "Far")){
    for(controls_i in c("none")){ 
      for(est_type_i in c("did", "tw")){
        for(trans_type in c("ihs", "log")){ # "ihs", "log"
          for(indep_var_type_i in c("", "_p1to3", "_rand", "_randrestrict", "_randtreat")){
            
            if( (indep_var_type_i == "_p1to3") & (est_type_i == "tw") ){
              next
            }
            
            if( (indep_var_type_i == "_p1to3") & (dataset_i == "dmspols_grid_nearroad") ){
              next
            }
            
            if( (controls_i == "splag") & (dataset_i == "dmspols_grid_nearroad") ){
              next
            }
            
            if( (controls_i == "splag") & (est_type_i == "tw") ){
              next
            }
            
            if( (controls_i == "splag") & (trans_type == "log") ){
              next
            }
            
            if( (controls_i == "splag") & (indep_var_type_i %in% c("_rand", "_randrestrict", "_randtreat"))){
              next
            }
            
            if( (trans_type == "log") & (str_detect(indep_var_type_i, "_rand")) ){
              next
            }
            
            if( (addis_dist_i == "Far") & (str_detect(indep_var_type_i, "_rand")) ){
              next
            }
            
            if( (dataset_i == "dmspols_grid_nearroad") & (str_detect(indep_var_type_i, "_rand")) ){
              next
            }
            
            # Skip certain parameter combinations ----------------------------------
            if((dataset_i == "dmspols_grid_nearroad") & (addis_dist_i == "Far")){
              next
            }
            
            if(trans_type == "log" & est_type_i == "tw"){
              next
            }
            
            # Transformation type ------------------------------------------------
            if(trans_type == "ihs"){
              # Only indicate if log, for appendix
              trans_type_suffix <- ""
              
              if(dataset_i == "kebele"){
              df_sub <- df %>%
                dplyr::filter(dep_var_raw %>% str_detect("_ihs$"))
              } else{
                df_sub <- df %>%
                  dplyr::filter(dep_var_raw %in% c("globcover_urban",
                                                   "globcover_cropland",
                                                   "dmspols_harmon_ihs"))
              }
              
            } else{
              trans_type_suffix <- "_log"
              
              if(dataset_i == "kebele"){
                df_sub <- df %>%
                  dplyr::filter(dep_var_raw %>% str_detect("_log$"))
              } else{
                df_sub <- df %>%
                  dplyr::filter(dep_var_raw %in% c("globcover_urban",
                                                   "globcover_cropland",
                                                   "dmspols_harmon_log"))
              }

            }
            
            df_sub_dmsp <- df_sub %>%
              dplyr::filter(dep_var == "Dependent Variable: Nighttime Lights") %>%
              dplyr::mutate(dep_var = case_when(
                dep_var_raw == "dmspols_harmon_ihs" ~ "Dependent Variable: Nighttime Lights, DMSP in 2013",
                dep_var_raw == "dmspols_harmon_viirs_ihs" ~ "Dependent Variable: Nighttime Lights, Simulated DMSP in 2013"
              ))
            
            df_sub <- df_sub %>%
              dplyr::filter(!(dep_var_raw %in% c("dmspols_harmon_viirs_ihs",
                                                 "dmspols_harmon_viirs_log")))
            
            # Controls Suffix --------------------------------------------------
            if(controls_i == "none")  controls_suffix_i <- ""
            if(controls_i == "splag") controls_suffix_i <- "_splag"
            
            # Parameters -----------------------------------------------------------
            # Vary parameters by whether the figures will go in the main text or be 
            # part of the SI
            
            if((dataset_i == "kebele") & 
               (addis_dist_i == "All") & 
               (controls_i %in% c("none", "splag")) &
               (est_type_i == "did") &
               (trans_type == "ihs")){
              
              ## Main text
              # p_dodge_width <- p_dodge_width_main
              # fig_height <- fig_height_main
              # fig_width <- fig_width_main
              # fig_nrow <- fig_nrow_main
              
              p_dodge_width <- p_dodge_width_si
              fig_height <- fig_height_si
              fig_width <- fig_width_si
              fig_nrow <- fig_nrow_si
              
            } else{
              
              ## SI
              p_dodge_width <- p_dodge_width_si
              fig_height <- fig_height_si
              fig_width <- fig_width_si
              fig_nrow <- fig_nrow_si
            }
            
            # Figure: Different DMSP: By Road Type -------------------------------------------
            if(dataset_i == "kebele"){
              if(est_type_i == "did"){
                if(indep_var_type_i == ""){
                  if(addis_dist_i == "All"){ #
                    if(controls_i == "none"){ #
                      if(trans_type == "ihs"){
                        # df_sub_dmsp <- df_sub_dmsp %>%
                        #   dplyr::mutate(dep_var = case_when(
                        #     dep_var_raw == "dmspols_harmon_ihs" ~ "Dependent Variable: Nighttime Lights, DMSP in 2012/13",
                        #     dep_var_raw == "dmspols_harmon_viirs_ihs" ~ "Dependent Variable: Nighttime Lights, Simulated DMSP in 2012/13"
                        #   ))
                        
                        p <- df_sub_dmsp %>%
                          dplyr::filter(dataset %in% dataset_i,
                                        addis_distance %in% addis_dist_i,
                                        indep_var_type %in% indep_var_type_i,
                                        ntl_num_groups %in% 4,
                                        ntl_group == "all",
                                        controls == controls_i,
                                        est_type == est_type_i) %>%
                          ggplot(aes(x = years_since_improved, 
                                     y = b, 
                                     ymin = p025, 
                                     ymax=p975,
                                     group = indep_var, 
                                     color = indep_var)) +
                          geom_point(position = position_dodge(width = p_dodge_width),size=1) + 
                          geom_linerange(position = position_dodge(width = p_dodge_width),size=0.5) +
                          geom_vline(xintercept=0,size=.5,alpha=0.5) +
                          geom_hline(yintercept=0,size=.5,alpha=0.5) +
                          labs(x="Years Since Road Improved",
                               y="Coefficient (+/- 95% CI)",
                               color="Road Type") +
                          scale_alpha_manual(values = c(0.1, 1)) +
                          scale_color_manual(values = c("dodgerblue1", "darkorange", "black"),
                                             guide = guide_legend(reverse = TRUE)) +
                          theme_minimal() +
                          theme(strip.text = element_text(face = "bold", color = "black")) +
                          facet_wrap(~dep_var,
                                     scales = "free_y",
                                     nrow = fig_nrow)
                        
                        ggsave(p,
                               filename = file.path(paper_figures, 
                                                    paste0(est_type_i,"_byroad_",dataset_i,"_",addis_dist_i,trans_type_suffix,indep_var_type_i,controls_suffix_i,"_dmsp_viirs.png")),
                               height = fig_height, 
                               width = fig_width,
                               dpi = dpi)
                        
                        # Figure: By 4 Speeds --------------------------------------------------
                        p <- df_sub_dmsp %>%
                          dplyr::filter(dataset %in% dataset_i,
                                        addis_distance %in% addis_dist_i,
                                        indep_var_type %in% indep_var_type_i,
                                        ntl_num_groups %in% 4,
                                        ntl_group != "all",
                                        indep_var %in% "All",
                                        controls == controls_i,
                                        est_type == est_type_i) %>%
                          ggplot(aes(x = years_since_improved, 
                                     y = b, 
                                     ymin = p025, 
                                     ymax=p975,
                                     group = ntl_group, 
                                     color = ntl_group)) +
                          geom_point(position = position_dodge(width = p_dodge_width),size=1) + 
                          geom_linerange(position = position_dodge(width = p_dodge_width),size=0.5) +
                          geom_vline(xintercept=0,size=.5,alpha=0.5) +
                          geom_hline(yintercept=0,size=.5,alpha=0.5) +
                          labs(x="Years Since Road Improved",
                               y="Coefficient (+/- 95% CI)",
                               color="Baseline\nNighttime\nLights") +
                          scale_alpha_manual(values = c(0.1, 1)) +
                          scale_color_manual(values = c("gray20", "gray60", "darkorange", "firebrick2"),
                                             guide = guide_legend(reverse = TRUE)) +
                          theme_minimal() +
                          theme(strip.text = element_text(face = "bold", color = "black")) +
                          facet_wrap(~dep_var,
                                     scales = "free_y",
                                     nrow = fig_nrow)
                        
                        ggsave(p,
                               filename = file.path(paper_figures, 
                                                    paste0(est_type_i,"_by4ntlgroups_",dataset_i,"_",addis_dist_i,trans_type_suffix,indep_var_type_i,controls_suffix_i,"_dmsp_viirs.png")),
                               height = fig_height, 
                               width = fig_width,
                               dpi = dpi)
                      }
                    }
                  }
                }
              }
            }
            
            # Figure: By Road Type -------------------------------------------------
            p <- df_sub %>%
              dplyr::filter(dataset %in% dataset_i,
                            addis_distance %in% addis_dist_i,
                            indep_var_type %in% indep_var_type_i,
                            ntl_num_groups %in% 4,
                            ntl_group == "all",
                            controls == controls_i,
                            est_type == est_type_i) %>%
              ggplot(aes(x = years_since_improved, 
                         y = b, 
                         ymin = p025, 
                         ymax=p975,
                         group = indep_var, 
                         color = indep_var)) +
              geom_point(position = position_dodge(width = p_dodge_width),size=1) + 
              geom_linerange(position = position_dodge(width = p_dodge_width),size=0.5) +
              geom_vline(xintercept=0,size=.5,alpha=0.5) +
              geom_hline(yintercept=0,size=.5,alpha=0.5) +
              labs(x="Years Since Road Improved",
                   y="Coefficient (+/- 95% CI)",
                   color="Road Type") +
              scale_alpha_manual(values = c(0.1, 1)) +
              scale_color_manual(values = c("dodgerblue1", "darkorange", "black"),
                                 guide = guide_legend(reverse = TRUE)) +
              theme_minimal() +
              theme(strip.text = element_text(face = "bold", color = "black")) +
              facet_wrap(~dep_var,
                         scales = "free_y",
                         nrow = fig_nrow)
            
            ggsave(p,
                   filename = file.path(paper_figures, 
                                        paste0(est_type_i,"_byroad_",dataset_i,"_",addis_dist_i,trans_type_suffix,indep_var_type_i,controls_suffix_i,".png")),
                   height = fig_height, 
                   width = fig_width,
                   dpi = dpi)
            
            # Figure: By 4 Speeds --------------------------------------------------
            p <- df_sub %>%
              dplyr::filter(dataset %in% dataset_i,
                            addis_distance %in% addis_dist_i,
                            indep_var_type %in% indep_var_type_i,
                            ntl_num_groups %in% 4,
                            ntl_group != "all",
                            indep_var %in% "All",
                            controls == controls_i,
                            est_type == est_type_i) %>%
              ggplot(aes(x = years_since_improved, 
                         y = b, 
                         ymin = p025, 
                         ymax=p975,
                         group = ntl_group, 
                         color = ntl_group)) +
              geom_point(position = position_dodge(width = p_dodge_width),size=1) + 
              geom_linerange(position = position_dodge(width = p_dodge_width),size=0.5) +
              geom_vline(xintercept=0,size=.5,alpha=0.5) +
              geom_hline(yintercept=0,size=.5,alpha=0.5) +
              labs(x="Years Since Road Improved",
                   y="Coefficient (+/- 95% CI)",
                   color="Baseline\nNighttime\nLights") +
              scale_alpha_manual(values = c(0.1, 1)) +
              scale_color_manual(values = c("gray20", "gray60", "darkorange", "firebrick2"),
                                 guide = guide_legend(reverse = TRUE)) +
              theme_minimal() +
              theme(strip.text = element_text(face = "bold", color = "black")) +
              facet_wrap(~dep_var,
                         scales = "free_y",
                         nrow = fig_nrow)
            
            ggsave(p,
                   filename = file.path(paper_figures, 
                                        paste0(est_type_i,"_by4ntlgroups_",dataset_i,"_",addis_dist_i,trans_type_suffix,indep_var_type_i,controls_suffix_i,".png")),
                   height = fig_height, 
                   width = fig_width,
                   dpi = dpi)
            
            # Figure: By 4 Speeds - FOR PPT (force SI params) ----------------------
            p <- df_sub %>%
              dplyr::filter(dataset %in% dataset_i,
                            addis_distance %in% addis_dist_i,
                            indep_var_type %in% indep_var_type_i,
                            ntl_num_groups %in% 4,
                            ntl_group != "all",
                            indep_var %in% "All",
                            controls == controls_i,
                            est_type == est_type_i) %>%
              ggplot(aes(x = years_since_improved, 
                         y = b, 
                         ymin = p025, 
                         ymax=p975,
                         group = ntl_group, 
                         color = ntl_group)) +
              geom_point(position = position_dodge(width = p_dodge_width_si),size=1) + 
              geom_linerange(position = position_dodge(width = p_dodge_width_si),size=0.5) +
              geom_vline(xintercept=0,size=.5,alpha=0.5) +
              geom_hline(yintercept=0,size=.5,alpha=0.5) +
              labs(x="Years Since Road Improved",
                   y="Coefficient (+/- 95% CI)",
                   color="Baseline\nNighttime\nLights") +
              scale_alpha_manual(values = c(0.1, 1)) +
              scale_color_manual(values = c("gray20", "gray60", "darkorange", "firebrick2"),
                                 guide = guide_legend(reverse = TRUE)) +
              theme_minimal() +
              theme(strip.text = element_text(face = "bold", color = "black")) +
              facet_wrap(~dep_var,
                         scales = "free_y",
                         nrow = fig_nrow_si)
            
            ggsave(p,
                   filename = file.path(paper_figures, 
                                        paste0(est_type_i,"_by4ntlgroups_",dataset_i,"_",addis_dist_i,trans_type_suffix,indep_var_type_i,controls_suffix_i,"_FOR_PPT.png")),
                   height = 4, 
                   width = 11.5,
                   dpi = dpi)
            
            # Figure: By 4 Speeds - FOR PPT (force SI params), NO CROP ----------------------
            p <- df_sub %>%
              dplyr::filter(dataset %in% dataset_i,
                            addis_distance %in% addis_dist_i,
                            indep_var_type %in% indep_var_type_i,
                            ntl_num_groups %in% 4,
                            ntl_group != "all",
                            indep_var %in% "All",
                            controls == controls_i,
                            est_type == est_type_i) %>%
              dplyr::filter(dep_var != "Dependent Variable: Cropland") %>%
              ggplot(aes(x = years_since_improved, 
                         y = b, 
                         ymin = p025, 
                         ymax=p975,
                         group = ntl_group, 
                         color = ntl_group)) +
              geom_point(position = position_dodge(width = p_dodge_width_si),size=1) + 
              geom_linerange(position = position_dodge(width = p_dodge_width_si),size=0.5) +
              geom_vline(xintercept=0,size=.5,alpha=0.5) +
              geom_hline(yintercept=0,size=.5,alpha=0.5) +
              labs(x="Years Since Road Improved",
                   y="Coefficient (+/- 95% CI)",
                   color="Baseline\nNighttime\nLights") +
              scale_alpha_manual(values = c(0.1, 1)) +
              scale_color_manual(values = c("gray20", "gray60", "darkorange", "firebrick2"),
                                 guide = guide_legend(reverse = TRUE)) +
              theme_minimal() +
              theme(strip.text = element_text(face = "bold", color = "black")) +
              facet_wrap(~dep_var,
                         scales = "free_y",
                         nrow = fig_nrow_si)
            
            ggsave(p,
                   filename = file.path(paper_figures, 
                                        paste0(est_type_i,"_by4ntlgroups_",dataset_i,"_",addis_dist_i,trans_type_suffix,indep_var_type_i,controls_suffix_i,"_nocrop_FOR_PPT.png")),
                   height = fig_height_si, 
                   width = 8,
                   dpi = dpi)
            
            # Figure: By 2 Speeds --------------------------------------------------
            
            # Don't create for diff-in-diff with 1km grid
            if(! ((est_type_i == "did") & (dataset_i == "dmspols_grid_nearroad")) ){
              
              p <- df_sub %>%
                dplyr::filter(dataset %in% dataset_i,
                              addis_distance %in% addis_dist_i,
                              indep_var_type %in% indep_var_type_i,
                              ntl_num_groups %in% 2,
                              ntl_group != "all",
                              indep_var %in% "All",
                              controls == controls_i,
                              est_type == est_type_i) %>%
                ggplot(aes(x = years_since_improved, 
                           y = b, 
                           ymin = p025, 
                           ymax=p975,
                           group = ntl_group, 
                           color = ntl_group)) +
                geom_point(position = position_dodge(width = p_dodge_width_si),size=1) + 
                geom_linerange(position = position_dodge(width = p_dodge_width_si),size=0.5) +
                geom_vline(xintercept=0,size=.5,alpha=0.5) +
                geom_hline(yintercept=0,size=.5,alpha=0.5) +
                labs(x="Years Since Road Improved",
                     y="Coefficient (+/- 95% CI)",
                     color="Baseline\nNighttime\nLights") +
                scale_alpha_manual(values = c(0.1, 1)) +
                scale_color_manual(values = c("gray20", "darkorange"),
                                   guide = guide_legend(reverse = TRUE)) +
                theme_minimal() +
                theme(strip.text = element_text(face = "bold", color = "black")) +
                facet_wrap(~dep_var,
                           scales = "free_y",
                           nrow = fig_nrow_si)
              
              ggsave(p,
                     filename = file.path(paper_figures, 
                                          paste0(est_type_i,"_by2ntlgroups_",dataset_i,"_",addis_dist_i,trans_type_suffix,indep_var_type_i,controls_suffix_i,".png")),
                     height = fig_height_si, 
                     width = fig_width_si,
                     dpi = dpi)
            }
            
            
            
          }
        }
      }
    }
  }
}

