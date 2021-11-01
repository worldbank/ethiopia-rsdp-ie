# Analysis: Coefficient Each Year - Results

# Exports dataframe of results, to be used to make figures

# https://cran.r-project.org/web/packages/did/vignettes/did-basics.html

#### Parameters
OVERWRITE_FILES <- T

# Load Data --------------------------------------------------------------------
#dataset <- "clusters_of_ntlall"
#dep_var <- "dmspols_harmon_ihs"
#indep_var <- "year_improvedroad_50aboveafter"
#ntl_group <- "high"

for(dataset in c("kebele")){ # "dmspols_grid_nearroad"
  
  # Define Dependent Variables -------------------------------------------------
  if(dataset %in% "kebele"){
    dep_var_vec <- c("globcover_urban_sum_ihs", "globcover_cropland_sum_ihs", "dmspols_harmon_ihs")
  }           
  
  if(dataset %in% "dmspols_grid_nearroad"){
    dep_var_vec <- c("globcover_urban", "globcover_cropland", "dmspols_harmon_ihs")
  } 
  
  for(dep_var in dep_var_vec){
    for(indep_var in c("year_improvedroad",
                       "year_improvedroad_50aboveafter",
                       "year_improvedroad_below50after")){
      for(ntl_num_groups in c(2,4)){
        
        if(ntl_num_groups %in% 2) ntl_group_vec <- c("all", "1", "2")
        if(ntl_num_groups %in% 4) ntl_group_vec <- c("all", "1", "2", "3", "4")
        
        for(ntl_group in ntl_group_vec){
          
          # For road type, only calculate for all groups
          if((indep_var %in% c("year_improvedroad_50aboveafter", "year_improvedroad_below50after")) &
             (ntl_group != "all")){
            next
          }
          
          print(paste(dataset, dep_var, indep_var, ntl_group, sep = " - "))
          
          ## Load/Prep Data
          data <- readRDS(file.path(panel_rsdp_imp_dir, dataset, "merged_datasets", "panel_data_clean.Rds"))
          
          if(ntl_num_groups %in% 2){
            #TODO: This variable doesn't exist
            data$ntl_group <- data$wor_ntlgroup_2bin
          } else{
            data$ntl_group <- data$wor_ntlgroup_4bin
          }
          
          cluster_var <- "woreda_id"
          
          data$dep_var   <- data[[dep_var]]
          data$indep_var <- data[[indep_var]]
          
          ## If by ntl_group, subset by group
          if(ntl_group %in% "1") data <- data[data$ntl_group %in% 1,]
          if(ntl_group %in% "2") data <- data[data$ntl_group %in% 2,]
          if(ntl_group %in% "3") data <- data[data$ntl_group %in% 3,]
          if(ntl_group %in% "4") data <- data[data$ntl_group %in% 4,]
          
          ## Subset
          data = data %>%
            ungroup() %>%
            dplyr::filter(year >= 1992,
                          year <= 2018,
                          !is.na(indep_var))
          
          # This way of selecting specific variables is robust to some names (ie, woreda_id)
          # not being in all the datasets
          data <- data[,names(data) %in% c("dep_var", "indep_var", "cell_id", "year", "woreda_id")]
          
          ## Title
          dataset_name <- case_when(dataset %in% "kebele"                      ~ "Kebele",
                                    dataset %in% "dmspols_grid_nearroad"       ~ "1x1km Grid")
          
          indepvar_name <- case_when(indep_var %in% "year_improvedroad"              ~ "All Roads",
                                     indep_var %in% "year_improvedroad_50aboveafter" ~ "Roads >= 50km/hr After Upgrade",
                                     indep_var %in% "year_improvedroad_below50after" ~ "Roads < 50km/hr After Upgrade")
          
          depvar_name <- case_when(dep_var %in% "globcover_urban_sum_ihs" ~ "Urban",
                                   dep_var %in% "globcover_urban" ~ "Urban",
                                   dep_var %in% "dmspols_harmon_ihs" ~ "NTL",
                                   dep_var %in% "dmspols_harmon_ihs2013" ~ "NTL [2013]",
                                   dep_var %in% "globcover_cropland_sum_ihs" ~ "Cropland",
                                   dep_var %in% "globcover_cropland" ~ "Cropland")
          
          ntl_group_name <- case_when(ntl_group %in% "low" ~ "Low",
                                      ntl_group %in% "high" ~ "High",
                                      ntl_group %in% "all" ~ "All")
          
          title <- paste0("Dep Var: ",
                          depvar_name,
                          ";  Indep Var: ",
                          indepvar_name,
                          ";  NTL Group: ",
                          ntl_group_name,
                          ";  Dataset: ",
                          dataset_name,
                          ";  N Groups: ",
                          ntl_num_groups)
          
          example_attgt <- att_gt(yname = "dep_var",
                                  tname = "year",
                                  idname = "cell_id",
                                  gname = "indep_var",
                                  xformla = ~1,
                                  data = data,
                                  control_group = "notyettreated",
                                  clustervars = cluster_var,
                                  print_details = T
          )
          
          ## Aggregate ATTs
          agg.simple.dynamic <- aggte(example_attgt, type = "dynamic")
          p_dynamic <- ggdid(agg.simple.dynamic)
          
          agg.simple.group <- aggte(example_attgt, type = "group")
          p_group <- ggdid(agg.simple.group)
          
          OUT_PATH_SUFFIX <- paste0(dataset, "_", dep_var, "_", indep_var, 
                                    "_numgroups", ntl_num_groups, "_group", ntl_group)
          
          ## Save Figure
          p_all <- ggarrange(p_dynamic, 
                             p_group,
                             nrow = 1)
          
          p_all <- annotate_figure(p_all,
                                   top = text_grob(title, color = "black", face = "bold", size = 12))
          
          # TODO: Send figures to another place; or maybe just remove this? Remove later on!!
          ggsave(p_all, filename = file.path(paper_figures,
                                             paste0("did_attgt_", OUT_PATH_SUFFIX, ".png")),
                 height = 3, width = 10)
          
          #### Save Data
          ## Dynamic
          dynamic_df <- data.frame(time               = agg.simple.dynamic$egt,
                                   att                = agg.simple.dynamic$att.egt,
                                   se                 = agg.simple.dynamic$se.egt,
                                   critical_value_95p = as.numeric(agg.simple.dynamic$crit.val.egt)) %>%
            mutate(dataset = dataset,
                   dep_var = dep_var,
                   indep_var = indep_var,
                   ntl_group = ntl_group)
          
          saveRDS(dynamic_df, 
                  file.path(panel_rsdp_imp_dir,
                            "all_units",
                            "results_datasets",
                            "individual_datasets",
                            paste0("dynamic_did_attgt_",OUT_PATH_SUFFIX, ".Rds")))
          
          ## Group
          group_df <- data.frame(group              = agg.simple.group$egt,
                                 att                = agg.simple.group$att.egt,
                                 se                 = agg.simple.group$se.egt,
                                 critical_value_95p = as.numeric(agg.simple.group$crit.val.egt)) %>%
            mutate(dataset = dataset,
                   dep_var = dep_var,
                   indep_var = indep_var,
                   ntl_group = ntl_group)
          
          saveRDS(group_df, 
                  file.path(panel_rsdp_imp_dir,
                            "all_units",
                            "results_datasets",
                            "individual_datasets",
                            paste0("group_did_attgt_",OUT_PATH_SUFFIX, ".Rds")))
          
        }
      }
    }
  }
}
