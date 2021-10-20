# Analysis: Coefficient Each Year - Figures

# Makes figures based on dataframe of results

#### Settings
p_dodge_width <- .5
dv_list <- c("dmspols_zhang_ihs", "dmspols_zhang_2", "globcover_urban", "globcover_cropland", "ndvi", "ndvi_cropland")

if(DATASET_TYPE %in% "woreda_panel_hdx_csa"){
  unit <- "_woreda"
} else{
  unit <- "_grid"
}

# Load Data --------------------------------------------------------------------
for(road_years_group in c("all", 
                          "dmspols",
                          "viirs",
                          "phase1",
                          "phase2",
                          "phase3",
                          "phase4")){
  
  results_df <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "results", paste0("results_coef_each_year_yeargroup",road_years_group,".Rds")))
  results_df <- results_df[!(results_df$variable %in% c("temp_avg", "precipitation")),]
  
  # Facet over region type -------------------------------------------------------
  for(addis_distance in c("All", "Far")){
    for(ntl_group in c("All")){    
      
      print(paste(ntl_group, addis_distance))
      
      figures_list <- list()
      i <- 1
      for(dv in dv_list){
        
        #### Depending on year group, use DMSP or VIIRS
        # Defaults to DMSP, so make switch for VIIRS
        if(road_years_group %in% c("viirs", "phase4")){
          if(dv %in% "dmspols_zhang_ihs") dv <- "viirs_mean_ihs"
          if(dv %in% "dmspols_zhang_2")   dv <- "viirs_mean_2"
          if(dv %in% "dmspols_zhang_6")   dv <- "viirs_mean_6"
          
        }
        
        
        if(dv == "globcover_urban")    dv_title <- "Globcover: Urban"
        if(dv == "globcover_cropland") dv_title <- "Globcover: Cropland"
        if(dv == "ndvi")               dv_title <- "NDVI"
        if(dv == "dmspols_ihs")        dv_title <- "NTL - DMSPOLS (Log)"
        if(dv == "dmspols_zhang_ihs")  dv_title <- "NTL - DMSPOLS (Log)"
        if(dv == "dmspols_zhang_2")    dv_title <- "DMSPOLS >= 2"
        if(dv == "dmspols_zhang_6")    dv_title <- "DMSPOLS >= 6"
        if(dv == "ndvi")               dv_title <- "NDVI"
        if(dv == "ndvi_cropland")      dv_title <- "NDVI: Cropland Areas"
        
        if(dv == "viirs_mean_ihs")      dv_title <- "VIIRS: Mean (Log)"
        if(dv == "viirs_median")      dv_title <- "VIIRS: Median"
        if(dv == "viirs_mean")      dv_title <- "VIIRS: Mean"
        if(dv == "viirs_median")      dv_title <- "VIIRS: Median"
        if(dv == "viirs_max")      dv_title <- "VIIRS: Max"
        if(dv == "viirs_mean_2")      dv_title <- "VIIRS: Mean Above 2"
        if(dv == "viirs_mean_6")      dv_title <- "VIIRS: Mean Above 6"
        
        p <- ggplot(data = results_df[(results_df$dv %in% dv) & 
                                        (results_df$addis_distance %in% addis_distance) & 
                                        (results_df$ntl_group %in% ntl_group),], 
                    aes(x=years_since_improved, y=b, ymin=p025, ymax=p975,
                        group = var, color = var)) + 
          geom_vline(xintercept=0,size=3,alpha=0.15) +
          geom_hline(yintercept=0,size=1,alpha=0.15) +
          geom_point(position = position_dodge(width = p_dodge_width),size=1.5) + 
          geom_linerange(position = position_dodge(width = p_dodge_width),size=1) +
          labs(x="",
               y="Coefficient",
               color="Road\nType",
               title = dv_title) +
          theme_minimal() +
          theme(plot.title = element_text(face="bold", hjust=.5)) +
          facet_wrap(~region, scales="free", nrow=1)
        
        if(i == length(dv_list)){
          p <- p + labs(x="Years Since Improved Road Constructed")
        }
        
        figures_list[[i]] <- p
        
        i <- i + 1
      }
      
      p_all <- ggarrange(figures_list[[1]],
                         figures_list[[2]],
                         figures_list[[3]],
                         figures_list[[4]],
                         figures_list[[5]],
                         figures_list[[6]],
                         nrow = 6,
                         common.legend = T,
                         legend = "right")
      
      ggsave(p_all, filename = file.path(figures_file_path, paste0("regressions_eachyear_regionfacet_addis",addis_distance,"_ntl",ntl_group,unit,"_yeargroup",road_years_group,".png")),
             height = 14, width =11)
      print(file.path(figures_file_path, paste0("regressions_eachyear_regionfacet_addis",addis_distance,"_ntl",ntl_group,unit,"_yeargroup",road_years_group,".png")))
      
    }
  }
  
  
  # Facet over ntl group (excluding all) -----------------------------------------
  for(addis_distance in c("All", "Far")){
    for(region in c("All")){ 
      
      print(paste(region, addis_distance))
      
      results_df$ntl_group[results_df$ntl_group %in% "1"] <- "Zero"
      results_df$ntl_group[results_df$ntl_group %in% "2"] <- "Below Median"
      results_df$ntl_group[results_df$ntl_group %in% "3"] <- "Above Median"
      
      results_df$ntl_group <- results_df$ntl_group %>% factor(levels = c("All",
                                                                         "Zero",
                                                                         "Below Median",
                                                                         "Above Median"))
      
      
      figures_list <- list()
      i <- 1
      for(dv in dv_list){
        
        
        #### Depending on year group, use DMSP or VIIRS
        # Defaults to DMSP, so make switch for VIIRS
        if(road_years_group %in% c("viirs", "phase4")){
          if(dv %in% "dmspols_zhang_ihs") dv <- "viirs_mean_ihs"
          if(dv %in% "dmspols_zhang_2")   dv <- "viirs_mean_2"
          if(dv %in% "dmspols_zhang_6")   dv <- "viirs_mean_6"
          
        }
        
        if(dv == "globcover_urban")    dv_title <- "Globcover: Urban"
        if(dv == "globcover_cropland") dv_title <- "Globcover: Cropland"
        if(dv == "ndvi")               dv_title <- "NDVI"
        if(dv == "dmspols_ihs")        dv_title <- "NTL - DMSPOLS (Log)"
        if(dv == "dmspols_zhang_ihs")  dv_title <- "NTL - DMSPOLS (Log)"
        if(dv == "dmspols_zhang_2")    dv_title <- "DMSPOLS >= 2"
        if(dv == "dmspols_zhang_6")    dv_title <- "DMSPOLS >= 6"
        if(dv == "ndvi")               dv_title <- "NDVI"
        if(dv == "ndvi_cropland")      dv_title <- "NDVI: Cropland Areas"
        
        if(dv == "viirs_mean_ihs")      dv_title <- "VIIRS: Mean (Log)"
        if(dv == "viirs_median")      dv_title <- "VIIRS: Median"
        if(dv == "viirs_mean")      dv_title <- "VIIRS: Mean"
        if(dv == "viirs_median")      dv_title <- "VIIRS: Median"
        if(dv == "viirs_max")      dv_title <- "VIIRS: Max"
        if(dv == "viirs_mean_2")      dv_title <- "VIIRS: Mean Above 2"
        if(dv == "viirs_mean_6")      dv_title <- "VIIRS: Mean Above 6"
        
        p <- ggplot(data = results_df[(results_df$ntl_group %in% c("All", "Zero", "Below Median", "Above Median")) &
                                        (results_df$dv %in% dv) & 
                                        (results_df$addis_distance %in% addis_distance) & 
                                        (results_df$region %in% region),], 
                    aes(x=years_since_improved, y=b, ymin=p025, ymax=p975,
                        group = var, color = var)) + 
          geom_vline(xintercept=0,size=3,alpha=0.15) +
          geom_hline(yintercept=0,size=1,alpha=0.15) +
          geom_point(position = position_dodge(width = p_dodge_width),size=1.5) + 
          geom_linerange(position = position_dodge(width = p_dodge_width),size=1) +
          labs(x="",
               y="Coefficient",
               color="Road\nType",
               title = dv_title) +
          theme_minimal() +
          theme(plot.title = element_text(face="bold", hjust=.5)) +
          facet_wrap(~ntl_group, scales="free", nrow=1)
        
        
        if(i == length(dv_list)){
          p <- p + labs(x="Years Since Improved Road Constructed")
        }
        
        figures_list[[i]] <- p
        
        i <- i + 1
        
      }
      
      p_all <- ggarrange(figures_list[[1]],
                         figures_list[[2]],
                         figures_list[[3]],
                         figures_list[[4]],
                         figures_list[[5]],
                         figures_list[[6]],
                         nrow = 6,
                         common.legend = T,
                         legend = "right")
      
      ggsave(p_all, filename = file.path(figures_file_path, paste0("regressions_eachyear_ntlfacet_addis",addis_distance,"_region",region,unit,"_yeargroup",road_years_group,".png")),
             height = 14, width =14)
      print(file.path(figures_file_path, paste0("regressions_eachyear_ntlfacet_addis",addis_distance,"_region",region,unit,"_yeargroup",road_years_group,".png")))
      
    }
  }
  
  
  
}





