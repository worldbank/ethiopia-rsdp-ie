
# "road_length_X_speed", 

addis_dist <- "All"
iv_suffix  <- ""
type <- "length_over"
lm_type <- "ntl_base"

HEIGHT <- 7
WIDTH = 14

for(addis_dist in c("All", "Far")){
  for(iv_suffix in c("", "_area", "_neigh", "_neigh_area", "_neigh_withi", "_neigh_withi_area")){
    #for(type in c("length_over", "length_X_speed")){
    for(lm_type in c("level", "ntl_base")){
      
      results_df <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "results", "long_diff_first_diff.Rds"))
      
      #### Exclude variables always
      results_df <- results_df[!(results_df$iv %in% c(
        "road_length_10over",
        "road_length_15over",
        "road_length_20over",
        "road_length_70over",
        "road_length_120over")),]
      
      #### DVs to Use
      results_df <- results_df[results_df$dv %in% c("dmspols_zhang_ihs",
                                                    "dmspols_zhang_2",
                                                    "viirs_mean_ihs",
                                                    "viirs_mean_2",
                                                    "globcover_cropland",
                                                    "globcover_urban",
                                                    "ndvi",
                                                    "ndvi_cropland"),]
      
      #### Subset Addis Type
      results_df <- results_df[results_df$addis_dist %in% addis_dist,]
      
      results_df <- results_df[results_df$iv_suffix %in% iv_suffix,]
      
      results_df <- results_df[results_df$lm_type %in% lm_type,]
      
      #### Subset Type
      #if(type %in% "length_over"){
      #  results_df <- results_df[!(results_df$iv %in% "road_length_X_speed"),]
      #  HEIGHT = 4
      #  WIDTH = 14
      #} else{
      #  results_df <- results_df[(results_df$iv %in% "road_length_X_speed"),]
      #  HEIGHT = 1.5
      #  WIDTH = 14
      #}
      
      #### Rename Variable Type
      results_df$variable[results_df$variable %in% "road_var"] <- "log(Var_e) - log(Var_b)"
      results_df$variable[results_df$variable %in% "road_var:dmspols_1996_group_woreda2"] <- "log(Var_e) - log(Var_b) X NTL Low"
      results_df$variable[results_df$variable %in% "road_var:dmspols_1996_group_woreda3"] <- "log(Var_e) - log(Var_b) X NTL High"
      
      results_df$variable <- results_df$variable %>% factor(c("log(Var_e) - log(Var_b)",
                                                              "log(Var_e) - log(Var_b) X NTL Low",
                                                              "log(Var_e) - log(Var_b) X NTL High") %>% rev())
      
      #### Rename p-value group
      results_df$pvalue_5p <- ifelse(results_df$pvalue < 0.05, "Sig 5% Level", "Insign.")
      results_df$pvalue_5p[is.na(results_df$pvalue_5p)] <- "Insign."
      
      #### Rename Dependent variables
      results_df$dv[results_df$dv %in% "viirs_mean_ihs"] <- "VIIRS Mean (IHS)"
      results_df$dv[results_df$dv %in% "viirs_median_ihs"] <- "VIIRS Median (IHS)"
      results_df$dv[results_df$dv %in% "viirs_max_ihs"] <- "VIIRS Max (IHS)"
      results_df$dv[results_df$dv %in% "viirs_mean_2"] <- "VIIRS > 2"
      results_df$dv[results_df$dv %in% "viirs_mean_6"] <- "VIIRS > 6"
      results_df$dv[results_df$dv %in% "ndvi_cropland"] <- "NDVI - Cropland Area"
      results_df$dv[results_df$dv %in% "ndvi"] <- "NDVI"
      results_df$dv[results_df$dv %in% "globcover_urban"] <- "Urban"
      results_df$dv[results_df$dv %in% "globcover_cropland"] <- "Cropland"
      results_df$dv[results_df$dv %in% "dmspols_zhang_ihs"] <- "DMSP-OLS (IHS)"
      results_df$dv[results_df$dv %in% "dmspols_zhang_6"] <- "DMSPOLS > 6"
      results_df$dv[results_df$dv %in% "dmspols_zhang_2"] <- "DMSPOLS > 2"
      
      results_df$dv <- results_df$dv %>% factor(levels = c("DMSP-OLS (IHS)",
                                                           "DMSPOLS > 2",
                                                           "VIIRS Mean (IHS)",
                                                           "VIIRS > 2",
                                                           "Urban",
                                                           "Cropland",
                                                           "NDVI",
                                                           "NDVI - Cropland Area"))
      
      results_df$iv[results_df$iv %in% "road_length_25over"] <- "Length >= 25km/hr"
      results_df$iv[results_df$iv %in% "road_length_30over"] <- "Length >= 30km/hr"
      results_df$iv[results_df$iv %in% "road_length_35over"] <- "Length >= 35km/hr"
      results_df$iv[results_df$iv %in% "road_length_45over"] <- "Length >= 45km/hr"
      results_df$iv[results_df$iv %in% "road_length_50over"] <- "Length >= 50km/hr"
      results_df$iv[results_df$iv %in% "road_length_70over"] <- "Length >= 70km/hr"
      results_df$iv[results_df$iv %in% "road_length_120over"] <- "Length >= 120km/hr"
      results_df$iv[results_df$iv %in% "road_length_X_speed"] <- "Sum: Length X Speed"
      
      p_list <- lapply(1:2, function(i){
        
        if(i == 1) data <- results_df[!(results_df$iv %in% "Sum: Length X Speed"),]
        if(i == 2) data <- results_df[(results_df$iv %in% "Sum: Length X Speed"),]
        
        ggplot(data,
               aes(ymin = p025,
                   ymax = p975,
                   y=b,
                   pch=variable,
                   linetype=variable,
                   color=pvalue_5p,
                   x = iv)) +
          geom_linerange(position = position_dodge(width = 0.5)) +
          geom_point(position = position_dodge(width = 0.5)) +
          scale_color_manual(values=c("black", "red")) +
          coord_flip() +
          geom_hline(yintercept = 0, alpha=1, color="gray50") +
          labs(x="", y="",color="") +
          scale_y_continuous(breaks = trans_breaks(identity, identity, n = 4)) +
          guides(linetype = guide_legend(reverse=T),
                 pch = guide_legend(reverse=T)) +
          theme(axis.text.x = element_text(size= 7, angle = 0)) +
          facet_wrap(~dv, 
                     scales = "free_x",
                     nrow = 1) 
        
      })
      
      p <- ggarrange(p_list[[1]],
                     p_list[[2]],
                     common.legend = T,
                     ncol = 1,
                     heights = c(5,1.5),
                     legend = "right")
      
      if(DATASET_TYPE %in% "woreda_panel_hdx_csa_nearroad"){
        type <- "_nearroad"
      } else{
        type <- ""
      }
      
      ggsave(p, filename = file.path(figures_file_path, paste0("results_longdiff_first_diff_addisDist",addis_dist,"_iv_suffix",iv_suffix,"_lmType",lm_type,type,".png")),
             height = HEIGHT,
             width = WIDTH)
      
      print(paste0("results_longdiff_first_diff_addisDist",addis_dist,"_iv_suffix",iv_suffix,"_lmType",lm_type,type,".png"))
      
    }
  }
}

#}



# DATASET_TYPE <- "woreda_panel_hdx_csa"
