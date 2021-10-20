# Exploratory Analysis

# APPROACH: Somewhat follow the Haiti paper. There it seems road improvement
# kinda random throughout their years. Here that's not the case, but maybe
# can assume that within an RDSP phase?

road_years_group <- "all"
dv <- "viirs_mean"
addis_distance <- "Far"
cluster_var <- "woreda_hdx_z_code"
time_period <- "all"

lm_results_all_df <- data.frame(NULL)

if(DATASET_TYPE %in% "woreda_panel_hdx_csa"){
  unit <- "woreda"
} else{
  unit <- "grid"
}

# Load Data --------------------------------------------------------------------
for(road_years_group in c("all", 
                          "dmspols",
                          "viirs",
                          "phase1",
                          "phase2",
                          "phase3",
                          "phase4")){
  for(addis_distance in c("All", "Far")){
    
    #### Where are we?
    print(paste(road_years_group, "============================================"))
    
    #### Load year_group dataset
    results_df <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "results", paste0("results_coef_post_yeargroup",road_years_group,".Rds")))
    
    #### Prep Dataframe
    # Remove control variables
    results_df <- results_df[!(results_df$variable %in% c("temp_avg", "precipitation")),]
    
    # Select variables
    if(road_years_group %in% c("viirs", "phase4")){
      results_df <- results_df[results_df$dv %in% c("viirs_mean_ihs",
                                                    "viirs_mean_2",
                                                    "globcover_urban",
                                                    "globcover_cropland",
                                                    "ndvi",
                                                    "ndvi_cropland"),]
      
    } else {
      
      results_df <- results_df[results_df$dv %in% c("dmspols_zhang_ihs",
                                                    "dmspols_zhang_2",
                                                    "globcover_urban",
                                                    "globcover_cropland",
                                                    "ndvi",
                                                    "ndvi_cropland"),]
    }
    
    #### P-Value Group
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
    
    #### Rename Independent Variables
    results_df$variable[results_df$variable %in% "pre_improvedroad_neg6_10"] <- "Imp. Road [6-10 yr lag]"
    results_df$variable[results_df$variable %in% "pre_improvedroad_neg2_5"] <- "Imp. Road [2-5 yr lag]"
    results_df$variable[results_df$variable %in% "post_improvedroad"] <- "Imp. Road"
    results_df$variable[results_df$variable %in% "post_improvedroad:dmspols_zhang_1996_group_woreda2"] <- "Imp. Road X NTL Low"
    results_df$variable[results_df$variable %in% "post_improvedroad:dmspols_zhang_1996_group_woreda3"] <- "Imp. Road X NTL High"
    results_df$variable[results_df$variable %in% "post_improvedroad:region_typeDense"] <- "Imp. Road X Dense Region"
    
    results_df$variable[results_df$variable %in% "pre_improvedroad_50aboveafter_neg6_10"] <- "Imp. Road >=50 [6-10 yr lag]"
    results_df$variable[results_df$variable %in% "pre_improvedroad_50aboveafter_neg2_5"] <- "Imp. Road >=50 [2-5 yr lag]"
    results_df$variable[results_df$variable %in% "post_improvedroad_50aboveafter"] <- "Imp. Road >=50"
    results_df$variable[results_df$variable %in% "post_improvedroad_50aboveafter:dmspols_zhang_1996_group_woreda2"] <- "Imp. Road >=50 X NTL Low"
    results_df$variable[results_df$variable %in% "post_improvedroad_50aboveafter:dmspols_zhang_1996_group_woreda3"] <- "Imp. Road >=50 X NTL High"
    results_df$variable[results_df$variable %in% "post_improvedroad_50aboveafter:region_typeDense"] <- "Imp. Road >=50 X Dense Region"
    
    results_df$variable[results_df$variable %in% "pre_improvedroad_below50after_neg6_10"] <- "Imp. Road <50 [6-10 yr lag]"
    results_df$variable[results_df$variable %in% "pre_improvedroad_below50after_neg2_5"] <- "Imp. Road <50 [2-5 yr lag]"
    results_df$variable[results_df$variable %in% "post_improvedroad_below50after"] <- "Imp. Road <50"
    results_df$variable[results_df$variable %in% "post_improvedroad_below50after:dmspols_zhang_1996_group_woreda2"] <- "Imp. Road <50 X NTL Low"
    results_df$variable[results_df$variable %in% "post_improvedroad_below50after:dmspols_zhang_1996_group_woreda3"] <- "Imp. Road <50 X NTL High"
    results_df$variable[results_df$variable %in% "post_improvedroad_below50after:region_typeDense"] <- "Imp. Road <50 X Dense Region"
    
    results_df$variable <- results_df$variable %>% factor(levels = c("Imp. Road [6-10 yr lag]",
                                                                     "Imp. Road [2-5 yr lag]",
                                                                     "Imp. Road >=50 [6-10 yr lag]",
                                                                     "Imp. Road >=50 [2-5 yr lag]",
                                                                     "Imp. Road <50 [6-10 yr lag]",
                                                                     "Imp. Road <50 [2-5 yr lag]",
                                                                     
                                                                     "Imp. Road",
                                                                     "Imp. Road X NTL Low",
                                                                     "Imp. Road X NTL High",
                                                                     "Imp. Road X Dense Region",
                                                                     
                                                                     "Imp. Road >=50",
                                                                     "Imp. Road >=50 X NTL Low",
                                                                     "Imp. Road >=50 X NTL High",
                                                                     "Imp. Road >=50 X Dense Region",
                                                                     
                                                                     "Imp. Road <50",
                                                                     "Imp. Road <50 X NTL Low",
                                                                     "Imp. Road <50 X NTL High",
                                                                     "Imp. Road <50 X Dense Region") %>% rev())
    
    #### Subset for Figure
    
    results_i_df <- results_df
    
    results_i_df <- results_i_df[results_i_df$road_years_group %in% road_years_group,]
    results_i_df <- results_i_df[results_i_df$addis_distance %in% addis_distance,]
    results_i_df <- results_i_df[!is.na(results_i_df$variable),]
    
    
    make_figure <- function(data){
      data %>%
        ggplot(aes(ymin = p025,
                   ymax = p975,
                   y=b,
                   color=pvalue_5p,
                   x = variable)) +
        geom_linerange() +
        geom_point() +
        scale_color_manual(values=c("black", "red")) +
        coord_flip() +
        geom_hline(yintercept = 0) +
        labs(x="", y="",color="") +
        facet_wrap(~dv, 
                   scales = "free_x",
                   nrow = 1)
    }
    
    fig_lm <- results_i_df %>%
      filter(model_type == "lm") %>%
      make_figure()
  
    fig_lm_50 <- results_i_df %>%
      filter(model_type == "lm_50") %>%
      make_figure()
    
    
    
    fig_lm_baselineNTL <- results_i_df %>%
      filter(model_type == "lm_baselineNTL") %>%
      make_figure()
    
    fig_lm_50_baselineNTL <- results_i_df %>%
      filter(model_type == "lm_50_baselineNTL") %>%
      make_figure()
    
    fig_lm_region <- results_i_df %>%
      filter(model_type == "lm_region") %>%
      make_figure()
    
    fig_lm_50_region <- results_i_df %>%
      filter(model_type == "lm_50_region") %>%
      make_figure()
    
    #### NTL
    p_all <- ggarrange(fig_lm,
              fig_lm_baselineNTL,
              fig_lm_50,
              fig_lm_50_baselineNTL,
              fig_lm_region,
              fig_lm_50_region,
              ncol = 1,
              common.legend = T,
              legend = "bottom",
              heights = c(.175, .25, .25, .325, .25, .25))
    
    ggsave(p_all, filename = file.path(figures_file_path, paste0("regressions_post_addis",addis_distance,"_",unit,"_yeargroup",road_years_group,".png")),
           height = 16, width =11)
    print(paste0("regressions_post_addis",addis_distance,"_",unit,"_yeargroup",road_years_group,".png"))
    
    
    #### Region
    p_region <- ggarrange(fig_lm,
                       fig_lm_region,
                       fig_lm_50,
                       fig_lm_50_region,
                       ncol = 1,
                       common.legend = T,
                       legend = "bottom",
                       heights = c(.175, .25, .25, .325))
    
    ggsave(p_region, filename = file.path(figures_file_path, paste0("regressions_post_region_addis",addis_distance,"_",unit,"_yeargroup",road_years_group,".png")),
           height = 14, width =11)
    print(paste0("regressions_post_region_addis",addis_distance,"_",unit,"_yeargroup",road_years_group,".png"))
    
  }
}


