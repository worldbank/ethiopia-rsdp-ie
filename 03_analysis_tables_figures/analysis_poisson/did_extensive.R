# DiD - arcsinh vs extensive

ihs <- function(y) {
  return(log(y + sqrt(y^2 + 1)))
}

# Load data --------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_dir, "kebele", "merged_datasets", "panel_data_clean.Rds"))

data <- data %>%
  dplyr::mutate(dmspols_harmon_ihs100     = ihs(dmspols_harmon * 100),
                globcover_urban_ihs100    = ihs(globcover_urban * 100),
                globcover_cropland_ihs100 = ihs(globcover_cropland * 100),
                
                dmspols_harmon_bin = as.numeric(dmspols_harmon > 0),
                globcover_urban_bin = as.numeric(globcover_urban > 0),
                globcover_cropland_bin = as.numeric(globcover_cropland > 0))

did_df <- map_df(c("dmspols_harmon_ihs",
                   "globcover_urban_ihs",
                   "globcover_cropland_ihs",
                   
                   "dmspols_harmon_ihs100",
                   "globcover_urban_ihs100",
                   "globcover_cropland_ihs100",
                   
                   "dmspols_harmon_bin",
                   "globcover_urban_bin",
                   "globcover_cropland_bin"), function(depvar){
                     print(depvar)
                     
                     data$depvar <- data[[depvar]]
                     
                     example_attgt <- att_gt(yname = "depvar",
                                             tname = "year",
                                             idname = "cell_id",
                                             gname = "year_improvedroad",
                                             xformla = ~1,
                                             data = data,
                                             control_group = "notyettreated",
                                             base_period = "universal",
                                             clustervars = "woreda_id",
                                             print_details = T
                     )
                     
                     agg.simple.dynamic <- aggte(example_attgt, type = "dynamic", na.rm = TRUE)
                     
                     agg.simple.dynamic_df <- agg.simple.dynamic %>% 
                       tidy() %>% 
                       as.data.frame()
                     
                     agg.simple.dynamic_df$depvar <- depvar
                     agg.simple.dynamic_df$depvar_clean <- depvar %>%
                       str_replace_all("_bin|_ihs100|_ihs", "")
                     
                     if(depvar %>% str_detect("_bin")){
                       
                       agg.simple.dynamic_df$type <- "Extensive Margin"
                       
                     } else if(depvar %>% str_detect("_ihs100")){
                       
                       agg.simple.dynamic_df$type <- "arcsinh(100*Y)"
                       
                     } else{
                       
                       agg.simple.dynamic_df$type <- "arcsinh(Y)"
                       
                     }
                     
                     return(agg.simple.dynamic_df)
                     
                   })

did_df %>%
  dplyr::filter(event.time %in% -10:10) %>%
  dplyr::mutate(depvar_clean = case_when(
    depvar_clean == "dmspols_harmon" ~ "Nighttime Lights",
    depvar_clean == "globcover_cropland" ~ "Cropland",
    depvar_clean == "globcover_urban" ~ "Urban"
  ) %>%
    factor(levels = c("Nighttime Lights",
                      "Urban",
                      "Cropland"))) %>%
  dplyr::mutate(type = type %>%
                  factor(levels = rev(c("Extensive Margin",
                                    "arcsinh(100*Y)",
                                    "arcsinh(Y)")))) %>%
  ggplot(aes(x = event.time, 
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = type,
             group = type)) +
  geom_point(position = position_dodge(width = 0.6)) +
  geom_linerange(position = position_dodge(width = 0.6)) +
  geom_vline(xintercept=0,size=.5,alpha=0.5) +
  geom_hline(yintercept=0,size=.5,alpha=0.5) +
  facet_wrap(~depvar_clean,
             scales = "free_y",
             ncol = 1) +
  labs(x="Years Since Road Improved",
       y="Coefficient (+/- 95% CI)",
       color = "Dependent\nVariable\nTransformation") +
  scale_color_manual(values = c("black",
                                "dodgerblue",
                                "darkorange")) +
  theme_minimal() +
  theme()

ggsave(filename = file.path(paper_figures, "did_arcsinh_extensive.png"),
       height = 7, width = 8)


