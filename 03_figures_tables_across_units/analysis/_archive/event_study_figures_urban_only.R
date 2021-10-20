# Event Study Figures: Urban

p_dodge_width <- 0.8

data_grid <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_nearroad", "results_datasets",
                               "did_coef_every_year.Rds")) %>%
  filter(dep_var %in% c("globcover_urban"),
         addis_distance %in% "Far",
         ntl_group %in% "All") %>%
  mutate(indep_var = case_when(
    indep_var == "years_since_improvedroad" ~ "All",
    indep_var == "years_since_improvedroad_50aboveafter" ~ ">=50 km/hr",
    indep_var == "years_since_improvedroad_below50after" ~ "<50 km/hr"
  ))

p <- data_grid %>%
  ggplot(aes(x = years_since_improved, y = b, ymin = p025, ymax=p975,
             group = indep_var, color=indep_var)) +
  geom_point(position = position_dodge(width = p_dodge_width),size=1) + 
  geom_linerange(position = position_dodge(width = p_dodge_width),size=0.5) +
  geom_vline(xintercept=0,size=.5,alpha=0.5) +
  geom_hline(yintercept=0,size=.5,alpha=0.5) +
  labs(x="Years Since Road Improved",
       y="Coefficient",
       color="Road Type",
       title = "Urban Land") +
  scale_color_manual(values = c("dodgerblue1", "darkorange", "black"),
                     guide = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size=10),
        legend.position = "bottom") 

ggsave(p, filename = file.path("~/Desktop/urban.png"), height = 3, width = 5)




gs_rd <- st_read("~/Desktop/gs_road_polyline.geojson")
leaflet() %>% addTiles() %>% addPolylines(data = gs_rd)

