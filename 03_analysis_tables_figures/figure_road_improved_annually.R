# Road Improved: Annually

# Load Data --------------------------------------------------------------------
road_df <- readRDS(file.path(rsdp_dir, "RawData", "RoadNetworkPanelData_1996_2016.Rds"))
road_df <- road_df %>% as.data.frame()

imp_df <- map_df(1996:2015, function(year_1){
  
  year_2 <- year_1 + 1
  
  road_df$speed_year_1 <- road_df[[paste0("Speed", year_1)]]
  road_df$speed_year_2 <- road_df[[paste0("Speed", year_2)]]
  
  road_df$year_1 <- year_1 
  road_df$year_2 <- year_2
  
  road_i_df <- road_df %>%
    dplyr::select(OBJECTID, LINKLENGTH, year_1, year_2, speed_year_1, speed_year_2)
  
  return(road_i_df)
})

imp_df <- imp_df %>%
  dplyr::filter(speed_year_2 > speed_year_1) 

# After improvement ------------------------------------------------------------
imp_agg_df <- imp_df %>%
  
  dplyr::mutate(category = case_when(
    speed_year_2 >= 50 ~ "Federal and Asphalt Roads\n[Road 50km/h or above\nafter improvement]\n ",
    speed_year_2 < 50 ~ "URRAP and Low to\nIntermediate\nClass Gravel Roads\n[Road below 50km/h\nafter improvement]"
  )) %>%
  
  group_by(year_2, category) %>%
  dplyr::summarise(LINKLENGTH = sum(LINKLENGTH)) %>%
  ungroup() %>%
  complete(year_2,
           category,
           fill = list(LINKLENGTH = 0))

p <- imp_agg_df %>%
  ggplot(aes(x = year_2,
             y = LINKLENGTH,
             color = category)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(x = NULL,
       y = "Length (km)",
       title = "Length of roads improved or constructed annually",
       color = NULL) +
  scale_color_manual(values = c("dodgerblue",
                                "darkorange")) +
  theme_classic2() +
  theme(plot.title = element_text(face = "bold"))

ggsave(p, 
       filename = file.path(paper_figures,
                            "road_improved_annually.png"),
       height = 4,
       width = 7)

