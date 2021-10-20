# Trends in Nighttime Lights and Urban

# Load/Prep Data ---------------------------------------------------------------
#### Globcover
gc_df <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_ethiopia", 
                                "individual_datasets", "globcover.Rds"))

gc_df <- gc_df %>%
  group_by(year) %>%
  dplyr::summarise(globcover_urban = sum(globcover_urban))

#### Globcover
ntl_df <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_ethiopia", 
                           "individual_datasets", "dmspols.Rds"))

ntl_df <- ntl_df %>%
  group_by(year) %>%
  dplyr::summarise(dmspols = sum(dmspols, na.rm=T))

# Figures ----------------------------------------------------------------------
ggplot() +
  geom_line(data = gc_df,
            aes(x = year, y = globcover_urban),
            size = 1.5,
            color = "red") +
  labs(x = NULL,
       y = expression("km"^2),
       title = "Area of Urban Land") +
  scale_x_continuous(breaks = seq(from = 1992, to=2018,by=4)) +
  theme(plot.background = element_rect(fill = "black",
                                       color = "black"),
        panel.background = element_rect(fill = "black",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "black"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", color = "white"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic", color = "white"),
        axis.text = element_text(color = "white"),
        axis.title.y = element_text(color = "white", angle = 0, vjust=0.5)) +
  ggsave(filename = file.path(panel_rsdp_imp_data_file_path, 
         "dmspols_grid_ethiopia",
         "outputs",
         "figures",
         "gc_urban_trends.png"),
         height = 2, width = 4)

# Figures ----------------------------------------------------------------------
ggplot() +
  geom_line(data = ntl_df,
            aes(x = year, y = dmspols),
            size = 1.5,
            color = "orange") +
  labs(x = NULL,
       y = NULL,
       title = "Nighttime Lights") +
  scale_x_continuous(breaks = seq(from = 1992, to=2018,by=4)) +
  scale_y_continuous(breaks = c(30000, 112000),
                     labels = c("Low", "High")) +
  theme(plot.background = element_rect(fill = "black",
                                       color = "black"),
        panel.background = element_rect(fill = "black",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "black"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", color = "white"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic", color = "white"),
        axis.text = element_text(color = "white"),
        axis.title.y = element_text(color = "white", angle = 0, vjust=0.5)) +
  ggsave(filename = file.path(panel_rsdp_imp_data_file_path, 
                              "dmspols_grid_ethiopia",
                              "outputs",
                              "figures",
                              "dmspols_trends.png"),
         height = 2, width = 4)



