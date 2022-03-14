# Trends in Road Variables using Woredas

# Woreda Data ------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_dir, "woreda", "merged_datasets", "panel_data_clean.Rds"))

# Prep Data --------------------------------------------------------------------
data_yr <- data %>%
  group_by(year, wor_ntlgroup_4bin) %>%
  dplyr::summarise_at(vars(contains("road_length_")), sum, na.rm=T) %>%
  ungroup() %>%
  dplyr::select(year, 
                wor_ntlgroup_4bin,
                road_length_10,
                road_length_15,
                road_length_20,
                road_length_25,
                road_length_30,
                road_length_35,
                road_length_45,
                road_length_50,
                road_length_70,
                road_length_120,
                road_length_10above) %>%
  filter(year >= 1996,
         year <= 2016) %>%
  pivot_longer(cols = -c(year, wor_ntlgroup_4bin, road_length_10above)) %>%
  mutate(prop_network = value / road_length_10above) %>%
  mutate(name = name %>% str_replace_all("road_length_", "")) %>%
  mutate(wor_ntlgroup_4bin_cat = case_when(
    wor_ntlgroup_4bin == 1 ~ "Dark: Max NTL in Woreda, 0",
    wor_ntlgroup_4bin == 2 ~ "Low: Max NTL in Woreda, 1 - 5",
    wor_ntlgroup_4bin == 3 ~ "Medium: Max NTL in Woreda, 6 - 8",
    wor_ntlgroup_4bin == 4 ~ "High: Max NTL in Woreda, 9+"
  )) %>%
  mutate(wor_ntlgroup_4bin_cat = wor_ntlgroup_4bin_cat %>%
           factor(levels = c("Dark: Max NTL in Woreda, 0", 
                             "Low: Max NTL in Woreda, 1 - 5", 
                             "Medium: Max NTL in Woreda, 6 - 8", 
                             "High: Max NTL in Woreda, 9+"))) %>%
  dplyr::mutate(name = name %>% 
                  as.character() %>% 
                  factor(levels = c("10", "15", "20", "25", "30", "35", "45", "50", "70", "120")))

# [Figure] All Speeds ----------------------------------------------------------
p <- data_yr %>%
  ggplot() +
  geom_col(aes(x = year, 
               y = prop_network, 
               group = name, 
               fill = name),
           width = 1) +
  facet_wrap(~wor_ntlgroup_4bin_cat) +
  theme_minimal() +
  labs(x = NULL,
       y = "Proportion of Road Network",
       fill = "Speed\n(km/h)",
       title = "Length of Roads by Estiamted Speed Limit",
       subtitle = "Across Woredas by Baseline Nightime Lights") +
  scale_fill_manual(values = wes_palette("FantasticFox1", n = 10, type = "continuous"))

ggsave(p, filename = file.path(paper_figures, "road_speed_byYear_byWoredaNTL.png"),
       height = 4, width = 6)


