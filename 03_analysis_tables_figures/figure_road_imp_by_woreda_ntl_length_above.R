# Trends in Road Variables using Woredas

# Woreda Data ------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_dir, "woreda", "merged_datasets", "panel_data_clean.Rds"))

# Prep Data --------------------------------------------------------------------
data_yr <- data %>%
  group_by(year, wor_ntlgroup_4bin) %>%
  dplyr::summarise_at(vars(contains("road_length_")), mean, na.rm=T) %>%
  ungroup() %>%
  filter(year >= 1996,
         year <= 2016) %>%
  pivot_longer(cols = -c(year, wor_ntlgroup_4bin)) %>%
  dplyr::filter(name %>% str_detect("above")) %>%
  mutate(name = name %>% 
           str_replace_all("road_length_", "") %>%
           str_replace_all("above", "")) %>%
  mutate(wor_ntlgroup_4bin_cat = case_when(
    wor_ntlgroup_4bin == 1 ~ "Dark: 0",
    wor_ntlgroup_4bin == 2 ~ "Low: 1-5",
    wor_ntlgroup_4bin == 3 ~ "Medium: 6 - 8",
    wor_ntlgroup_4bin == 4 ~ "High: 9+"
  )) %>%
  mutate(wor_ntlgroup_4bin_cat = wor_ntlgroup_4bin_cat %>%
           factor(levels = c("Dark: 0", 
                             "Low: 1-5", 
                             "Medium: 6 - 8", 
                             "High: 9+")))

# [Figure] Above Select Speeds -------------------------------------------------
data_yr$name_full <- paste0("Road Length \u2265 ", data_yr$name, "km/h")

p <- data_yr %>%
  filter(name %in% c("30", "50", "70")) %>%
  ggplot() +
  geom_line(aes(x = year, 
                y = value, 
                group = wor_ntlgroup_4bin_cat, 
                color = wor_ntlgroup_4bin_cat),
            size = 1.5) +
  labs(x = NULL,
       y = "Kilometers of Road",
       title = "Average Lengh of Road Above Different Speed Limits",
       subtitle = "Across Woredas with Different Baseline Nighttime Lights",
       color = "Maximum Value of\nNighttime Lights\nWithin Woreda") +
  scale_color_manual(values = c("black", "gold", "orange2", "red"),
                     labels = c("Dark: 0", "Low: 1-5", "Medium: 6-8", "High: 9+")) +
  facet_wrap(~name_full,
             scale = "free_y",
             nrow = 1) 

ggsave(p, filename = file.path(paper_figures, "roadLengthAbove_bySpeed_byWoredaNTL.png"),
       height = 3, width = 10)

