# Trends in Road Variables using Woredas

# Woreda Data ------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_dir, "woreda", "merged_datasets", "panel_data_clean.Rds"))

# Prep Data --------------------------------------------------------------------
road_length_vars <- names(data) %>% str_subset("road_length_")
for(var in road_length_vars) data[[var]][ is.na(data[[var]]) ] <- 0
for(var in road_length_vars) data[[paste0(var,"_area")]] <- data[[var]] / data$area_polygon

data$road_length_10 <- data$road_length_10above - data$road_length_15above
data$road_length_15 <- data$road_length_15above - data$road_length_20above
data$road_length_20 <- data$road_length_20above - data$road_length_25above
data$road_length_25 <- data$road_length_25above - data$road_length_30above
data$road_length_30 <- data$road_length_30above - data$road_length_35above
data$road_length_35 <- data$road_length_35above - data$road_length_45above
data$road_length_45 <- data$road_length_45above - data$road_length_50above
data$road_length_50 <- data$road_length_50above - data$road_length_70above
data$road_length_70 <- data$road_length_70above - data$road_length_120above
data$road_length_120 <- data$road_length_120above

data_yr <- data %>%
  group_by(year, dmspols_harmon_1996_bin4) %>%
  dplyr::summarise_at(vars(contains("road_length")), mean) %>%
  ungroup()

# Data to Long -----------------------------------------------------------------
data_yr_long <- data_yr %>%
  pivot_longer(cols = -c(year, dmspols_harmon_1996_bin4)) %>%
  filter(year >= 1996,
         year <= 2016)

data_yr_long$type <- NA
data_yr_long$type[grepl("_area", data_yr_long$name)] <- "area"
data_yr_long$type[grepl("above$", data_yr_long$name)] <- "above"
data_yr_long$type[grepl("[[:digit:]]$", data_yr_long$name)] <- "length"

data_yr_long$speed <- data_yr_long$name %>%
  str_replace_all("road_length_", "") %>%
  str_replace_all("above_area", "") %>%
  str_replace_all("above", "") %>%
  as.numeric() %>%
  factor(levels = c("10", "15", "20", "25", "30", "35", "45", "50", "70", "120")) 

data_yr_long$dmspols_1996_bin4_cat <- NA
data_yr_long$dmspols_1996_bin4_cat[data_yr_long$dmspols_harmon_1996_bin4 %in% 1] <- "Dark: Max NTL in Woreda, 0"
data_yr_long$dmspols_1996_bin4_cat[data_yr_long$dmspols_harmon_1996_bin4 %in% 2] <- "Low: Max NTL in Woreda, 1 - 5"
data_yr_long$dmspols_1996_bin4_cat[data_yr_long$dmspols_harmon_1996_bin4 %in% 3] <- "Medium: Max NTL in Woreda, 6 - 8"
data_yr_long$dmspols_1996_bin4_cat[data_yr_long$dmspols_harmon_1996_bin4 %in% 4] <- "High: Max NTL in Woreda, 9+"

data_yr_long$dmspols_1996_bin4_cat <- data_yr_long$dmspols_1996_bin4_cat %>%
  factor(levels = c("Dark: Max NTL in Woreda, 01", "Low: Max NTL in Woreda, 1 - 5", "Medium: Max NTL in Woreda, 6 - 8", "High: Max NTL in Woreda, 9+"))

data_yr_long$dmspols_harmon_1996_bin4 <- data_yr_long$dmspols_harmon_1996_bin4 %>% as.factor()

# [Figure] All Speeds ----------------------------------------------------------
p <- data_yr_long %>%
  filter(type %in% "length") %>%
  ggplot() +
  geom_col(aes(x = year, y = value, group = speed, fill = speed),
           width = 1) +
  facet_wrap(~dmspols_1996_bin4_cat) +
  theme_minimal() +
  labs(x = NULL,
       y = "Proportion of Road Network",
       fill = "Speed\n(km/hr)",
       title = "Length of Roads by Estiamted Speed Limit",
       subtitle = "Across Woredas by Baseline Nightime Lights") +
  scale_fill_manual(values = wes_palette("FantasticFox1", n = 10, type = "continuous"))

ggsave(p, filename = file.path(paper_figures, "road_speed_byYear_byWoredaNTL.png"),
       height = 4, width = 6)

# [Figure] Above Select Speeds -------------------------------------------------
data_yr_long$speed_name <- paste0("Road Length \u2265 ", data_yr_long$speed, "km/hr")

p <- data_yr_long %>%
  filter(year >= 1997,
         year <= 2015,
         type %in% "above",
         speed %in% c(30, 50, 70)) %>%
  ggplot() +
  # geom_line(aes(x = year, y = value, 
  #               group = dmspols_1996_bin4),
  #           color = "black",
  #           size = 1) +
  geom_line(aes(x = year, y = value, 
                group = dmspols_harmon_1996_bin4, 
                color = dmspols_harmon_1996_bin4),
            size = 1.5) +
  labs(x = NULL,
       y = "Kilometers of Road",
       title = "Average Lengh of Road Above Different Speed Limits",
       subtitle = "Across Woredas with Different Baseline Nighttime Lights",
       color = "Maximum Value of\nNighttime Lights\nWithin Woreda") +
  scale_color_manual(values = c("black", "gold", "orange2", "red"),
                     labels = c("Dark: 0", "Low: 1-5", "Medium: 6-8", "High: 9+")) +
  facet_wrap(~speed_name,
             scale = "free_y",
             nrow = 1) 

ggsave(p, filename = file.path(paper_figures, "roadLengthAbove_bySpeed_byWoredaNTL.png"),
       height = 3, width = 10)

