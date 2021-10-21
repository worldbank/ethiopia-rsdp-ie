# Analysis

# EXPLORING STUFF

data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "woreda", "merged_datasets", "panel_data_clean.Rds"))

data <- data %>%
  filter(year >= 1996)

#### PLOTS
data_sum <- data %>%
  group_by(year) %>%
  summarise_if(is.numeric, sum, na.rm=T) %>%
  filter(year >= 1996,
         year <= 2016)

data_sum %>%
  ggplot() +
  geom_line(aes(x = year, y=road_length_30above, color="30")) +
  geom_line(aes(x = year, y=road_length_45above, color="45")) +
  geom_line(aes(x = year, y=road_length_50above, color="50")) +
  geom_line(aes(x = year, y=road_length_70above, color="70")) 

#### REGRESSIONS
felm(dmspols_zhang_ihs ~ log(road_length_50above+1) | cell_id | 0 | 0, data = data) %>% summary()

