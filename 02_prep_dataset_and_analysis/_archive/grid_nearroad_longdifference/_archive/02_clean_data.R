# Create Long Difference Dataset

calc_ihs <- function(x) log(x + sqrt(x^2 + 1))

data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data.Rds"))
data <- data[data$distance_anyroad2016 < 5000,]
data <- data[data$distance_anyroad2016 > 1000,]

data$near_anyimproved_ever <- as.numeric(data$distance_anyimproved_ever < 5000)
data$near_improved_bf2013_ever <- as.numeric(data$distance_improved_bf2013_ever < 5000)
data$near_mst <- as.numeric(data$distance_mst < 5000)

data$near_anyimproved_ever[data$year %in% 1996] <- 0
data$near_improved_bf2013_ever[data$year %in% 1996] <- 0
data$near_mst[data$year %in% 1996] <- 0

data$woreda_hdx_w_uid <- data$woreda_hdx_w_uid %>% as.factor()

data$dmspols_ihs <- calc_ihs(data$dmspols)

data <- data[,c("near_anyimproved_ever",
                "near_improved_bf2013_ever",
                "near_mst",
                "globcover_urban",
                "globcover_cropland",
                "ndvi",
                "ndvi_cropland",
                "dmspols_zhang",
                "dmspols",
                "dmspols_ihs",
                "temp_avg",
                "precipitation",
                "year",
                "cell_id",
                "woreda_hdx_w_uid")]

# Non First Diff ---------------------------------------------------------------
data_1996 <- data %>%
  as.data.frame() %>%
  filter(year == 1996) %>%
  group_by(cell_id) %>%
  summarise(temp_avg = mean(temp_avg, na.rm=T),
         precipitation = mean(precipitation, na.rm = T))

# First difference datasets ----------------------------------------------------
data_dmspols <- data %>%
  as.data.frame() %>%
  dplyr::filter(year %in% c(1996, 2012)) %>%
  mutate(endline = as.numeric(year == 2012)) %>%
  arrange(endline) %>%
  mutate(year = year %>% as.character()) %>%
  group_by(cell_id) %>%
  mutate_if(is.numeric, diff) %>%
  filter(year == "1996") %>%
  dplyr::select(cell_id, woreda_hdx_w_uid, 
                near_anyimproved_ever, near_improved_bf2013_ever, near_mst,
                temp_avg, precipitation,
                dmspols, dmspols_zhang, dmspols_ihs) %>%
  dplyr::rename(dmspols_2013diff = dmspols,
                dmspols_ihs_2013diff = dmspols_ihs,
                dmspols_zhang_2013diff = dmspols_zhang,
                temp_avg_2013diff = temp_avg,
                precipitation_2013diff = precipitation)

data_full <- data %>%
  as.data.frame() %>%
  dplyr::select(year, cell_id, temp_avg, precipitation, globcover_urban, globcover_cropland, ndvi_cropland, ndvi) %>%
  dplyr::filter(year %in% c(1996, 2018)) %>%
  mutate(endline = as.numeric(year == 2018)) %>%
  arrange(endline) %>%
  mutate(year = year %>% as.character()) %>%
  group_by(cell_id) %>%
  mutate_if(is.numeric, diff) %>%
  filter(year == "1996") %>%
  dplyr::rename(globcover_urban_2018diff = globcover_urban,
                globcover_cropland_2018diff = globcover_cropland,
                ndvi_cropland_2018diff = ndvi_cropland,
                ndvi_2018diff = ndvi,
                temp_avg_2018diff = temp_avg,
                precipitation_2018diff = precipitation) %>%
  dplyr::select(-c(year, endline))

## Merge
data_all <- merge(data_dmspols, data_full, by = "cell_id")
data_all <- merge(data_all, data_1996, by = "cell_id")

# Export -----------------------------------------------------------------------
data_all$woreda_hdx_w_uid <- data_all$woreda_hdx_w_uid %>% as.character() %>% as.numeric()

saveRDS(data_all, file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "long_diff_data.Rds"))
write_dta(data_all, file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "long_diff_data.dta"))


