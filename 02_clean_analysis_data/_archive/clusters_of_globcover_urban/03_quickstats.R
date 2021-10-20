# Quick Summary Stats

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_globcover_urban", "merged_datasets", "panel_data_clean.Rds"))

# Number Clusters in Analysis --------------------------------------------------
## All improved roads
data %>%
  filter(!is.na(years_since_improvedroad)) %>%
  pull(cell_id) %>%
  unique() %>%
  length()

## Roads < 50km/hr
data %>%
  filter(!is.na(years_since_improvedroad_below50after)) %>%
  pull(cell_id) %>%
  unique() %>%
  length()

## Roads >= 50km/hr
data %>%
  filter(!is.na(years_since_improvedroad_50aboveafter)) %>%
  pull(cell_id) %>%
  unique() %>%
  length()

# Near All Roads ---------------------------------------------------------------
data %>%
  filter(distance_road <= 5000) %>%
  pull(cell_id) %>%
  unique() %>%
  length()

data %>%
  filter(distance_mst_mindist <= 5000) %>%
  pull(cell_id) %>%
  unique() %>%
  length()

data %>%
  filter(distance_mst <= 5000) %>%
  pull(cell_id) %>%
  unique() %>%
  length()

