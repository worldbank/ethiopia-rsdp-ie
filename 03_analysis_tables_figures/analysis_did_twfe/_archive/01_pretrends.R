# Analysis: Coefficient Each Year - Results

# Exports dataframe of results, to be used to make figures

#### Parameters
OVERWRITE_FILES <- F

#### Default
# dataset <- "dmspols_grid_nearroad"
# dep_var <- "globcover_urban"
# indep_var <- "years_since_improvedroad"
# controls <- ""
# addis_distance <- "All"
# ntl_group <- "All"

# Load data --------------------------------------------------------------------
dataset <- "kebele"

data <- readRDS(file.path(panel_rsdp_imp_dir, dataset, "merged_datasets", 
                          "panel_data_clean.Rds"))

data_sum <- data %>%
  dplyr::filter(!is.na(years_since_improvedroad)) %>%
  group_by(years_since_improvedroad) %>%
  dplyr::summarise_at(vars(globcover_urban_sum_ihs,
                           globcover_cropland_sum_ihs,
                           dmspols_harmon_ihs), mean,
                      na.rm = T) %>%
  ungroup() %>%
  pivot_longer(cols = -years_since_improvedroad) %>%
  mutate(name = case_when(
    name == "globcover_urban_sum_ihs" ~ "Urban",
    name == "globcover_cropland_sum_ihs" ~ "Cropland",
    name == "dmspols_harmon_ihs" ~ "NTL"
  )) %>%
  mutate(years_since_improvedroad = years_since_improvedroad %>%
           as.character() %>%
           as.numeric())


data_sum %>%
  ggplot() +
  geom_line(aes(x = years_since_improvedroad,
                y = value)) + 
  geom_vline(xintercept = 0, color = "red") +
  facet_wrap(~name,
             scales = "free")









