# DID Poisson

# Load data --------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_dir, "kebele", "merged_datasets", "panel_data_clean.Rds"))

# Prep formula -----------------------------------------------------------------

# Control variables
#cntrl <- "far_addis"

# Generate variable signaling the first treatment, d
for (i in 1992:2017) {
  j <- i - 1991
  data[[paste0("d_", j)]] <- as.numeric(data$year_improvedroad == i)
}

# Generate the time variable
for (i in 1992:2017) {
  j <- i - 1991
  data[[paste0("f_", j)]] <- as.numeric(data$year == i)
}

# Generate the time till the end of the period variable
data$f_till26 <- data$f_26
for (j in 25:1) {
  i <- j + 1
  data[[paste0("f_till", j)]] <- data[[paste0("f_till", i)]] + data[[paste0("f_", j)]]
}

# Generate the treatment dummy
data$w <- 0
for (i in 1992:2017) {
  j <- i - 1991
  o <- i - 1992
  data$w <- (data[[paste0("d_", j)]] * data[[paste0("f_till", j)]]) + data$w
}

# # If we have controls
# for (i in 1992:2017) {
#   j <- i - 1991
#   for (control in cntrl) {
#     control_mean <- mean(data[[control]][data[[paste0("d_", j)]] == 1], na.rm = TRUE)
#     data[[paste0(control, "_dm", j)]] <- data[[control]] - control_mean
#   }
# }

# Generate the interaction effects
f_str <- ""
d_str <- ""
f_x <- ""

for (i in 1:26) {
  f_str <- paste(f_str, paste0("f_", i), sep = " ")
  d_str <- paste(d_str, paste0("d_", i), sep = " ")
  
  for (j in 1:26) {
    if(j - i != -1){
      f_x <- paste(f_x, paste0("d_", i, ":", "f_", j), sep = "+")
    }
  }
}

# Create relative time variable
data$relative_time <- data$year_improvedroad - data$year

# Group relative time
# The Stata 'group' command creates a categorical variable
# In R, we can use factors and create a new variable with unique levels
data$RT <- as.numeric(factor(data$relative_time))

# Run model --------------------------------------------------------------------
model_dmsp <- fixest::fepois(formula(
  paste0("dmspols_harmon ~ ", f_x) %>% paste0(" | year")
), 
vcov = conley(50), 
data = data %>% filter(!is.na(dmspols_harmon)))

model_urban <- fixest::fepois(formula(
  paste0("globcover_urban ~ ", f_x) %>% paste0(" | year")
), 
vcov = conley(50), 
data = data %>% filter(!is.na(globcover_urban)))

model_cropland <- fixest::fepois(formula(
  paste0("globcover_cropland ~ ", f_x) %>% paste0(" | year")
), 
vcov = conley(50), 
data = data %>% filter(!is.na(globcover_cropland)))

# Figure -----------------------------------------------------------------------
model_all_df <- bind_rows(
  tidy(model_dmsp, conf.int = TRUE) %>% dplyr::mutate(depvar = "Nighttime Lights"),
  tidy(model_urban, conf.int = TRUE) %>% dplyr::mutate(depvar = "Urban"),
  tidy(model_cropland, conf.int = TRUE) %>% dplyr::mutate(depvar = "Cropland")
)

model_all_df %>%
  separate(term, into = c("f", "d"), sep = ":") %>%
  dplyr::mutate(f = f %>% str_replace_all("f_", "") %>% as.numeric()) %>%
  dplyr::mutate(d = d %>% str_replace_all("d_", "") %>% as.numeric()) %>%
  dplyr::mutate(i = f - d) %>%
  group_by(i, depvar) %>%
  dplyr::summarise(estimate_mean = mean(estimate),
                   estimate_sd = sd(estimate),
                   n = n()) %>%
  ungroup() %>%
  dplyr::mutate(estimate_se = estimate_sd/sqrt(n),
                lb = estimate_mean - estimate_se*1.96,
                ub = estimate_mean + estimate_se*1.96) %>%
  dplyr::filter(!is.na(i)) %>%
  dplyr::filter(i %in% -10:10) %>%
  
  ggplot(aes(x = i,
             y = estimate_mean,
             ymin = lb,
             ymax = ub)) +
  geom_point() +
  geom_linerange() +
  geom_vline(xintercept=0,size=.5,alpha=0.5) +
  geom_hline(yintercept=0,size=.5,alpha=0.5) +
  facet_wrap(~depvar) +
  labs(x="Years Since Road Improved",
       y="Coefficient (+/- 95% CI)") +
  theme_minimal() +
  theme()

ggsave(filename = file.path(paper_figures, "did_poisson.png"),
       height = 3, width = 8)


