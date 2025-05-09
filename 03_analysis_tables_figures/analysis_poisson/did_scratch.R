
#dep_var_vec <- c("globcover_urban_sum_ihs", "globcover_cropland_sum_ihs", "dmspols_harmon_ihs")

ihs <- function(y) {
  return(log(y + sqrt(y^2 + 1)))
}

# TWFE -------------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_dir, "kebele", "merged_datasets", "panel_data_clean.Rds"))

##
data <- data %>%
  dplyr::mutate(dmspols_harmon_ihs100 = ihs(dmspols_harmon * 100))

example_attgt_cont100 <- att_gt(yname = "dmspols_harmon_ihs100",
                                tname = "year",
                                idname = "cell_id",
                                gname = "year_improvedroad",
                                xformla = ~1,
                                data = data,
                                control_group = "notyettreated",
                                base_period = "universal",
                                clustervars = "woreda_id",
                                print_details = T
)

agg.simple.dynamic_cont100 <- aggte(example_attgt_cont100, type = "dynamic", na.rm = TRUE)

ggdid(agg.simple.dynamic_cont100)

##
example_attgt_cont <- att_gt(yname = "dmspols_harmon_ihs",
                             tname = "year",
                             idname = "cell_id",
                             gname = "year_improvedroad",
                             xformla = ~1,
                             data = data,
                             control_group = "notyettreated",
                             base_period = "universal",
                             clustervars = "woreda_id",
                             print_details = T
)

agg.simple.dynamic_cont <- aggte(example_attgt_cont, type = "dynamic", na.rm = TRUE)

ggdid(agg.simple.dynamic_cont)

##
data <- data %>%
  dplyr::mutate(dmspols_harmon_bin = as.numeric(dmspols_harmon > 0))

example_attgt_bin <- att_gt(yname = "dmspols_harmon_bin",
                            tname = "year",
                            idname = "cell_id",
                            gname = "year_improvedroad",
                            xformla = ~1,
                            data = data,
                            control_group = "notyettreated",
                            base_period = "universal",
                            clustervars = "woreda_id",
                            print_details = T
)

agg.simple.dynamic_bin <- aggte(example_attgt_bin, type = "dynamic", na.rm = TRUE)


## Plot on one
bin_df  <- agg.simple.dynamic_bin %>% tidy() %>% as.data.frame() %>%
  dplyr::mutate(type = "Extensive Margin") %>%
  as.data.frame()
cont_df <- agg.simple.dynamic_cont %>% tidy() %>% as.data.frame() %>%
  dplyr::mutate(type = "arcsinh(Y)") %>%
  as.data.frame()
cont100_df <- agg.simple.dynamic_cont100 %>% tidy() %>% as.data.frame() %>%
  dplyr::mutate(type = "arcsinh(100*Y)") %>%
  as.data.frame()

bind_rows(bin_df,
          cont_df,
          cont100_df) %>%
  dplyr::filter(event.time %in% -10:10) %>%
  ggplot(aes(x = event.time, 
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = type,
             group = type)) +
  geom_point(position = position_dodge(width = 0.6)) +
  geom_linerange(position = position_dodge(width = 0.6))





min(data$dmspols_harmon_ihs[data$dmspols_harmon_ihs > 0], na.rm = T)

model_ols  <- feols(dmspols_harmon_ihs ~ factor(years_since_improvedroad) | year + cell_id, data = data, conley(50))
model_pois <- fepois(dmspols_harmon_ihs    ~ factor(years_since_improvedroad) | year + cell_id, data = data, conley(50))

data$dmspols_harmon

##
t_model_ols <- tidy(model_ols, conf.int = TRUE) %>%
  dplyr::mutate(year = term %>%
                  str_replace_all("factor\\(years_since_improvedroad\\)", "") %>%
                  as.numeric())

ggplot(t_model_ols, aes(x = year, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Years Since Improved Road", y = "Coefficient Estimate",
       title = "Impact on Nighttime Lights by Years Since Road Improvement") +
  theme_minimal()

##
t_model_pois <- tidy(model_pois, conf.int = TRUE) %>%
  dplyr::mutate(year = term %>%
                  str_replace_all("factor\\(years_since_improvedroad\\)", "") %>%
                  as.numeric())

ggplot(t_model_pois, aes(x = year, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Years Since Improved Road", y = "Coefficient Estimate",
       title = "Impact on Nighttime Lights by Years Since Road Improvement") +
  theme_minimal()

# IC ---------------------------------------------------------------------------
#### Levels
data_ic <- readRDS(file.path(panel_rsdp_imp_dir, "kebele", "merged_datasets", "panel_data_clean.Rds"))

data_ic <- data_ic %>%
  dplyr::filter(year %in% c(1996, 2009),
                distance_rsdp123_targettedlocs > 5000) %>%
  dplyr::mutate(near_rsdp123 = as.numeric(distance_rsdp123 < 5000),
                distance_rsdp123_targettedlocs_log = log(distance_rsdp123_targettedlocs),
                post = as.numeric(year == 2009))

data_ic %>%
  group_by(year, near_rsdp123) %>%
  dplyr::summarise(dmspols_harmon_ihs = mean(dmspols_harmon_ihs)) %>%
  ungroup() %>%
  pivot_wider(names_from =year,
              id_cols = near_rsdp123,
              values_from = dmspols_harmon_ihs) %>%
  mutate(pc = (`2009` - `1996`)/`1996`)

(0.0770 - 0.0247)/0.0247
(0.198 - 0.0586)/0.0586

feols(dmspols_harmon_ihs ~ near_rsdp123:post | cell_id, 
      vcov = conley(50), data = data_ic)

fepois(dmspols_harmon_ihs ~ near_rsdp123:post + distance_rsdp123_targettedlocs_log | cell_id, 
       vcov = conley(50), data = data_ic)

#### Long-Difference 1
data <- readRDS(file.path(panel_rsdp_imp_dir, "kebele", "merged_datasets", "panel_data_clean.Rds"))

data <- data %>%
  dplyr::filter(year %in% c(1996, 2009)) 

data_1d <- data %>%
  arrange(year) %>%
  
  # First difference
  group_by(cell_id) %>%
  summarize_at(c("dmspols_harmon_ihs"), 
               diff) 

data_invar <- data %>%
  dplyr::filter(year == 1996) %>%
  dplyr::select(cell_id, distance_rsdp123_targettedlocs, distance_rsdp123,
                latitude, longitude)

data_ld <- data_1d %>%
  left_join(data_invar, by = "cell_id") %>%
  dplyr::filter(distance_rsdp123_targettedlocs > 5000) %>%
  dplyr::mutate(near_rsdp123 = as.numeric(distance_rsdp123 < 5000),
                distance_rsdp123_targettedlocs_log = log(distance_rsdp123_targettedlocs))

feols(dmspols_harmon_ihs ~ near_rsdp123 + distance_rsdp123_targettedlocs_log, vcov = conley(50), data = data_ld)

feols(dmspols_harmon_ihs ~ near_rsdp123, vcov = conley(50), data = data_ld)
feols(dmspols_harmon_ihs ~ near_rsdp123 - 1, vcov = conley(50), data = data_ld)

#### Long-Difference 2
data_ld <- readRDS(file.path(panel_rsdp_imp_dir, "kebele", "merged_datasets", 
                             paste0("longdiff_data_clean_base", 
                                    1996,
                                    "_end",
                                    2009,
                                    ".Rds")))

data_ld <- data_ld %>%
  dplyr::filter(distance_rsdp123_targettedlocs > 5000) %>%
  dplyr::mutate(distance_city_addisababa = distance_city_addisababa / 1000 / 100,
                near_rsdp123 = as.numeric(distance_rsdp123 < 5000),
                distance_rsdp123_targettedlocs_log = log(distance_rsdp123_targettedlocs))

model_ols <- feols(dmspols_harmon_ihs ~ near_rsdp123 + distance_rsdp123_targettedlocs_log, 
                   vcov = conley(50), data = data_ld)
model_ols

# Alice -------------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_dir, "kebele", "merged_datasets", "panel_data_clean.Rds"))

# Control variables
cntrl <- "far_addis"

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

# If we have controls
for (i in 1992:2017) {
  j <- i - 1991
  for (control in cntrl) {
    control_mean <- mean(data[[control]][data[[paste0("d_", j)]] == 1], na.rm = TRUE)
    data[[paste0(control, "_dm", j)]] <- data[[control]] - control_mean
  }
}

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

model <- fixest::fepois(formula(
  paste0("dmspols_harmon_ihs ~ ", f_x) %>% paste0(" | year")
), 
vcov = conley(50), 
data = data %>% filter(!is.na(dmspols_harmon)))

tidy(model, conf.int = TRUE) %>%
  separate(term, into = c("f", "d"), sep = ":") %>%
  dplyr::mutate(f = f %>% str_replace_all("f_", "") %>% as.numeric()) %>%
  dplyr::mutate(d = d %>% str_replace_all("d_", "") %>% as.numeric()) %>%
  dplyr::mutate(i = f - d) %>%
  dplyr::mutate(i = case_when(
    i > 10 ~ 10,
    i < -10 ~ -10,
    TRUE ~ i
  )) %>%
  group_by(i) %>%
  dplyr::summarise(estimate_mean = mean(estimate),
                   estimate_sd = sd(estimate),
                   n = n()) %>%
  ungroup() %>%
  dplyr::mutate(estimate_se = estimate_sd/sqrt(n),
                lb = estimate_mean - estimate_se*1.96,
                ub = estimate_mean + estimate_se*1.96) %>%
  dplyr::filter(!is.na(i)) %>%
  
  ggplot(aes(x = i,
             y = estimate_mean,
             ymin = lb,
             ymax = ub)) +
  geom_point() +
  geom_linerange()




t_model_ols <- tidy(model, conf.int = TRUE) %>%
  dplyr::mutate(year = term %>%
                  str_replace_all("factor\\(years_since_improvedroad\\)", "") %>%
                  as.numeric())

ggplot(t_model_ols, aes(x = year, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Years Since Improved Road", y = "Coefficient Estimate",
       title = "Impact on Nighttime Lights by Years Since Road Improvement") +
  theme_minimal()

