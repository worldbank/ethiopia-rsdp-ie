# Number of Projects Near

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data_clean.Rds"))

data_base <- data %>%
  summarise(dmspols_zhang = mean(dmspols_zhang[year == 1996], na.rm=T),
            globcover_urban = mean(globcover_urban[year == 1996], na.rm=T),
            globcover_cropland = mean(globcover_cropland[year == 1996], na.rm=T),
            ndvi_cropland = mean(ndvi_cropland[year == 2000], na.rm=T),
            viirs_mean = mean(viirs_mean[year == 2012], na.rm=T)) %>%
  t() %>%
  as.data.frame() %>%
  dplyr::rename(base = V1)
data_base$variable <- row.names(data_base)

data_end <- data %>%
  summarise(dmspols_zhang = mean(dmspols_zhang[year == 2012], na.rm=T),
            globcover_urban = mean(globcover_urban[year == 2018], na.rm=T),
            globcover_cropland = mean(globcover_cropland[year == 2018], na.rm=T),
            ndvi_cropland = mean(ndvi_cropland[year == 2015], na.rm=T),
            viirs_mean = mean(viirs_mean[year == 2019], na.rm=T)) %>%
  t() %>%
  as.data.frame() %>%
  dplyr::rename(end = V1)
data_end$variable <- row.names(data_end)

data_sum <- merge(data_base, data_end, by = "variable")
data_sum$variable[data_sum$variable %in% "dmspols_zhang"] <- "NTL: DMSP-OLS (Average)"
data_sum$variable[data_sum$variable %in% "globcover_cropland"] <- "Cropland (Proportion)"
data_sum$variable[data_sum$variable %in% "globcover_urban"] <- "Urban (Proportion)"
data_sum$variable[data_sum$variable %in% "ndvi_cropland"] <- "NDVI in Cropland Areas (Average)"
data_sum$variable[data_sum$variable %in% "viirs_mean"] <- "NTL: VIIRS (Average)"

data_sum$base <- data_sum$base %>% round(3)
data_sum$end <- data_sum$end %>% round(3)

names(data_sum) <- c("Variable", "Baseline", "Endline")

data_sum$tex <- paste(data_sum$Variable, " & ",
                      data_sum$Baseline, " & ",
                      data_sum$Endline, " \\\\ ")

sink(file.path(tables_file_path, "sum_stat_dv_wards.tex"))
cat("\\begin{tabular}{lcc} ")
cat("\\hline ")
cat("Variable & Baseline Value & Endline Value \\\\ ")
cat("\\hline ")
for(i in 1:nrow(data_sum)){
  cat(data_sum$tex[i])
}

cat("\\hline ")
cat("\\end{tabular} ")
sink()

## Trends
data_sum_yr <- data %>%
  group_by(year) %>%
  summarise(dmspols_zhang = mean(dmspols_zhang, na.rm=T),
            globcover_urban = mean(globcover_urban, na.rm=T),
            globcover_cropland = mean(globcover_cropland, na.rm=T),
            ndvi_cropland = mean(ndvi_cropland, na.rm=T),
            viirs_mean = mean(viirs_mean, na.rm=T)) %>%
  pivot_longer(-year)

data_sum_yr$name[data_sum_yr$name %in% "dmspols_zhang"] <- "NTL: DMSP-OLS (Average)"
data_sum_yr$name[data_sum_yr$name %in% "globcover_cropland"] <- "Cropland (Proportion)"
data_sum_yr$name[data_sum_yr$name %in% "globcover_urban"] <- "Urban (Proportion)"
data_sum_yr$name[data_sum_yr$name %in% "ndvi_cropland"] <- "NDVI in Cropland Areas (Average)"
data_sum_yr$name[data_sum_yr$name %in% "viirs_mean"] <- "NTL: VIIRS (Average)"

p <- ggplot(data_sum_yr,
       aes(x=year,
           y=value)) +
  geom_line() +
  facet_wrap( ~ name,
              scales = "free") +
  theme_minimal() +
  labs(x="", 
       y="")  
ggsave(p, filename = file.path(figures_file_path, "woreda_dv_trends.png"),
       height = 6, width=8)






