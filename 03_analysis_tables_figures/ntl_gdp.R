# NTL and GDP

# Create data ------------------------------------------------------------------
wdi_df <- WDI(indicator = c("NY.GDP.MKTP.KN"),
              country = "ET",
              start = 1992,
              end = 2013)

wor_sf <- readRDS(file.path(woreda_dir, "FinalData", "woreda.Rds")) %>%
  st_as_sf() %>%
  st_union() %>%
  st_transform(4326) 

ntl_df <- map_df(1992:2013, function(year_i){
  r <- raster(file.path(data_dir, "VIIRS_DMSPOLS_Intercalibrated", 
                        "RawData", paste0("Harmonized_DN_NTL_",year_i,"_calDMSP.tif")))
  
  ntl_sum <- exact_extract(r, wor_sf, fun = "sum")
  
  data.frame(ntl_sum = ntl_sum,
             year = year_i)
})

wdi_ntl_df <- wdi_df %>%
  left_join(ntl_df, by = "year") %>%
  clean_names() %>%
  mutate(ny_gdp_mktp_kn_log = log(ny_gdp_mktp_kn),
         ntl_sum_log = log(ntl_sum)) %>%
  arrange(year) %>%
  mutate(ny_gdp_mktp_kn_log_diff = c(diff(ny_gdp_mktp_kn_log), NA),
         ntl_sum_log_diff = c(diff(ntl_sum_log), NA))

# Analysis ---------------------------------------------------------------------

p1 <- wdi_ntl_df %>%
  ggplot() +
  geom_line(aes(x = year,
                y = ny_gdp_mktp_kn_log_diff)) +
  theme_classic2() +
  labs(x = NULL,
       y = "GDP, constant LCU (logged)",
       title = "A. Annual trends in GDP, logged") 

p2 <- wdi_ntl_df %>%
  ggplot() +
  geom_line(aes(x = year,
                y = ntl_sum_log_diff)) +
  theme_classic2() +
  labs(x = NULL,
       y = "Luminosity (logged)",
       title = "B. Annual trends in NTL, logged")

p3 <- wdi_ntl_df %>%
  ggplot(aes(y = ny_gdp_mktp_kn_log_diff,
             x = ntl_sum_log_diff)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = F,
              color = "darkorange") +
  #stat_regline_equation(label.x=10, label.y=27.5) +
  #stat_cor(aes(label=..rr.label..), label.x=10, label.y=27.3) +
  theme_classic2() +
  labs(x = "NTL luminosity, logged",
       y = "GDP, logged",
       title = "C. Association between GDP and NTL")

p <- ggarrange(p1, p2, p3, nrow =1)
ggsave(p, filename = file.path(paper_figures, "ntl_gdp.png"),
       height = 3, width = 12)

ntl_diff <- wdi_ntl_df$ntl_sum_log[wdi_ntl_df$year == 2013] -
  wdi_ntl_df$ntl_sum_log[wdi_ntl_df$year == 1992]

gdp_diff <- wdi_ntl_df$ny_gdp_mktp_kn_log[wdi_ntl_df$year == 2013] -
  wdi_ntl_df$ny_gdp_mktp_kn_log[wdi_ntl_df$year == 1992]

sink(file.path(paper_tables, "gdp_ntl.tex"))
cat("\\begin{tabular}{l | ll | l} \n")
cat("\\hline \n")
cat("Variable & 1992 & 2013 & 2013 - 1992 \\\\ \n")
cat("\\hline \n")
cat("GDP, logged & ")
cat(wdi_ntl_df$ny_gdp_mktp_kn_log[wdi_ntl_df$year == 1992] %>% round(2))
cat(" & ")
cat(wdi_ntl_df$ny_gdp_mktp_kn_log[wdi_ntl_df$year == 2013] %>% round(2))
cat(" & ")
cat(gdp_diff %>% round(2))
cat(" \\\\ \n")

cat("Nighttime lights, logged & ")
cat(wdi_ntl_df$ny_gdp_mktp_kn_log[wdi_ntl_df$year == 1992] %>% round(2))
cat(" & ")
cat(wdi_ntl_df$ny_gdp_mktp_kn_log[wdi_ntl_df$year == 2013] %>% round(2))
cat(" & ")
cat(ntl_diff %>% round(2))
cat(" \\\\ \n ")
cat("\\hline \n")
cat(" $(ln(GDP_{2013}) - ln(GDP_{1992})) - (ln(NTL_{2013}) - ln(NTL_{1992}))$ & & & ")
cat(round(gdp_diff/ntl_diff,2))
cat(" \\\\ \n ")
cat("\\hline \n")
cat("\\end{tabular}")
sink()



