# Number of Projects Near

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data_clean.Rds"))

data <- data %>%
  mutate(near_improvedroad_Nprjs = near_improvedroad_all_years %>% 
           str_replace_all(";", "") %>% 
           na_if("NA") %>%
           nchar() %>% 
           divide_by(4),
         near_improvedroad_below50after_Nprjs = near_improvedroad_below50after_all_years %>% 
           str_replace_all(";", "") %>% 
           na_if("NA") %>%
           nchar() %>% 
           divide_by(4) %>%
           replace_na(0),
         near_improvedroad_50aboveafter_Nprjs = near_improvedroad_50aboveafter_all_years %>% 
           str_replace_all(";", "") %>% 
           na_if("NA") %>%
           nchar() %>% 
           divide_by(4) %>%
           replace_na(0)) %>%
  filter(year == 2016)

N_all <- data$near_improvedroad_Nprjs %>% table() %>%
  as.data.frame() %>%
  dplyr::rename(N_roads = ".",
                N_all = Freq) %>%
  mutate(prop_all = N_all / sum(N_all))

N_below50after <- data$near_improvedroad_below50after_Nprjs %>% table() %>%
  as.data.frame() %>%
  dplyr::rename(N_roads = ".",
                N_below50after = Freq) %>%
  mutate(prop_below50after = N_below50after / sum(N_below50after))

N_50aboveafter <- data$near_improvedroad_50aboveafter_Nprjs %>% table() %>%
  as.data.frame() %>%
  dplyr::rename(N_roads = ".",
                N_50aboveafter = Freq) %>%
  mutate(prop_50aboveafter = N_50aboveafter / sum(N_50aboveafter))

N_all <- N_all %>%
  full_join(N_50aboveafter, by="N_roads", all=T) %>%
  full_join(N_below50after, by="N_roads", all=T) %>%
  mutate(N_roads = N_roads %>% as.character %>% as.numeric) %>%
  arrange(N_roads)

for(var in names(N_all)){
  N_all[[var]][is.na(N_all[[var]])] <- 0
}

N_all <- N_all %>%
  mutate(tex = paste(N_roads, 
                     N_all, 
                     prop_all %>% round(3),
                     N_50aboveafter, 
                     prop_50aboveafter %>% round(3),
                     N_below50after, 
                     prop_below50after %>% round(3), 
                     sep=" & ")) %>%
  mutate(tex = paste(tex, " \\\\ "))

# Make Table -------------------------------------------------------------------
# \\geq
sink(file.path(tables_file_path, "wards_N_years_improved.tex"))
cat("\\begin{tabular}{c | cc | cc | cc}  \n")
cat("\\hline \n")
cat("N years where road & \\multicolumn{2}{c|}{All Roads} & \\multicolumn{2}{c|}{$\\geq$ 50km/hr} & \\multicolumn{2}{c}{$<$ 50km/hr} \\\\  \n")
cat("improved in woreda & N Woredas & Prop & N Woredas & Prop & N Woredas & Prop \\\\ \n")
cat("\\hline \n")

for(i in 1:nrow(N_all)){
  cat(N_all$tex[i], " \n")
}

cat("\\hline \n")
cat("\\end{tabular} ")
sink()


stargazer(N_all)




ggplot() +
  geom_bar(data=data,
           aes(x=near_improvedroad_Nprjs)) +
  coord_flip()




data$near_improvedroad_all_years %>% str_replace_all(";", "") %>% nchar() %>% divide_by(4)





lm(globcover_urban ~ road_length_X_speed_neigh_area, data=data) %>% summary()

###
data$road_length_X_speed_neigh <- log(data$road_length_X_speed_neigh + 1)
felm(dmspols_zhang_ihs ~ road_length_X_speed_neigh*factor(dmspols_1996_group) | cell_id + year  | 0 | cell_id, data=data) %>% summary()
felm(globcover_urban ~ road_length_X_speed_neigh*factor(dmspols_1996_group) | cell_id + year  | 0 | cell_id, data=data) %>% summary()

felm(dmspols_zhang_ihs ~ road_length_X_speed_neigh*factor(dmspols_1996_group) | cell_id + year  | 0 | cell_id, data=data) %>% summary()

felm(dmspols_zhang_ihs ~ log(road_length_35over_neigh_area+1) | cell_id + year  | 0 | cell_id, data=data) %>% summary()
felm(dmspols_zhang_ihs ~ road_length_10over_neigh_area | cell_id + year  | 0 | cell_id, data=data) %>% summary()
felm(dmspols_zhang_ihs ~ road_length_50over_neigh_area | cell_id + year  | 0 | cell_id, data=data) %>% summary()
felm(dmspols_zhang_ihs ~ road_length_70over_neigh_area | cell_id + year  | 0 | cell_id, data=data) %>% summary()
felm(dmspols_zhang_ihs ~ road_length_120over_neigh_area | cell_id + year  | 0 | cell_id, data=data) %>% summary()




###
data$post_improvedroad_t1[is.na(data$post_improvedroad_t1)] <- 0
data$post_improvedroad_t2[is.na(data$post_improvedroad_t2)] <- 0
data$post_improvedroad_t3[is.na(data$post_improvedroad_t3)] <- 0

data$post_improvedroad_t123 <- data$post_improvedroad_t1 + data$post_improvedroad_t2 + data$post_improvedroad_t3

felm(dmspols_zhang_ihs ~ factor(post_improvedroad_t123) | cell_id + year  | 0 | cell_id, data=data) %>% summary()

felm(dmspols_zhang_ihs ~ post_improvedroad_t1 + post_improvedroad_t2 + post_improvedroad_t3 | cell_id + year  | 0 | cell_id, data=data) %>% summary()

felm(dmspols_zhang_ihs ~ post_improvedroad_50aboveafter_t1 + post_improvedroad_50aboveafter_t2 + post_improvedroad_50aboveafter_t3 +
       post_improvedroad_below50after_t1 + post_improvedroad_below50after_t2 + post_improvedroad_below50after_t3| cell_id + year  | 0 | cell_id, data=data) %>% summary()



felm(globcover_urban ~ post_improvedroad_t1 + post_improvedroad_t2 + post_improvedroad_t3 | cell_id + year  | 0 | cell_id, data=data) %>% summary()





lm(globcover_urban ~ years_since_improvedroad_below50after,
   data=data[data$dmspols_1996_group %in% 2,]) %>% summary()



data$years_since_improvedroad %>% is.na %>% table()
data$year_improvedroad_50aboveafter %>% is.na %>% table()

lm(dmspols_1996_ihs ~ MA_rd_25over_theta5_exclude100km_log, data=data) %>% 
  summary()


lm(dmspols_ihs ~ MA_rd_25over_theta1_exclude100km_log, data=data) %>% 
  summary()

lm(dmspols_ihs ~ MA_rd_25over_theta1_log, data=data) %>% 
  summary()

lm(globcover_urban ~ MA_rd_25over_theta1_log, data=data) %>% 
  summary()

lm(globcover_urban ~ MA_rd_25over_theta1_exclude100km_log, data=data) %>% 
  summary()

lm(globcover_cropland ~ MA_rd_25over_theta1_log, data=data) %>% 
  summary()

lm(globcover_cropland ~ MA_rd_25over_theta1_exclude100km_log, data=data) %>% 
  summary()



lm(ndvi ~ MA_rd_25over_theta1_log, data=data) %>% 
  summary()

lm(ndvi ~ MA_rd_25over_theta1_exclude100km_log, data=data) %>% 
  summary()


felm(dmspols_zhang_ihs ~ MA_rd_25over_theta1 | year + uid | 0 | 0, data=data) %>% summary()



# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data_clean.Rds"))

data$dmspols_zhang_2 <- data$dmspols_zhang >= 2
data$dmspols_zhang_6 <- data$dmspols_zhang >= 6 # near median

# Functions --------------------------------------------------------------------
lm_confint_tidy <- function(lm, varremove){
  lm_confint <- confint(lm) %>% 
    as.data.frame
  names(lm_confint) <- c("p025", "p975")
  lm_confint$b <- (lm_confint$p025 + lm_confint$p975)/2
  lm_confint$variable <- row.names(lm_confint)
  
  lm_confint <- lm_confint[!grepl("cluster_id)|year)|Intercept)", lm_confint$variable),]
  lm_confint$years_since_improved <- gsub(varremove, "", lm_confint$variable) %>% as.numeric
  
  return(lm_confint)
}


# Overal Results ---------------------------------------------------------------
globcover_urban_df <- data.frame(NULL)
globcover_cropland_df <- data.frame(NULL)
ndvi_df <- data.frame(NULL)
dmspols_zhang_ihs_df <- data.frame(NULL)
dmspols_ihs_df <- data.frame(NULL)
dmspols_zhang_2_df <- data.frame(NULL)

for(region_type in c("All", "Dense", "Sparse")){
  print(region_type)
  if(region_type %in% c("Dense", "Sparse")){
    data_temp <- data[data$region_type %in% region_type,]
  } else{
    data_temp <- data
  }
  
  globcover_urban_df_temp <- bind_rows(
    felm(globcover_urban ~ years_since_improvedroad | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad") %>% mutate(var = "All"),
    
    felm(globcover_urban ~ years_since_improvedroad_50aboveafter | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad_50aboveafter") %>% mutate(var = "50 Above"),
    
    felm(globcover_urban ~ years_since_improvedroad_below50after | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad_below50after") %>% mutate(var = "Below 50")
    ) %>% mutate(region = region_type)
  
  globcover_cropland_df_temp <- bind_rows(
    felm(globcover_cropland ~ years_since_improvedroad | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad") %>% mutate(var = "All"),
    
    felm(globcover_cropland ~ years_since_improvedroad_50aboveafter | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad_50aboveafter") %>% mutate(var = "50 Above"),
    
    felm(globcover_cropland ~ years_since_improvedroad_below50after | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad_below50after") %>% mutate(var = "Below 50")
  ) %>% mutate(region = region_type)
  
  ndvi_df_temp <- bind_rows(
    felm(ndvi ~ years_since_improvedroad | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad") %>% mutate(var = "All"),
    
    felm(ndvi ~ years_since_improvedroad_50aboveafter | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad_50aboveafter") %>% mutate(var = "50 Above"),
    
    felm(ndvi ~ years_since_improvedroad_below50after | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad_below50after") %>% mutate(var = "Below 50")
  ) %>% mutate(region = region_type)
  
  dmspols_ihs_df_temp <- bind_rows(
    felm(dmspols_ihs ~ years_since_improvedroad | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad") %>% mutate(var = "All"),
    
    felm(dmspols_ihs ~ years_since_improvedroad_50aboveafter | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad_50aboveafter") %>% mutate(var = "50 Above"),
    
    felm(dmspols_ihs ~ years_since_improvedroad_below50after | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad_below50after") %>% mutate(var = "Below 50")
  ) %>% mutate(region = region_type)
  
  dmspols_zhang_ihs_df_temp <- bind_rows(
    felm(dmspols_zhang_ihs ~ years_since_improvedroad | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad") %>% mutate(var = "All"),
    
    felm(dmspols_zhang_ihs ~ years_since_improvedroad_50aboveafter | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad_50aboveafter") %>% mutate(var = "50 Above"),
    
    felm(dmspols_zhang_ihs ~ years_since_improvedroad_below50after | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad_below50after") %>% mutate(var = "Below 50")
  ) %>% mutate(region = region_type)
  
  dmspols_zhang_2_df_temp <- bind_rows(
    felm(dmspols_zhang_2 ~ years_since_improvedroad | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad") %>% mutate(var = "All"),
    
    felm(dmspols_zhang_2 ~ years_since_improvedroad_50aboveafter | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad_50aboveafter") %>% mutate(var = "50 Above"),
    
    felm(dmspols_zhang_2 ~ years_since_improvedroad_below50after | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad_below50after") %>% mutate(var = "Below 50")
  ) %>% mutate(region = region_type)
  
  globcover_urban_df <- bind_rows(globcover_urban_df_temp, globcover_urban_df)
  globcover_cropland_df <- bind_rows(globcover_cropland_df_temp, globcover_cropland_df)
  dmspols_ihs_df <- bind_rows(dmspols_ihs_df_temp, dmspols_ihs_df)
  dmspols_zhang_ihs_df <- bind_rows(dmspols_zhang_ihs_df_temp, dmspols_zhang_ihs_df)
  dmspols_zhang_2_df <- bind_rows(dmspols_zhang_2_df_temp, dmspols_zhang_2_df)
  ndvi_df <- bind_rows(ndvi_df_temp, ndvi_df)
}

# Figures ----------------------------------------------------------------------
p_dodge_width <- .5
ggplot(data=ndvi_df[ndvi_df$years_since_improved <= 10,], aes(x=years_since_improved, y=b, ymin=p025, ymax=p975,
                              group = var, color = var)) + 
  geom_vline(xintercept=0,size=3,alpha=0.15) +
  geom_hline(yintercept=0,size=1,alpha=0.15) +
  geom_point(position = position_dodge(width = p_dodge_width),size=2) + 
  geom_linerange(position = position_dodge(width = p_dodge_width),size=1.25) +
  labs(x="Years Since Improved Road Constructed",
       y="Coefficient",
       color="Road\nType") +
  theme_minimal() +
  facet_wrap(~region, scales="free", nrow=3)

fig <- ggplot(data = results_all_i[results_all_i$improved_road == "All",],
              aes(x = years_since_improved, 
                  y = b, ymin = p025, ymax = p975,
                  group = region_type, color = region_type)) +
  geom_vline(xintercept=0,size=3,alpha=0.15) +
  geom_point(position = position_dodge(width = p_dodge_width),size=3) + 
  geom_linerange(position = position_dodge(width = p_dodge_width),size=1.25) +
  labs(x="Years Since Improved Road Constructed",
       y="Coefficient",
       color = "Region",
       title = paste0(results_all_i$DV_full)) +
  scale_x_continuous(breaks = seq(-30, 30, by = 2)) +
  theme_minimal() +
  theme(axis.text = element_text(size=14, color="black"),
        axis.title = element_text(size=15, color="black"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        plot.title = element_text(hjust = 0.5, face="bold"),
        strip.text = element_text(size=14))  +
  facet_wrap( ~ ntl_base_full, nrow=4, scales="free") 



lm_result2 <- felm(dmspols_zhang_ihs ~ years_since_improvedroad | year + cell_id | 0 | GADM_ID_3, data=data) 

lm_result1 <- felm(globcover_urban ~ years_since_improvedroad | year + cell_id | 0 | GADM_ID_3, data=data[data$distance_city_addisababa > 50*1000,]) 
lm_result2 <- felm(dmspols_zhang_ihs ~ years_since_improvedroad | year + cell_id | 0 | GADM_ID_3, data=data[data$distance_city_addisababa > 50*1000,]) 

lm_result1 <- felm(globcover_urban ~ years_since_improvedroad_50aboveafter | year + cell_id | 0 | GADM_ID_3, data=data[data$distance_city_addisababa > 50*1000,]) 
lm_result2 <- felm(dmspols_zhang_ihs ~ years_since_improvedroad_50aboveafter | year + cell_id | 0 | GADM_ID_3, data=data[data$distance_city_addisababa > 50*1000,]) 




summary(lm_result2)

data$years_since_improved_50aboveafter %>% table()

# Improved by RDSP -------------------------------------------------------------
# RDSP I - From July 1997 - June 2002 [1997 - 2002]
# RDSP II - From July 2002 to June 2007 [2003 - 2007]
# RDSP III - From July 2007 to June 2010 [2008 - 2010]
# RDSP IV - From July 2010 to June 2015 [2011 - 2015]
# RDSP V - From July 2015 to June 2020 [2016 - 2020]

data_rdsp_i <- data[data$year_improved %in% 1997:2002,]
data_rdsp_ii <- data[data$year_improved %in% 2003:2007,]
data_rdsp_iii <- data[data$year_improved %in% 2008:2010,]
data_rdsp_iv <- data[data$year_improved %in% 2011:2015,]
data_rdsp_v <- data[data$year_improved %in% 2016:2020,]

# Globcover Urban
lm_rdspiii_urban <- felm(globcover_urban ~ post_improved | year + cell_id | 0 | GADM_ID_3, data=data_rdsp_iii) 
lm_rdspiii_urban_baseline <- felm(globcover_urban ~ post_improved*globcover_urban_1996 - globcover_urban_1996 | year + cell_id | 0 | GADM_ID_3, data=data_rdsp_iii) 

lm_rdspiii_urban_years <- felm(globcover_urban ~ years_since_improved | year + cell_id | 0 | GADM_ID_3, data=data_rdsp_iii) 

summary(lm_rdspiii_urban)
summary(lm_rdspiii_urban_baseline)
summary(lm_rdspiii_urban_years)

# DMSP-OLS
data_rsdp_temp <- data_rdsp_ii
lm_rdspiii_dmspols_zhang <- felm(dmspols_zhang_ihs ~ post_improved | year + cell_id | 0 | GADM_ID_3, data=data_rsdp_temp) 
lm_rdspiii_dmspols_zhang_baseline <- felm(dmspols_zhang_ihs ~ post_improved*dmspols_zhang_1996_ihs - dmspols_zhang_1996_ihs | year + cell_id | 0 | GADM_ID_3, data=data_rsdp_temp) 
lm_rdspiii_dmspols_zhang_baselinegroup <- felm(dmspols_zhang_ihs ~ post_improved*dmspols_zhang_1996_group - dmspols_zhang_1996_group | year + cell_id | 0 | GADM_ID_3, data=data_rsdp_temp) 

lm_rdspiii_dmspols_zhang_years <- felm(dmspols_zhang_ihs ~ years_since_improved | year + cell_id | 0 | GADM_ID_3, data=data_rsdp_temp) 
lm_rdspiii_dmspols_zhang_years_base1 <- felm(dmspols_zhang_ihs ~ years_since_improved | year + cell_id | 0 | GADM_ID_3, data=data_rsdp_temp[data_rsdp_temp$dmspols_zhang_1996_group %in% 1,]) 
lm_rdspiii_dmspols_zhang_years_base2 <- felm(dmspols_zhang_ihs ~ years_since_improved | year + cell_id | 0 | GADM_ID_3, data=data_rsdp_temp[data_rsdp_temp$dmspols_zhang_1996_group %in% 2,]) 
lm_rdspiii_dmspols_zhang_years_base3 <- felm(dmspols_zhang_ihs ~ years_since_improved | year + cell_id | 0 | GADM_ID_3, data=data_rsdp_temp[data_rsdp_temp$dmspols_zhang_1996_group %in% 3,]) 

summary(lm_rdspiii_dmspols_zhang)
summary(lm_rdspiii_dmspols_zhang_baseline)
summary(lm_rdspiii_dmspols_zhang_baselinegroup)

summary(lm_rdspiii_dmspols_zhang_years)
summary(lm_rdspiii_dmspols_zhang_years_base1)
summary(lm_rdspiii_dmspols_zhang_years_base2)
summary(lm_rdspiii_dmspols_zhang_years_base3)



lm_rdspiii_urban_baseline <- felm(globcover_urban ~ post_improved*globcover_urban_1996 - globcover_urban_1996 | year + cell_id | 0 | 0, data=data_rdsp_iii) 
summary(lm_rdspiii_urban_baseline)

lm_rdspiii_urban_baseline <- felm(globcover_cropland ~ post_improved*globcover_cropland_1996 - globcover_cropland_1996 | year + cell_id | 0 | 0, data=data_rdsp_iii) 
summary(lm_rdspiii_urban_baseline)

# Restrict to cells that were improved at some point where have data. For example,
# if using DMSP-OLS only have data until 2012, so only want areas near improved 
# roads that were improved until 2012. 

lm_test <- felm(globcover_urban ~ years_since_improved_50above | year + cell_id | 0 | GADM_ID_3, data=data)
summary(lm_test)


head(data)

