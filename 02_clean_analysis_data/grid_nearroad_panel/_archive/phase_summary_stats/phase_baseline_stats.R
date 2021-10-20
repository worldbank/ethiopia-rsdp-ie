# Phase Road States

NEAR_THRESHOLD <- 5 * 1000
ROUND_NUM <- 2

# Load and Prep Road Data ------------------------------------------------------
points <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data_clean.Rds"))

rsdp_rounds_stacked <- bind_rows(
  points[points$distance_rsdp_phase1 <= NEAR_THRESHOLD,] %>% filter(year %in% 1997) %>% mutate(rsdp_round = 1),
  points[points$distance_rsdp_phase2 <= NEAR_THRESHOLD,] %>% filter(year %in% 2003) %>% mutate(rsdp_round = 2),
  points[points$distance_rsdp_phase3 <= NEAR_THRESHOLD,] %>% filter(year %in% 2008) %>% mutate(rsdp_round = 3), 
  points[points$distance_rsdp_phase4 <= NEAR_THRESHOLD,] %>% filter(year %in% 2011) %>% mutate(rsdp_round = 4) 
)

#### Summarize by Round
summary_df <- rsdp_rounds_stacked %>%
  group_by(rsdp_round) %>%
  summarise(dmspols_mean = mean(dmspols),
            dmspols_prop_lit = mean(dmspols > 0),
            dmspols_prop_lit_N = sum(dmspols > 0),
            globcover_cropland_prop = mean(globcover_cropland),
            globcover_urban_prop = mean(globcover_urban),
            globcover_urban_N = sum(globcover_urban > 0),
            distance_city_addisababa_km_mean = mean(distance_city_addisababa)/1000) %>%
  t() %>%
  as.data.frame() %>%
  dplyr::rename(phase_1 = V1,
        phase_2 = V2,
        phase_3 = V3,
        phase_4 = V4) %>%
  rownames_to_column() %>%
  filter(rowname != "rsdp_round")

#### Summarize by Baseline/Endline
summary_df$baseline <- NA
summary_df$baseline[summary_df$rowname %in% "dmspols_mean"] <- mean(points$dmspols[points$year %in% 1996], na.rm=T)
summary_df$baseline[summary_df$rowname %in% "dmspols_prop_lit"] <- mean(points$dmspols[points$year %in% 1996] > 0, na.rm=T)
summary_df$baseline[summary_df$rowname %in% "dmspols_prop_lit_N"] <- sum(points$dmspols[points$year %in% 1996] > 0, na.rm=T)
summary_df$baseline[summary_df$rowname %in% "globcover_cropland_prop"] <- mean(points$globcover_cropland[points$year %in% 1996], na.rm=T)
summary_df$baseline[summary_df$rowname %in% "globcover_urban_prop"] <- mean(points$globcover_urban[points$year %in% 1996], na.rm=T)
summary_df$baseline[summary_df$rowname %in% "globcover_urban_N"] <- sum(points$globcover_urban[points$year %in% 1996] > 0, na.rm=T)
summary_df$baseline[summary_df$rowname %in% "distance_city_addisababa_km_mean"] <- mean(points$distance_city_addisababa[points$year %in% 1996]/1000, na.rm=T)

summary_df$endline <- NA
summary_df$endline[summary_df$rowname %in% "dmspols_mean"] <- mean(points$dmspols[points$year %in% 2013], na.rm=T)
summary_df$endline[summary_df$rowname %in% "dmspols_prop_lit"] <- mean(points$dmspols[points$year %in% 2013] > 0, na.rm=T)
summary_df$endline[summary_df$rowname %in% "dmspols_prop_lit_N"] <- sum(points$dmspols[points$year %in% 2013] > 0, na.rm=T)
summary_df$endline[summary_df$rowname %in% "globcover_cropland_prop"] <- mean(points$globcover_cropland[points$year %in% 2018], na.rm=T)
summary_df$endline[summary_df$rowname %in% "globcover_urban_prop"] <- mean(points$globcover_urban[points$year %in% 2018], na.rm=T)
summary_df$endline[summary_df$rowname %in% "globcover_urban_N"] <- sum(points$globcover_urban[points$year %in% 2018] > 0, na.rm=T)
summary_df$endline[summary_df$rowname %in% "distance_city_addisababa_km_mean"] <- mean(points$distance_city_addisababa[points$year %in% 2018]/1000, na.rm=T)

#### Rename Rows
summary_df$rowname[summary_df$rowname %in% "dmspols_mean"] <- "NTL, Mean"
summary_df$rowname[summary_df$rowname %in% "dmspols_prop_lit"] <- "NTL, Prop Cells Lit"
summary_df$rowname[summary_df$rowname %in% "dmspols_prop_lit_N"] <- "NTL, N Cells Lit"
summary_df$rowname[summary_df$rowname %in% "globcover_cropland_prop"] <- "Prop Cropland (Globcove)"
summary_df$rowname[summary_df$rowname %in% "globcover_urban_prop"] <- "Prop Urban (Globcover)"
summary_df$rowname[summary_df$rowname %in% "globcover_urban_N"] <- "N Cells with Urban Area (Globcover)"
summary_df$rowname[summary_df$rowname %in% "distance_city_addisababa_km_mean"] <- "Avg Dist to Addis Ababa (km)"

# Generate Table ---------------------------------------------------------------
#### Round Numbers
# Different level of rounding depending on the variable. For a variable, can't
# round by 2 for one row and 4 for another. Consequently, make strings and substring.

for(var in names(summary_df)[2:7]){
  summary_df[[var]] <- summary_df[[var]] %>% as.character()
}


for(var in names(summary_df)[2:7]){
  for(rowname_i in summary_df$rowname){

    if(rowname_i == "Prop Urban (Globcover)"){
      summary_df[[var]][summary_df$rowname %in% rowname_i] <- summary_df[[var]][summary_df$rowname %in% rowname_i] %>% as.numeric() %>% round(5) %>% format(scientific=F) %>% as.character()
    } else{
      summary_df[[var]][summary_df$rowname %in% rowname_i] <- summary_df[[var]][summary_df$rowname %in% rowname_i] %>% as.numeric() %>% round(2) %>% as.character()
    }
    
  }
}

##### Latex String
summary_df <- summary_df %>%
  mutate(latex = paste(rowname, " & ", 
                       phase_1, " & ", 
                       phase_2, " & ", 
                       phase_3, " & ", 
                       phase_4, " & ", 
                       baseline, " & ", 
                       endline, " \\\\ "))

#### Generate Table
sink(file.path(tables_file_path, "phase_baseline_stats.tex"))
cat("\\begin{tabular}{l | cccc | cc} ")
cat("\\hline ")
cat("Variable & \\multicolumn{4}{c|}{Phase} & & \\\\ ")
cat(" & I & II & III & IV & Baseline & Endline \\\\ ")
cat("\\hline ")
for(i in 1:nrow(summary_df)){
  
  cat(summary_df$latex[i])
  
}
cat("\\hline ")
cat("\\end{tabular} ")
sink()


