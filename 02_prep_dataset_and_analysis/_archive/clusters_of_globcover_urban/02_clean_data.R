# Create Varibles for Analysis

# The code implements the following cleaning steps
# 1. Distance to road categories (e.g., min distance to improved road >50km/hr)
# 2. Create (1) years since improved and (2) binary improved variables
# 3. Add grouped, lagged treatment (dummy for 2-5 & 6-10 years before treatment)
# 4. Dependent variable transformations
# 5. Other variable transformations
# 6. Woreda level stats (within woreda + near road; not getting full woreda value)
# 7. Remove variables don't need

# The code is memory intensive. To prevent from crashing, break the datasets into
# different chunks, implementing the code on each chunk, the append together.
# Chunk done by woreda and cell, so that an entire woreda must be contained
# fully within a chunk

#### Parameters
NEAR_CUTOFF <- 5 * 1000     # meters distance for "close to road"
ALL_YEARS_IMPROVED_VAR <- F # add variables indicate 2nd and 3rd year of treatment
CHUNK_SIZE <- 200           # number of woredas in each chunk

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_globcover_urban", "merged_datasets", "panel_data.Rds"))

#data <- data %>%
#  filter(cluster_n_cells > 1)

# dataframes?
data$globcover_cropland <- data$globcover_cropland %>% as.vector()
data$globcover_cropland_sum <- data$globcover_cropland_sum  %>% as.vector()
data$globcover_urban      <- data$globcover_urban %>% as.vector()
data$globcover_urban_sum  <- data$globcover_urban_sum  %>% as.vector()


# Distance to aggregate road categories ----------------------------------------
# We calculate distance to roads by speed limit. Here we calculate distance
# to any road, road 50 km/hr and above and roads less than 50 km/hr

## Distance improved road
data$distance_improvedroad <- apply(data[,paste0("distance_improvedroad_speedafter_",c(20,25,30,35,45,50,70,120))], 1, FUN = min_na)
data$distance_improvedroad_50aboveafter <- apply(data[,paste0("distance_improvedroad_speedafter_",c(50,70,120))], 1, FUN = min_na)
data$distance_improvedroad_below50after <- apply(data[,paste0("distance_improvedroad_speedafter_",c(20,25,30,35,45))], 1, FUN = min_na)

## Distance road
data$distance_road <- apply(data[,paste0("distance_road_speed_",c(10,15,20,25,30,35,45,50,70,120))], 1, FUN = min_na)
data$distance_road_50above <- apply(data[,paste0("distance_road_speed_",c(50,70,120))], 1, FUN = min_na)
data$distance_road_below50 <- apply(data[,paste0("distance_road_speed_",c(10,15,20,25,30,35,45))], 1, FUN = min_na)

# Years Since / Post Improved Variables --------------------------------------
generate_road_improved_variables <- function(road_var, 
                                             data,
                                             all_years_improved_var,
                                             NEAR_CUTOFF){
  # DESCRIPTION: Creates variables indicating years since road improved,
  # and first year road was improved.
  
  # INPUT:
  # road_var: name of road variable that captures distance to road in meters
  # data: dataset
  # all_years_improved_var: T/F, whether to add a variable indicating all
  # years near an improved road
  
  print(road_var)
  final_vars <- c("year_roadTEMP", "years_since_roadTEMP", "post_roadTEMP")
  
  road_type <- road_var %>% str_replace_all("distance_", "")
  data$distance_roadTEMP <- data[[road_var]]
  
  ## Variable for year of first improvement
  data <- data %>%
    
    # Whether near improved road
    mutate(near_roadTEMP = distance_roadTEMP <= NEAR_CUTOFF) %>%
    
    # Year road improved (if any). Only consider earliest improved road. If cell near
    # area where another road was improved, we don't consider this. So:
    # 0 0 0 0 2007 0 0 2010 0 0 0 --> would yield 2007, while all zeros returns NA
    mutate(near_roadTEMP_X_year = near_roadTEMP * year) %>%
    mutate(near_roadTEMP_X_year = na_if(near_roadTEMP_X_year, 0) %>% as.numeric())
  
  # Create variable indicating all years road improved: e.g., 2007;2010
  if(all_years_improved_var){
    data <- data %>%
      group_by(cell_id) %>%
      mutate(near_roadTEMP_all_years = paste(near_roadTEMP_X_year, collapse=";") %>% str_replace_all("NA;|;NA", "")) 
    
    final_vars <- c(final_vars, "near_roadTEMP_all_years")
  }
  
  ## Variable for each cell of first year became near an improved road
  data_dt <- as.data.table(data)
  data <- data_dt[, year_roadTEMP:=min(near_roadTEMP_X_year,na.rm=T), by=list(cell_id)] %>% as.data.frame()
  data$year_roadTEMP[data$year_roadTEMP %in% Inf] <- NA
  
  ## Years since road improved and binary 1/0 road improved variable
  data$years_since_roadTEMP <- data$year - data$year_roadTEMP
  data$post_roadTEMP <- data$years_since_roadTEMP >= 0
  data$post_roadTEMP[is.na(data$post_roadTEMP)] <- 0
  
  # +/- 10 years aggregate
  data$years_since_roadTEMP[data$years_since_roadTEMP >= 10] <- 10
  data$years_since_roadTEMP[data$years_since_roadTEMP <= -10] <- -10
  
  # Subset variables and rename
  data <- data %>%
    dplyr::select(all_of(final_vars))
  
  # Prep variables
  data$years_since_roadTEMP <- data$years_since_roadTEMP %>% as.factor() %>% relevel("-1")
  data$post_roadTEMP <- data$post_roadTEMP %>% as.numeric()
  
  names(data) <- names(data) %>% str_replace_all("roadTEMP", road_type)
  
  return(data)
}

roadimproved_df <- lapply(c("distance_improvedroad", 
                            "distance_improvedroad_50aboveafter", 
                            "distance_improvedroad_below50after",
                            "distance_road", 
                            "distance_road_50above", 
                            "distance_road_below50"),
                          generate_road_improved_variables, 
                          data, 
                          ALL_YEARS_IMPROVED_VAR,
                          NEAR_CUTOFF) %>% bind_cols()
data <- bind_cols(data, roadimproved_df)

# Lagged treatment -----------------------------------------------------------
data$pre_improvedroad_neg2_5 <- as.numeric(data$years_since_improvedroad %in% -2:-5) %>% as.numeric()
data$pre_improvedroad_neg6_10 <- as.numeric(data$years_since_improvedroad %in% -6:-10) %>% as.numeric()

data$pre_improvedroad_50aboveafter_neg2_5 <- as.numeric(data$years_since_improvedroad_50aboveafter %in% -2:-5) %>% as.numeric()
data$pre_improvedroad_50aboveafter_neg6_10 <- as.numeric(data$years_since_improvedroad_50aboveafter %in% -6:-10) %>% as.numeric()

data$pre_improvedroad_below50after_neg2_5 <- as.numeric(data$years_since_improvedroad_below50after %in% -2:-5) %>% as.numeric()
data$pre_improvedroad_below50after_neg6_10 <- as.numeric(data$years_since_improvedroad_below50after %in% -6:-10) %>% as.numeric()

# Variables for treated time 2, 3, etc ---------------------------------------
if(ALL_YEARS_IMPROVED_VAR){
  data <- data %>%
    dplyr::mutate(near_improvedroad_all_years_t1 = near_improvedroad_all_years %>% substring(1,4) %>% as.numeric(),
                  near_improvedroad_all_years_t2 = near_improvedroad_all_years %>% substring(6,9) %>% as.numeric(),
                  near_improvedroad_all_years_t3 = near_improvedroad_all_years %>% substring(11,14) %>% as.numeric(),
                  
                  near_improvedroad_50aboveafter_all_years_t1 = near_improvedroad_50aboveafter_all_years %>% substring(1,4) %>% as.numeric(),
                  near_improvedroad_50aboveafter_all_years_t2 = near_improvedroad_50aboveafter_all_years %>% substring(6,9) %>% as.numeric(),
                  near_improvedroad_50aboveafter_all_years_t3 = near_improvedroad_50aboveafter_all_years %>% substring(11,14) %>% as.numeric(),
                  
                  near_improvedroad_below50after_all_years_t1 = near_improvedroad_below50after_all_years %>% substring(1,4) %>% as.numeric(),
                  near_improvedroad_below50after_all_years_t2 = near_improvedroad_below50after_all_years %>% substring(6,9) %>% as.numeric(),
                  near_improvedroad_below50after_all_years_t3 = near_improvedroad_below50after_all_years %>% substring(11,14) %>% as.numeric()) %>%
    
    dplyr::mutate(post_improvedroad_t1 = as.numeric((near_improvedroad_all_years_t1) - year >= 0),
                  post_improvedroad_t2 = as.numeric((near_improvedroad_all_years_t2) - year >= 0),
                  post_improvedroad_t3 = as.numeric((near_improvedroad_all_years_t3) - year >= 0),
                  
                  post_improvedroad_50aboveafter_t1 = as.numeric((near_improvedroad_50aboveafter_all_years_t1) - year >= 0),
                  post_improvedroad_50aboveafter_t2 = as.numeric((near_improvedroad_50aboveafter_all_years_t2) - year >= 0),
                  post_improvedroad_50aboveafter_t3 = as.numeric((near_improvedroad_50aboveafter_all_years_t3) - year >= 0),
                  
                  post_improvedroad_below50after_t1 = as.numeric((near_improvedroad_below50after_all_years_t1) - year >= 0),
                  post_improvedroad_below50after_t2 = as.numeric((near_improvedroad_below50after_all_years_t2) - year >= 0),
                  post_improvedroad_below50after_t3 = as.numeric((near_improvedroad_below50after_all_years_t3) - year >= 0))
  
  # Replace NAs with 0
  post_t_vars <- names(data)[grepl("_t1$|_t2$|_t3$", names(data)) & grepl("^post", names(data))]
  
  for(var in post_t_vars){
    data[[var]][is.na(data[[var]])] <- 0
  }
  
}

# Log Variables ----------------------------------------------------------------
calc_ihs <- function(x) log(x + sqrt(x^2 + 1))

ma_var <- data %>% names() %>% str_subset("^MA_")
for(var in ma_var) data[[paste0(var, "_log")]] <- data[[var]] %>% log()

ntl_var <- data %>% names() %>% str_subset("dmspols|globcover")
for(var in ntl_var) data[[paste0(var, "_log")]] <- log(data[[var]] + 1)
for(var in ntl_var) data[[paste0(var, "_ihs")]] <- calc_ihs(data[[var]])

# Dependent Variable Transformations -----------------------------------------
# Inverse Hyperbolic Since Transformation 
# This is used by Mitnik et. al. due to lots of zeros in DMSP-OLS 
calc_ihs <- function(x) log(x + sqrt(x^2 + 1))

data <- data %>%
  
  group_by(cell_id) %>%
  
  # Baseline variables
  mutate(dmspols_1996 = dmspols[year == 1996],
         dmspols_zhang_1996 = dmspols_zhang[year == 1996],
         dmspols_harmon_1996 = dmspols_harmon[year == 1996],
         globcover_urban_1996 = globcover_urban[year == 1996],
         globcover_urban_sum_1996 = globcover_urban_sum[year == 1996],
         globcover_urban_sum_ihs_1996 = globcover_urban_sum_ihs[year == 1996],
         
         globcover_cropland_1996 = globcover_cropland[year == 1996],
         globcover_cropland_sum_1996 = globcover_cropland_sum[year == 1996],
         globcover_cropland_sum_ihs_1996 = globcover_cropland_sum_ihs[year == 1996],
         dmspols_sum2_1996 = dmspols_sum2[year == 1996],
         dmspols_sum6_1996 = dmspols_sum6[year == 1996],
         dmspols_sum10_1996 = dmspols_sum10[year == 1996],
         dmspols_zhang_sum2_ihs_1996 = dmspols_zhang_sum2_ihs[year == 1996],
         dmspols_zhang_sum6_ihs_1996 = dmspols_zhang_sum6_ihs[year == 1996],
         dmspols_zhang_ihs_1996 = dmspols_zhang_ihs[year == 1996],
         dmspols_zhang_sum_ihs_1996 = dmspols_zhang_sum_ihs[year == 1996],
         
         dmspols_harmon_sum2_ihs_1996 = dmspols_harmon_sum2_ihs[year == 1996],
         dmspols_harmon_sum6_ihs_1996 = dmspols_harmon_sum6_ihs[year == 1996],
         dmspols_harmon_ihs_1996 = dmspols_harmon_ihs[year == 1996],
         dmspols_harmon_sum_ihs_1996 = dmspols_harmon_sum_ihs[year == 1996]) %>%
  
  ungroup() 
  
# Baseline NTL quantiles
dmspols_1996_median <- data$dmspols_1996[data$dmspols_1996 > 0] %>% median(na.rm=T) 
dmspols_1996_median <- 2
data$dmspols_1996_group[data$dmspols_1996 < dmspols_1996_median] <- "1"
data$dmspols_1996_group[data$dmspols_1996 >= dmspols_1996_median] <- "2"

dmspols_zhang_1996_median <- data$dmspols_zhang_1996[data$dmspols_zhang_1996 > 0] %>% median(na.rm=T) 
data$dmspols_zhang_1996_group[data$dmspols_zhang_1996 < dmspols_zhang_1996_median] <- "1"
data$dmspols_zhang_1996_group[data$dmspols_zhang_1996 >= dmspols_zhang_1996_median] <- "2"

# Default NTL Group
data$ntl_group <- data$dmspols_1996_group

# data$dmspols_1996_group <- data$dmspols_1996_group %>% as.factor()
# data$dmspols_zhang_1996_group <- data$dmspols_zhang_1996_group %>% as.factor()

#### Binary variables for above NTL threshold
# For woreda/non-grid dataset, this is done in the extraction phase, so this
# represents the proportion of cells. 

## Binary - Above or Not
data$dmspols_1 <- data$dmspols >= 1
data$dmspols_2 <- data$dmspols >= 2
data$dmspols_6 <- data$dmspols >= 6

data$dmspols_zhang_1 <- data$dmspols_zhang >= 1
data$dmspols_zhang_2 <- data$dmspols_zhang >= 2
data$dmspols_zhang_6 <- data$dmspols_zhang >= 6

data$dmspols_harmon_1 <- data$dmspols_harmon >= 1
data$dmspols_harmon_2 <- data$dmspols_harmon >= 2
data$dmspols_harmon_6 <- data$dmspols_harmon >= 6

# Log Counts -------------------------------------------------------------------
data$globcover_urban_sum_log <- log(data$globcover_urban_sum + 1)
data$dmspols_sum1_log <- log(data$dmspols_sum1 + 1)
data$dmspols_sum2_log <- log(data$dmspols_sum2 + 1)
data$dmspols_sum6_log <- log(data$dmspols_sum6 + 1)
data$dmspols_zhang_sum1_log <- log(data$dmspols_zhang_sum1 + 1)
data$dmspols_zhang_sum2_log <- log(data$dmspols_zhang_sum2 + 1)
data$dmspols_zhang_sum6_log <- log(data$dmspols_zhang_sum6 + 1)
data$dmspols_harmon_sum1_log <- log(data$dmspols_harmon_sum1 + 1)
data$dmspols_harmon_sum2_log <- log(data$dmspols_harmon_sum2 + 1)
data$dmspols_harmon_sum6_log <- log(data$dmspols_harmon_sum6 + 1)


## bin4
data$dmspols_1996_bin4 <- NA
data$dmspols_1996_bin4[data$dmspols_sum2_1996 %in% 0] <- 1
data$dmspols_1996_bin4[data$dmspols_sum2_1996 > 0]    <- 2
data$dmspols_1996_bin4[data$dmspols_sum6_1996 > 0]    <- 3
data$dmspols_1996_bin4[data$dmspols_sum10_1996 > 0]    <- 4

data$dmspols_1996_bin4_1 <-  as.numeric(data$dmspols_1996_bin4 == 1)
data$dmspols_1996_bin4_2 <-  as.numeric(data$dmspols_1996_bin4 == 2)
data$dmspols_1996_bin4_3 <-  as.numeric(data$dmspols_1996_bin4 == 3)
data$dmspols_1996_bin4_4 <-  as.numeric(data$dmspols_1996_bin4 == 4)

# Other variable transformations ---------------------------------------------
data$far_addis <- as.numeric(data$distance_city_addisababa >= 100*1000)

## Nighttime lights groups
# ntl_non0_med <- data$dmspols_1996_woreda[data$dmspols_1996_woreda > 0] %>% median(na.rm=T)
# data$ntl_group <- NA
# data$ntl_group[data$dmspols_1996_woreda <= ntl_non0_med] <- "1"
# data$ntl_group[data$dmspols_1996_woreda > ntl_non0_med] <- "2"

## Check Median NTL Value
data$dmspols_zhang[data$dmspols_zhang > 0 & data$year %in% 1996] %>% median(na.rm=T)

## NTL lit at baseline
data$dmspols_zhang_base0na <- data$dmspols_zhang
data$dmspols_zhang_base0na[data$dmspols_zhang_1996 %in% 0] <- NA

data$dmspols_zhang_ihs_base0na <- data$dmspols_zhang_ihs
data$dmspols_zhang_ihs_base0na[data$dmspols_zhang_1996 %in% 0] <- NA

# Subset Clusters: Positive Urban Cluster in Row -------------------------------
# For each cell_id (cluster), determine number of positive values in a row

# Subset Clusters: Cluster Size ------------------------------------------------


# Subset Clusters: Near Improved -----------------------------------------------
data <- data %>%
  filter(distance_anyimproved_ever < NEAR_CUTOFF)

## First Year Cluster Appears --------------------------------------------------
data$globcover_urban_sum_bin <- as.numeric(data$globcover_urban_sum > 0)

data <- data %>%
  mutate(globcover_urban_sum_bin_X_year = (globcover_urban_sum_bin * year) %>% ifelse(. == 0, NA, .)) %>%
  group_by(cell_id) %>%
  mutate(first_year_urban = min(globcover_urban_sum_bin_X_year, na.rm=T)) 

data$dmspols_zhang_sum0greater_bin <- as.numeric(data$dmspols_zhang_sum0greater > 0)
data$globcover_urban_sum_above0 <- as.numeric(data$globcover_urban_sum > 0)

# Baseline Variables - MA ------------------------------------------------------
MA_vars <- names(data) %>% str_subset("^MA_")

data_MA_vars <- data[data$year %in% 1996, c("cell_id", MA_vars)]
data_MA_vars <- data_MA_vars %>% rename_at(vars(-cell_id), ~ paste0(., '_1996'))

data <- merge(data, data_MA_vars, by = "cell_id")

# Pretrends Variables ----------------------------------------------------------
data <- data %>%
  group_by(cell_id) %>%
  mutate(globcover_urban_sum_pretnd96_92     = globcover_urban_sum[year == 1996]      - globcover_urban_sum[year == 1992],
         globcover_urban_sum_ihs_pretnd96_92 = globcover_urban_sum_ihs[year == 1996]  - globcover_urban_sum_ihs[year == 1992],
         globcover_urban_pretnd96_92     = globcover_urban[year == 1996]   - globcover_urban[year == 1992], 
         dmspols_pretnd96_92             = dmspols[year == 1996]           - dmspols[year == 1992],
         dmspols_log_pretnd96_92         = dmspols_log[year == 1996]       - dmspols_log[year == 1992],
         dmspols_ihs_pretnd96_92         = dmspols_ihs[year == 1996]       - dmspols_ihs[year == 1992],
         dmspols_zhang_log_pretnd96_92   = dmspols_zhang_log[year == 1996] - dmspols_zhang_log[year == 1992],
         dmspols_zhang_ihs_pretnd96_92   = dmspols_zhang_ihs[year == 1996] - dmspols_zhang_ihs[year == 1992],
         dmspols_harmon_log_pretnd96_92   = dmspols_harmon_log[year == 1996] - dmspols_harmon_log[year == 1992],
         dmspols_harmon_ihs_pretnd96_92   = dmspols_harmon_ihs[year == 1996] - dmspols_harmon_ihs[year == 1992]) %>%
  ungroup()

# Export -----------------------------------------------------------------------
saveRDS(data, file.path(panel_rsdp_imp_data_file_path, "clusters_of_globcover_urban", "merged_datasets", "panel_data_clean.Rds"))

