# The Impact of Ethiopia's Road Sector Development Program: Evidence from Satellite Data
# Main R Script

# OUTLINE
# 1. Define filepaths
# 2. Settings: Define which scripts to run
# 3. Parameters
# 4. Load packages
# 5. Load user defined functions
# 6. Delete processed files: If want to run from "scratch"
# 7. Run code

# NOTE
# -- Code was run using R Version 4.0.5

# INSTRUCTIONS
# 1. Set root paths in "1. Filepaths"
# 2. Change settings in "2. Settings" to define which code to run

# 1. Filepaths -----------------------------------------------------------------
#### Root Paths
if(Sys.info()[["user"]] == "robmarty"){
  project_dir  <- "~/Dropbox/World Bank/Replication Packages/Impact of Ethiopia RSDP"
  code_dir     <- "~/Documents/Github/ethiopia-rsdp-ie"
  overleaf_dir <- "~/Dropbox/Apps/Overleaf/The Impact of Ethiopia RSDP Evidence from Satellite Data"
}

#### Paths from Root
## Within project_dir
data_dir           <- file.path(project_dir, "Data")

gadm_dir           <- file.path(data_dir, "GADM")
rsdp_dir           <- file.path(data_dir, "RSDP Roads")
ntl_harmon_dir     <- file.path(data_dir, "VIIRS_DMSPOLS_Intercalibrated")
kebele_dir         <- file.path(data_dir, "Kebeles")
woreda_dir         <- file.path(data_dir, "Woredas")
gpw_dir            <- file.path(data_dir, "Gridded Population of the World")
rsdp123_iv_dir     <- file.path(data_dir, "RSDP Phase I-III Roads - IV - Targetted Locations")
region_caps_dir    <- file.path(data_dir, "Regional Capitals")
elev_dir           <- file.path(data_dir, "Elevation")
gc_dir             <- file.path(data_dir, "Globcover")
ndvi_dir           <- file.path(data_dir, "NDVI")
temp_dir           <- file.path(data_dir, "Temperature")
precip_dir         <- file.path(data_dir, "Precipitation")
elec_net_dir       <- file.path(data_dir, "Electricity Network")
panel_rsdp_imp_dir <- file.path(data_dir, "Panel Data RSDP Impacts")

## Within code_file_path
ancil_data_code_dir   <- file.path(code_dir, "00_process_ancillary_datasets")
extract_data_code_dir <- file.path(code_dir, "01_extract_data_to_datasets")
clean_data_code_dir   <- file.path(code_dir, "02_clean_analysis_data")
analysis_code_dir     <- file.path(code_dir, "03_analysis_tables_figures")

## For Tables/Figures
paper_figures <- file.path(overleaf_dir, "Figures")
paper_tables  <- file.path(overleaf_dir, "Tables")

#paper_figures <- file.path(project_dir, "Output", "Figures")
#paper_tables  <- file.path(project_dir, "Output", "Tables")

# 2. Settings ------------------------------------------------------------------
# The below settings define which code to run. Time estimates are given, which
# are based off of running code on a Mac with: (a) 3 GHz Dual-Code Intel Code i7
# processor & (b) 16 GB RAM.

##### RUN ANY CODE - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RUN_CODE <- T

##### WHETHER TO ONLY RUN CODE TO GENERATE FIGURES & TABLES - - - - - - - - - - 
# If TRUE, only generates tables and figures; skips code for cleaning datasets
# and skips code for estimating difference-in-difference & TWFE models (this 
# script takes a long time (1+ week), and saves data that is used to generate
# the figures). 
# Only generating figures & tables takes less than 1 hour
ONLY_GEN_FIGURES_TABLES <- T

##### EXTRACT DATA TO DATASET PARAMETERS - - - - - - - - - - - - - - - - - - - -

# Checks if data already extracted. If T, re-extracts data. If F, skips 
# extracting data
OVERWRITE_EXTRACTED_DATA <- F 

# Whether to run code for creating unit level datasets. 
# Takes less than 1 hour to run.
CREATE_UNIT_LEVEL_DATASET <- T

# Whether to run code for extracting data to unit level datasets. 
# Takes ~1+ day to run
EXTRACT_DATA <- T

# Computing travel time between units for calculating market access.
# Tabes ~3 days to run
SKIP_MA_COMPUTE_TT <- T

##### WHETHER TO DELETE PROCESSED FILES - - - - - - - - - -  - - - - - - - - - - 

# Whether to delete processed files; ie, data files that are created from code.
# Doing this may be useful for fully replicating the code to ensure it works
# from a "raw" state of only relying on RawData files.

# BY DEFAULT IS SET TO FALSE. 
DELETE_PROCESSED_FILES <- F

# 3. Parameters ----------------------------------------------------------------

# Ethiopia UTM
UTM_ETH <- '+init=epsg:20138'

# Meters distance for "close to road"
NEAR_CUTOFF <- 5 * 1000     

# 4. Packages ------------------------------------------------------------------
#library(devtools)
#install_github("hunzikp/velox")
#devtools::install_github("zeehio/facetscales")

library(AER)
library(estimatr)
library(labelled)
library(clusterSEs)
library(rgdal)
library(raster)
library(terra)
library(velox)
library(rgeos)
library(parallel)
library(pbmcapply)
library(haven)
library(spex)
library(RColorBrewer)
library(sf)
library(tidyr)
library(lfe)
library(reshape)
library(dplyr)
library(tibble)
library(ggplot2)
library(data.table)
library(coefplot)
library(stringr)
library(spdep)
library(doBy)
library(stargazer)
library(scales)
library(rasterVis)
library(ggpubr)
library(readr)
library(gdistance)
library(shp2graph)
library(riverplot)
library(leaflet)
library(TTR)
library(tidyselect)
library(dvmisc)
library(purrr)
library(viridis)
library(wesanderson)
library(hrbrthemes)
library(spatialEco)
library(did)
library(facetscales)

# 5. User Defined Functions ----------------------------------------------------

# Functions
source("https://raw.githubusercontent.com/ramarty/fast-functions/master/R/functions_in_chunks.R")
source(file.path(code_dir, "_functions", "clean_data_functions.R"))

# 6. Delete Processed Files ----------------------------------------------------
# Code to delete processed files; ie, data files that are created from the code.
# Only delete if you want to run the code from "scratch" and recreate all files.

if(DELETE_PROCESSED_FILES){
  
  # Print warning message
  total_time <- 30
  for(i_seconds in total_time:1){
    
    if((i_seconds %% 5) %in% 0){
      cat(paste0("***WARNING***: DATA FILES THAT ARE CREATED FROM THE CODE WILL BE DELETED IN ",
                 i_seconds, " SECONDS. IF YOU DID NOT INTEND FOR THIS TO HAPPEN ",
                 "STOP THE CODE FROM RUNNING AND SET 'DELETE_PROCESSED_FILES' TO FALSE."))
      cat("\n\n")
    }
    
    Sys.sleep(1)
  }
  
  
  tmp <- file.path(project_dir, "Data") %>%
    
    # Grab data files
    list.files(pattern = "*.Rds|*.dta", recursive = T, full.names = T) %>%
    
    # Subset to folders with processed data
    str_subset("FinalData|Panel Data RSDP Impacts") %>%
    
    # Remove files
    lapply(file.remove)
  
  cat("DATA FILES THAT ARE CREATED FROM THE CODE HAVE BEEN DELETED.")
}

# 7. Code ======================================================================
if(RUN_CODE){
  
  if(ONLY_GEN_FIGURES_TABLES %in% F){
    # ** 7.0 PREP ANCILLARY DATA ===============================================
    
    # **** Download Data from Google Earth Engine ------------------------------
    # These scripts should be run in the Google Earth Engine code editor. The data
    # files that these scripts produce have already been downloaded and put into 
    # "RawData" folders; consequently, these do not have to be run for the 
    # remaining code to work. The below files create rasters of elevation,
    # annual precipitation and annual temperature across Ethiopia.
    
    # [Github Repo]/00_process_ancillary_datasets/extract_from_gee/extract_elevation.js
    # [Github Repo]/00_process_ancillary_datasets/extract_from_gee/extract_precipitation.js
    # [Github Repo]/00_process_ancillary_datasets/extract_from_gee/extract_temperature.js
    
    # **** Download GADM -------------------------------------------------------
    # Download GADM Data
    
    source(file.path(ancil_data_code_dir, "gadm", "download_gadm.R"))
    
    # **** Clean Woredas -------------------------------------------------------
    # Clean Woreda file. Add nighttime lights and nighttime lights groups.
    
    source(file.path(ancil_data_code_dir, "woreda", "clean_woreda.R"))
    
    # **** RSDP I-III, Road IV MSTs --------------------------------------------
    # Create minimum spanning trees used as instruments
    
    rsdp123_iv_code_dir <- file.path(ancil_data_code_dir, "rsdp_123_iv")
    
    # Create dataset of targeted locations (endpoints of roads and regional capitals)
    source(file.path(rsdp123_iv_code_dir, "01_phase_123_roads_targetted_locations.R"))
    
    # MST between targeted locations, using Euclidean distance
    # NOTE: This code takes a few hours to run
    source(file.path(rsdp123_iv_code_dir, "02_create_euclidean_distance.R"))
    
    # MST between targeted locations, using cost surface
    # NOTE: This code takes a few hours to run
    source(file.path(rsdp123_iv_code_dir, "02_create_minimam_spanning_tree.R"))
    
    # We compute MSTs within each region; this code appends them together
    source(file.path(rsdp123_iv_code_dir, "03_append_regional_networks.R"))
    
    # ** 7.1 EXTRACT DATA TO DATASETS ==========================================
    # Scripts that (1) create datasets at different units of analysis -- grid,
    # kebeles and woreda; and (2) extracts data to these datasets. When extracting
    # data, saves a file for each different dataset. For example, saves a dataset
    # for distance to roads, a separate dataset for average nighttime lights, etc.
    # In a later step, these datasets are merged together.
    
    ## Process Kebeles
    DATASET_TYPE <- "kebele"
    source(file.path(extract_data_code_dir, "_extract_data_main.R"))
    
    ## Process grid; grid across all of Ethiopia
    DATASET_TYPE <- "dmspols_grid_ethiopia"
    source(file.path(extract_data_code_dir, "_extract_data_main.R"))
    
    ## Process grid; grids within 10km of a road
    DATASET_TYPE <- "dmspols_grid_nearroad"
    source(file.path(extract_data_code_dir, "_extract_data_main.R"))
    
    ## Process Woreda
    DATASET_TYPE <- "woreda"
    source(file.path(extract_data_code_dir, "_extract_data_main.R"))
    
    # ** 7.2 CLEAN ANALYSIS DATA ===============================================
    
    #### Panel Data
    
    ## Grid - Panel
    source(file.path(clean_data_code_dir, "grid_nearroad_panel", "01_merge_data.R"))
    source(file.path(clean_data_code_dir, "grid_nearroad_panel", "02_clean_data.R"))
    
    ## Kebele - Panel
    source(file.path(clean_data_code_dir, "kebele_panel", "01_merge_data.R"))
    source(file.path(clean_data_code_dir, "kebele_panel", "02_clean_data.R"))
    
    ## Woreda - Panel
    source(file.path(clean_data_code_dir, "woreda_panel", "01_merge_data.R"))
    source(file.path(clean_data_code_dir, "woreda_panel", "02_clean_data.R"))
    
    ## Grid - All Ethiopia
    source(file.path(clean_data_code_dir, "grid_ethiopia", "01_merge_data.R"))
    source(file.path(clean_data_code_dir, "grid_ethiopia", "02_clean_data.R"))
    
    #### Long Difference
    
    ## Kebele - Long Difference
    source(file.path(clean_data_code_dir, "kebele_longdifference", "01_clean_data.R"))
    
    ## Woreda - Long Difference
    source(file.path(clean_data_code_dir, "woreda_longdifference", "01_clean_data.R"))
    
    ## Grid/All Ethiopia - Long Difference
    source(file.path(clean_data_code_dir, "grid_ethiopia_longdifference", "01_clean_data.R"))
  }
  
  # ** 7.3 ANALYSIS, TABLES & FIGURES ==========================================
  
  # **** Main Text: Summary Stats, Figures and Maps ----------------------------
  
  # FIGURE 1A: Sankey diagram of road improvements
  # NOTE: Figure produced using R version 3.6.1 (R version 4.0.5 displayed differently)
  source(file.path(analysis_code_dir, "figure_sankey_speeds_rsdpyears.R"))
  
  # FIGURE 1B: Bar chart of road improvements by region
  source(file.path(analysis_code_dir, "figure_prop_network_improved_region_phase.R"))
  
  # TABLE 1: Summary Stats of Dependent variables
  source(file.path(analysis_code_dir, "table_sum_stats_dep_vars.R"))
  
  # FIGURE 2: Map of NTL, Globcover and RSDP
  source(file.path(analysis_code_dir, "figure_rsdp_NTL_globcover_map.R"))
  
  # FIGURE 3 & SI FIGURE: MST Maps
  source(file.path(analysis_code_dir, "figure_mst_map.R"))
  source(file.path(analysis_code_dir, "figure_mst_map_regional.R"))
  
  # **** Diff-in-Diff & TWFE ---------------------------------------------------
  did_twfe_code_dir <- file.path(analysis_code_dir, "analysis_did_twfe")
  
  # Estimate models and save dataframes of results
  if(ONLY_GEN_FIGURES_TABLES %in% F){
    source(file.path(did_twfe_code_dir, "01_did_results.R")) 
    source(file.path(did_twfe_code_dir, "01_twfe_results.R"))
  }
  
  # Make figures
  source(file.path(did_twfe_code_dir, "02_did_twfe_figures.R"))
  
  # **** Long-Diff: IV ---------------------------------------------------------
  
  # Run Regressions and export tabkes
  source(file.path(analysis_code_dir, "analysis_iv_longdiff", "iv_longdiff_ntlgroups2.R"))
  source(file.path(analysis_code_dir, "analysis_iv_longdiff", "iv_longdiff_ntlgroups4.R"))
  
  # **** Market Access ---------------------------------------------------------
  ma_code_dir <- file.path(analysis_code_dir, "analysis_ma")
  
  # MA Long Diff: Regressions and tables
  source(file.path(ma_code_dir, "ma_analysis_longdiff_ntlgroups2.R"))
  source(file.path(ma_code_dir, "ma_analysis_longdiff_ntlgroups4.R"))
  
  # MA Levels: Regressions and tables
  source(file.path(ma_code_dir, "ma_analysis_levels_ntlgroups2.R"))
  source(file.path(ma_code_dir, "ma_analysis_levels_ntlgroups4.R"))
  
  # **** SI --------------------------------------------------------------------
  
  ### SECTION: Road Improvements by Baseline Nighttime Lights
  
  # Figure: Length of roads above select speed limits, by baseline Woreda NTL
  source(file.path(analysis_code_dir, "figure_road_imp_by_woreda_ntl_length_above.R"))
  
  # Figure: Proportion of road types over time, by baseline Woreda NTL
  source(file.path(analysis_code_dir, "figure_road_imp_by_woreda_ntl_proportion.R"))
  
  
  ### SECTION: Nighttime Lights Data
  
  # Figure: Map of nighttime lights in multiple years
  source(file.path(analysis_code_dir, "figure_dmsp_multiple_years_map.R"))
  
  
  ### SECTION: Trends in Woreda-Level Outcome Variables Over Time
  
  # Figure: Trends in outcome variables
  source(file.path(analysis_code_dir, "figure_woreda_summary_trends.R"))
  
  # Figure: Growth rate of outcome variables
  source(file.path(analysis_code_dir, "figure_woreda_summary_histograms.R"))
  
  
  ### SECTION: Kebele Information
  
  # Table: Kebele Area
  source(file.path(analysis_code_dir, "table_kebele_area.R"))
  
  # Figure: Kebele Map
  source(file.path(analysis_code_dir, "figure_kebele_map.R"))
  
  
  ### SECTION: Universal Electricity Access Program versus RSDP
  
  # Figure: Map of electricity network & improved roads
  source(file.path(analysis_code_dir, "figure_electricity_network_vs_roads_map.R"))
  

  ### SECTION: What land cover type does urban replace?
  # Table: Globclover land cover class before changed to urban
  source(file.path(analysis_code_dir, "table_gc_land_class_before_urban.R"))
  

  ### SECTION: Estimating Travel Time from Road Network Data
  
  # Figures: Speed and example travel time
  source(file.path(analysis_code_dir, "figure_travel_time_speed_example.R"))
  
  
  ### SECTION: Constructing Minimum Spanning Trees
  
  # Figure: Cost surface map for MST
  source(file.path(analysis_code_dir, "figure_msts_cost_surface_map.R"))
  
  
  ### SECTION: Balance Across Non-Targeted Treated and Control Areas
  
  # Table: Balance across treated/control
  source(file.path(analysis_code_dir, "table_balance_nontargeted_control.R"))
  
  
  ### SECTION: Additional IV Results 
  # Table: N units near MSTs
  source(file.path(analysis_code_dir, "table_mst_n_units_near.R"))
  
}










