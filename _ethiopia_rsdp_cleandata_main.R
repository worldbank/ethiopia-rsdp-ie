# The Impact of Ethiopia's Road Investment Program on Economic Development and 
# Land Use: Evidence from Satellite Data

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
  project_dir <- "~/Documents/Github/ethiopia-rsdp-ie"
  project_dir <- "~/Dropbox/World Bank/Replication Packages/Impact of Ethiopia RSDP"
}

code_dir <- "~/Documents/Github/ethiopia-rsdp-ie"

#### Paths from Root
## Within project_dir
data_dir           <- file.path(project_dir, "data")

gadm_dir           <- file.path(data_dir, "GADM")
wb_boundaries_dir  <- file.path(data_dir, "World Bank Boundaries")
rsdp_dir           <- file.path(data_dir, "RSDP Roads")
ntl_harmon_dir     <- file.path(data_dir, "VIIRS_DMSPOLS_Intercalibrated")
ntl_bm_dir         <- file.path(data_dir, "VIIRS BlackMarble")
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
# paper_figures <- file.path(project_dir, "Output", "Figures")
# paper_tables  <- file.path(project_dir, "Output", "Tables")

paper_figures <- file.path("~/Dropbox/Apps/Overleaf/The Impact of Ethiopia RSDP Evidence from Satellite Data/Figures")
paper_tables  <- file.path("~/Dropbox/Apps/Overleaf/The Impact of Ethiopia RSDP Evidence from Satellite Data/Tables")

# 2. Settings ------------------------------------------------------------------
# The below settings define which code to run. Time estimates are given, which
# are based off of running code on a Mac with: (a) 3 GHz Dual-Code Intel Code i7
# processor & (b) 16 GB RAM.

##### RUN ANY CODE - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RUN_CODE <- F

##### WHETHER TO ONLY RUN CODE TO GENERATE FIGURES & TABLES - - - - - - - - - - 
# If TRUE, only generates tables and figures; skips code for cleaning datasets
# and skips code for estimating difference-in-difference & TWFE models (this 
# script takes a long time (1+ week), and saves data that is used to generate
# the figures). 
# TIME: Only generating figures & tables takes ~1 hour to run
ONLY_GEN_FIGURES_TABLES <- T

##### EXTRACT DATA TO DATASET PARAMETERS - - - - - - - - - - - - - - - - - - - -

# Checks if data already extracted. If T, re-extracts data. If F, skips 
# extracting data
OVERWRITE_EXTRACTED_DATA <- F 

# Whether to run code for creating unit level datasets. 
# TIME: Takes less than 1 hour to run.
CREATE_UNIT_LEVEL_DATASET <- F

# Whether to run code for extracting data to unit level datasets. 
# TIME: Takes ~1+ day to run
EXTRACT_DATA <- F

# Computing travel time between units for calculating market access.
# TIME: Takes 3+ days to run
SKIP_MA_COMPUTE_TT <- F

##### WHETHER TO DELETE PROCESSED FILES - - - - - - - - - -  - - - - - - - - - - 

# Whether to delete processed files; ie, data files that are created from code.
# Doing this may be useful for fully replicating the code to ensure it works
# from a "raw" state of only relying on RawData files.

# BY DEFAULT IS SET TO FALSE. 
DELETE_PROCESSED_FILES <- F

# 3. Parameters ----------------------------------------------------------------

# Ethiopia UTM
UTM_ETH     <- '+init=epsg:20138'
UTM_ETH_NUM <- 20138

# Meters distance for "close to road"
NEAR_CUTOFF <- 5000 # meters    

# 4. Packages ------------------------------------------------------------------

#### Specific Package Versions
if(!require("did")){
  require(devtools)
  install_version("did", version = "2.0.0", repos = "http://cran.us.r-project.org")
}

if(!require("riverplot")){
  install.packages("http://cran.r-project.org/src/contrib/Archive/riverplot/riverplot_0.10.tar.gz", 
                   type="source", repos=NULL)  
}

library(did)
library(riverplot)

#### Packages from CRAN
if(!require("pacman")) install.packages("pacman")

# pacman::p_load(AER, 
#                clusterSEs,
#                coefplot,
#                data.table,
#                devtools,
#                #did,
#                doBy,
#                dplyr,
#                dvmisc,
#                estimatr,
#                gdistance,
#                ggplot2,
#                ggpubr,
#                #geodata,
#                haven,
#                hrbrthemes,
#                labelled,
#                leaflet,
#                lfe,
#                parallel,
#                pbmcapply,
#                purrr,
#                raster,
#                rasterVis,
#                RColorBrewer,
#                readr,
#                modelsummary,
#                reshape,
#                #rgdal,
#                #rgeos,
#                riverplot,
#                scales,
#                sf,
#                shp2graph,
#                sfdep,
#                spatialEco,
#                spdep,
#                spex,
#                stargazer,
#                stringr,
#                #terra,
#                fixest,
#                tibble,
#                tidyr,
#                tidyselect,
#                TTR,
#                #velox,
#                viridis,
#                wesanderson,
#                exactextractr,
#                conleyreg)

library(AER)
library(clusterSEs)
library(coefplot)
library(data.table)
library(devtools)
# library(did)
library(doBy)
library(dplyr)
library(dvmisc)
library(estimatr)
library(gdistance)
library(ggplot2)
library(ggpubr)
# library(geodata)
library(haven)
library(hrbrthemes)
library(labelled)
library(leaflet)
library(lfe)
library(parallel)
library(pbmcapply)
library(purrr)
library(raster)
#library(rasterVis)
library(RColorBrewer)
library(readr)
#library(modelsummary)
library(reshape)
# library(rgdal)
# library(rgeos)
library(riverplot)
library(scales)
library(sf)
library(shp2graph)
#library(sfdep)
library(spatialEco)
library(spdep)
library(spex)
library(stargazer)
library(stringr)
# library(terra)
# library(fixest)
library(tibble)
library(tidyr)
library(tidyselect)
library(TTR)
library(velox)
library(viridis)
library(wesanderson)
library(exactextractr)
#library(conleyreg)
library(rgeos)
library(rgdal)


# 5. User Defined Functions ----------------------------------------------------

# Functions
source("https://raw.githubusercontent.com/ramarty/fast-functions/refs/heads/master/R/functions_in_chunks.R")
#source("https://raw.githubusercontent.com/ramarty/fast-functions/master/R/functions_in_chunks.R")
source(file.path(code_dir, "_functions", "clean_data_functions.R"))
source(file.path(code_dir, "_functions", "update_iv_coef_name.R"))

# Update modelsummary to not include table environment; only include tabular environment
modelsummary_tab <- function(...,
                             output = NULL){
  latex_output <- capture.output(modelsummary(..., output = "latex"))
  
  # Remove the "\begin{table}" and "\end{table}" lines
  latex_output <- latex_output[-c(1, length(latex_output))]
  latex_output <- latex_output[-1]
  
  # Print the customized LaTeX output without table environment
  sink(output)
  cat(latex_output, sep = "\n")
  sink()
}

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
  
  # **** Download VIIRS BlackMarble ------------------------------------------
  # Download GADM Data
  
  source(file.path(ancil_data_code_dir, "viirs_blackmarble", "download_viirs_bm.R"))
  
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
