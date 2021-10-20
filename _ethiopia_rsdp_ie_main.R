# The Impact of Ethiopia's Road Sector Development Program: Evidence from Satellite Data
# Main R Script

# Filepaths --------------------------------------------------------------------
#### Root Paths
if(Sys.info()[["user"]] == "robmarty"){
  project_dir  <- "~/Dropbox/World Bank/Replication Packages/Impact of Ethiopia RSDP"
  code_dir     <- "~/Documents/Github/ethiopia-rsdp-ie"
  overleaf_dir <- "~/Dropbox/Apps/Overleaf/The Impact of Ethiopia RSDP Evidence from Satellite Data"
}

#### Paths from Root
## Within project_dir
data_dir       <- file.path(project_dir, "Data")
gadm_dir       <- file.path(data_dir, "GADM")
rsdp_dir       <- file.path(data_dir, "RSDP Roads")
ntl_harmon_dir <- file.path(data_dir, "VIIRS_DMSPOLS_Intercalibrated")
kebele_dir     <- file.path(data_dir, "Kebeles")
woreda_dir     <- file.path(data_dir, "Woredas")
gpw_dir        <- file.path(data_dir, "Gridded Population of the World")
rsdp123_iv_dir <- file.path(data_dir, "RSDP Phase I-III Roads - IV - Targetted Locations")
region_caps_dir <- file.path(data_dir, "Regional Capitals")
elev_dir       <- file.path(data_dir, "Elevation")
gc_dir         <- file.path(data_dir, "Globcover")
ndvi_dir       <- file.path(data_dir, "NDVI")
temp_dir       <- file.path(data_dir, "Temperature")
precip_dir     <- file.path(data_dir, "Precipitation")
panel_rsdp_imp_dir <- file.path(data_dir, "Panel Data RSDP Impacts")

## Within code_file_path
ancil_data_code_dir <- file.path(code_dir, "00_process_ancillary_datasets")
prep_data_code_dir <- file.path(code_dir, "01_extract_data_to_datasets")
analysis_code_dir  <- file.path(code_dir, "02_prep_dataset_and_analysis")

## For Tables/Figures
#paper_figures <- file.path(overleaf_dir, "Figures")
#paper_tables <- file.path(overleaf_dir, "Tables")

paper_figures <- file.path(project_dir, "Output", "Figures")
paper_tables <- file.path(project_dir, "Output", "Tables")

# Parameters -------------------------------------------------------------------

#### YEAR SUBSETS
# road_year <- list(all = 1996:2016,     
#                   dmspols = 1996:2012, 
#                   viirs = 2013:2016,  
#                   phase1 = 1997:2002, # 1997:2002
#                   phase2 = 2003:2007, # 2002:2007
#                   phase3 = 2008:2010, # 2007:2010
#                   phase4 = 2011:2016) # 2010:2015

# Ethiopia UTM
UTM_ETH <- '+init=epsg:20138' # Ethiopia UTM

# Packages ---------------------------------------------------------------------
library(AER)
library(did)
library(estimatr)
library(labelled)
library(clusterSEs)
library(rgdal)
library(raster)
library(terra)
library(velox)
library(dplyr)
library(rgeos)
library(parallel)
library(pbmcapply)
library(data.table)
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
library(facetscales) # devtools::install_github("zeehio/facetscales")

# User Defined Functions -------------------------------------------------------

# Functions
source("https://raw.githubusercontent.com/ramarty/fast-functions/master/R/functions_in_chunks.R")
source(file.path(code_dir, "_functions", "clean_data_functions.R"))
#source(file.path(code_dir, "Functions", "commonly_used.R"))
#source(file.path(code_dir, "Functions", "rename_lm_vars.R"))


# Code =========================================================================
RUN_CODE <- F

if(RUN_CODE){
  
  # 0. Clean Woredas -----------------------------------------------------------
  # Clean Woreda file. Add nighttime lights and nighttime lights groups.
  
  source(file.path(ancil_data_code_dir, "woreda", "clean_woreda.R"))
  
  # 0. RSDP I-III, Road IV MSTs ------------------------------------------------
  # Create minimum spanning trees used as instruments
  
  rsdp123_iv_code_dir <- file.path(ancil_data_code_dir, "rsdp_123_iv")

  # Create dataset of targeted locations (endpoints of roads and regional capitals)
  source(file.path(rsdp123_iv_code_dir, "01_phase_123_roads_targetted_locations.R"))
  
  # MST between targeted locations, using Euclidean distance
  source(file.path(rsdp123_iv_code_dir, "02_create_euclidean_distance.R"))
  
  # MST between targeted locations, using cost surface
  source(file.path(rsdp123_iv_code_dir, "02_create_minimam_spanning_tree.R"))
  
  # We compute MSTs within each region; this code appends them together
  source(file.path(rsdp123_iv_code_dir, "03_append_regional_networks.R"))
  
  # 1. Extract Data to Datasets --------------------------------------------------
  # Scripts that (1) create datasets at different units of analysis -- grid,
  # kebeles and woreda; and (2) extracts data to these datasets. When extracting
  # data, saves a file for each different dataset. For example, saves a dataset
  # for distance to roads, a separate dataset for average nighttime lights, etc.
  # In a later step, these datasets are merged together.
  
  #### PARAMETERS
  
  ## Whether to run code for creating unit level datasets and extracting data
  ## to those datasets
  CREATE_UNIT_LEVEL_DATASET <- T
  EXTRACT_DATA <- T
  
  ## Checks if data already extracted. If T, re-extracts data. If F, skips 
  ## extracting data
  OVERWRITE_EXTRACTED_DATA <- F 
  
  ## Computing travel time between untis for market access takes a particularly
  ## long time; whether to skip
  SKIP_MA_COMPUTE_TT <- F
  
  ## Process grid; grids within 10km of a road
  DATASET_TYPE <- "dmspols_grid_nearroad"
  source(file.path(prep_data_code_dir, "_extract_data_main.R"))
  
  ## Process Kebeles
  DATASET_TYPE <- "kebele"
  source(file.path(prep_data_code_dir, "_extract_data_main.R"))
  
  ## Process Woreda
  DATASET_TYPE <- "woreda"
  source(file.path(prep_data_code_dir, "_extract_data_main.R"))
  
  # 2. Prep Datasets and Analysis ----------------------------------------------
  
  # FIGURE 1A
  source(file.path(analysis_code_dir, "figures_road_improvement", 
                   "sankey_speeds_rsdpyears.R"))
  
  # FIGURE 1B
  source(file.path(analysis_code_dir, "figures_road_improvement", 
                   "prop_network_improved_region_phase.R"))
  
  
  
  
}










