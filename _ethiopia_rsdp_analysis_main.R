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
# -- Code was run using R Version 4.3.3

# INSTRUCTIONS
# 1. Set root paths in "1. Filepaths"
# 2. Change settings in "2. Settings" to define which code to run

# 1. Filepaths -----------------------------------------------------------------
#### Root Paths
if(Sys.info()[["user"]] == "robmarty"){
  project_dir <- "~/Documents/Github/ethiopia-rsdp-ie"
  project_dir <- "~/Dropbox/World Bank/Replication Packages/Impact of Ethiopia RSDP"
}

# code_dir <- project_dir
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

## Whether to run scripts to produce tables/figures
RUN_CODE <- F

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

pacman::p_load(AER, 
               clusterSEs,
               coefplot,
               data.table,
               devtools,
               #did,
               doBy,
               dplyr,
               dvmisc,
               estimatr,
               gdistance,
               ggplot2,
               ggpubr,
               geodata,
               haven,
               hrbrthemes,
               labelled,
               leaflet,
               lfe,
               parallel,
               pbmcapply,
               purrr,
               raster,
               forcats,
               rasterVis,
               RColorBrewer,
               readr,
               modelsummary,
               reshape,
               riverplot,
               scales,
               sf,
               shp2graph,
               sfdep,
               spatialEco,
               spdep,
               spex,
               stargazer,
               stringr,
               terra,
               fixest,
               tibble,
               tidyr,
               tidyselect,
               TTR,
               viridis,
               wesanderson,
               exactextractr,
               conleyreg,
               blackmarbler,
               WDI,
               janitor)

# 5. User Defined Functions ----------------------------------------------------

# Functions
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

# 7. Code ======================================================================
if(RUN_CODE){
  
  # ** 7.3 ANALYSIS, TABLES & FIGURES ==========================================
  
  # **** Main Text: Summary Stats, Figures and Maps ----------------------------
  
  # FIGURE 1A: Sankey diagram of road improvements
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
  source(file.path(did_twfe_code_dir, "01_did_results.R")) 
  source(file.path(did_twfe_code_dir, "01_twfe_results.R"))
  
  # Make figures
  source(file.path(did_twfe_code_dir, "02_did_main_figures.R"))
  source(file.path(did_twfe_code_dir, "02_did_twfe_figures.R"))
  source(file.path(did_twfe_code_dir, "03_did_figures_buffers.R"))
  source(file.path(did_twfe_code_dir, "03_road_random_dist.R"))
  
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
  
  # **** Supplementary Information ---------------------------------------------
  
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
  # **
  source(file.path(analysis_code_dir, "table_kebele_area.R"))
  
  # Figure: Kebele Map
  # **
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
  
  ### SECTION: Urban Land Cover Classification Along Roads
  # Table: Proportion of urban area along roads
  source(file.path(analysis_code_dir, "figure_roads_urban_analysis.R"))
  
  ### SECTION: Nighttime Lights Data
  # Figure: Cloud cover over time
  source(file.path(analysis_code_dir, "figure_cloud_cover.R"))
  
  ### SECTION: Trends in Land Cover
  # Figure: Urban land in neighboring countries
  source(file.path(analysis_code_dir, "figure_gc_othercountry_check.R"))
  
  ### SECTION: Universal Electricity Access Program versus RSDP
  source(file.path(analysis_code_dir, 
                   "analysis_iv_longdiff",
                   "placebo_elect_trans.R"))
}
