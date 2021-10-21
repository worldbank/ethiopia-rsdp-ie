# Main code for extracting data to different datasets. Creates unit-level
# datasets then extracts data to those units. 

# Parameters -------------------------------------------------------------------

#### DATASET
# Defines dataset to run analysis on. Either at woreda level, grid level, or
# grid subsample:
# OPTIONS:
# --"dmspols_grid_ethiopia": Grid in Ethiopia
# --"dmspols_grid_nearroad": Near 10km of any road as of 2016
# --"woreda": Woreda polygons
# --"kebele": Kebele polygons

# DATASET_TYPE <- "kebele"

# Some scripts check whether DATASET_TYPE is a grid or polygon (eg, woreda) level.
# Inidates whether grid level for if/else statements for script
GRID_DATASET <- grepl("grid", DATASET_TYPE)

#### CHUNK SIZE
# For some functions, we break up the dataset into chunks. These are vectorized
# functions; however, vectorizing across the whole sample (eg, 1km grid across
# all of Ethiopia) would take up too much memory. Consequently, we vectorize
# into manageable chunks. Chunk size differs depending on grid level or woreda
# level.
if(GRID_DATASET){
  CHUNK_SIZE_DIST_ROADS <- 1250
} else{
  CHUNK_SIZE_DIST_ROADS <- 3
}

# Some functions set up to use multiple cores (mclapply). Number of cores to use.
MCCORS_DIST_ROADS <- 1 

# Close to road threshold (kilometers)
#DIST_THRESH <- 2 

# Run Script Parameters --------------------------------------------------------
## Whether to run code for creating unit level datasets and extracting data
## to those datasets
#CREATE_UNIT_LEVEL_DATASET <- T
#EXTRACT_DATA <- T

## Checks if data already extracted. If T, re-extracts data. If F, skips 
## extracting data
#OVERWRITE_EXTRACTED_DATA <- F 

## Computing travel time between untis for market access takes a particularly
## long time; whether to skip
#SKIP_MA_COMPUTE_TT <- F

# RUN SCRIPTS ==================================================================

# ** Create Unit Level Datasets ------------------------------------------------
if(CREATE_UNIT_LEVEL_DATASET){
  
  source(file.path(extract_data_code_dir, 
                   "01_create_initial_unitlevel_dataset", 
                   paste0("create_",DATASET_TYPE,".R")))
  
}

# ** Extract Data to Grids -----------------------------------------------------
if(EXTRACT_DATA){
  
  # Grab Scripts to Scrape - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  ## Scripts for all unit types
  scripts <- file.path(extract_data_code_dir, 
                       "02_extract_variables") %>%
    list.files(pattern = ".R", full.names = T) %>%
    sort()
  
  ## Scripts specific to units
  if(GRID_DATASET | grepl("kebele", DATASET_TYPE)){
    scripts_unit_specific <- file.path(extract_data_code_dir, 
                                       "02_extract_variables_grid_kebele_specific") %>%
      list.files(pattern = ".R", full.names = T) %>%
      sort()
    
    scripts <- c(scripts, scripts_unit_specific)
  } 
  
  ## Woreda Only
  if(grepl("woreda", DATASET_TYPE)){
    scripts_unit_specific <- file.path(extract_data_code_dir, 
                                       "02_extract_variables_woreda_specific") %>%
      list.files(pattern = ".R", full.names = T) %>%
      sort()
    
    scripts <- c(scripts, scripts_unit_specific)
  }
  
  ## Market Access Scripts
  if(grepl("woreda|kebele", DATASET_TYPE)){
    scripts_unit_specific <- file.path(extract_data_code_dir, 
                                       "02_extract_market_access") %>%
      list.files(pattern = ".R", full.names = T) %>%
      sort()
    
    scripts <- c(scripts, scripts_unit_specific)
  }
  
  # Remove certain scripts to run - - - - - - - - - - - - - - - - - - - - - - - 
  
  ## Check which data already extracted
  if(OVERWRITE_EXTRACTED_DATA %in% F){
    ## List of all datasets to be created
    dataset_names <- scripts %>% 
      str_replace_all(".*/", "") %>% 
      str_replace_all("extract_", "") %>%
      paste0("ds") # from .R to .Rds
    
    ## List of datasets already extracted
    extracted_datasets <- file.path(panel_rsdp_imp_dir,
                                    DATASET_TYPE,
                                    "individual_datasets") %>%
      list.files()
    
    ## Updated list of scripts to extract
    scripts <- scripts[!(dataset_names %in% extracted_datasets)]
  }
  
  ## Skip Certain Scripts
  if(SKIP_MA_COMPUTE_TT){
    scripts <- scripts[!grepl("extract_ma1_travel_times_for_market_access.R", scripts)]
  }
  
  # If DATASET_TYPE is "dmspols_grid_ethiopia", remove certain scripts; these
  # aren't needed for full grid and take a while to run 
  if(DATASET_TYPE == "dmspols_grid_ethiopia"){
    scripts_to_rm <- c("extract_gpw.R", 
                       "extract_distance_roads_improved_by_speedlimit_after.R") %>%
      paste(collapse = "|")
    
    scripts <- scripts[!grepl(scripts_to_rm, scripts)]
  }
  
  # Run Scripts - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  for(script_i in scripts){
    print(paste(script_i, "--------------------------------------------------"))
    source(script_i)
  } 
  
}








