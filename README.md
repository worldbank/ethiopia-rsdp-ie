# Replication Package for: The Impact of Ethiopia's Road Investment Program on Economic Development and Land Use: Evidence from Satellite Data

## Code

#### Main Script
* `_ethiopia_rsdp_ie_main.R`: Main script that runs all code

#### Organization
* `_functions`: User-defined functions that are used throughout
* `00_process_ancillary_datasets`: Processes datasets that are used in subsequent scripts
* `01_extract_data_to_datasets`: Extracts data to different datasets (e.g., nighttime lights in 1x1km grid, Kebeles and Woredas)
* `02_clean_analysis_data`: Cleans data for analysis
* `03_analysis_tables_figures`: Code for analysis and for generating tables and figures

## Data

Data can be found in the `/Data` folder here (INCLUDE LINK TO VIEWABLE DB FOLDER).
* Within `/Data`, there is a folder for each dataset. Each dataset folder generally contains a `/RawData` and `/FinalData` folder, where `/RawData` contains data downloaded from its source and `FinalData` contains data processed from code.
* `Panel Data RSDP Impacts` contains analysis datasets using different units of analysis and datasets of results (e.g., dataframe of regression coefficients that are later used to make figures). Within a dataset folder, `/merged_datasets` contains the final, analysis datasets. For example, `/Data/Panel Data RSDP Impacts/kebele/merged_datasets/panel_data_clean.Rds` contains cleaned Kebele-level panel data and `/longdiff_data_clean_base1996_end2016.Rds` contains cleaned Kebele-level data that uses a long difference from 1996 to 2016.



