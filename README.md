# Replication Package for: [The Impact of Ethiopia's Road Investment Program on Economic Development and Land Use: Evidence from Satellite Data](https://documents.worldbank.org/en/publication/documents-reports/documentdetail/099332404062230683/idu073a7158605532046490b712098aed9008539)

## Code

#### Main Script
* `_ethiopia_rsdp_ie_main.R`: Main script that runs all code

#### Organization

Code is organized into the below folders:

* `_functions`: User-defined functions that are used throughout
* `00_process_ancillary_datasets`: Processes datasets that are used in subsequent scripts
* `01_extract_data_to_datasets`: Extracts data to different datasets (e.g., nighttime lights in 1x1km grid, Kebeles and Woredas)
* `02_clean_analysis_data`: Cleans data for analysis
* `03_analysis_tables_figures`: Code for analysis and for generating tables and figures

## Data

### Summary 
Data can be found in the `/Data` folder [here](https://www.dropbox.com/sh/l19l75ktxii7iba/AAB973jQioF9b8OiPPAlvA9Ba?dl=0)
* Within `/Data`, there is a folder for each dataset. Each dataset folder generally contains a `/RawData` and `/FinalData` folder, where `/RawData` contains data downloaded from its source and `FinalData` contains data processed from code.
* `Panel Data RSDP Impacts` contains analysis datasets using different units of analysis and datasets of results (e.g., dataframe of regression coefficients that are later used to make figures). Within a dataset folder, `/merged_datasets` contains the final, analysis datasets. For example, `/Data/Panel Data RSDP Impacts/kebele/merged_datasets/panel_data_clean.Rds` contains cleaned Kebele-level panel data and `/longdiff_data_clean_base1996_end2016.Rds` contains cleaned Kebele-level data that uses a long difference from 1996 to 2016.

### Datasets that need to be manually downloaded

In this repository, we include all fully processed datasets in order for analysis to be easily replicated. However, the repository includes code to process raw data and create the fully processed dataset. A number of datasets need to be downloaded from their original source. The following datasets need to be manually downloaded; click on the link for instructions that include (1) the link to access and download the data and (2) a description of which folder to put the downloaded dataset into.

* [Electricity Network]
* [Elevation]
* [GlobCover: 1992 - 2015 Data]
* [GlobCover: 2016 - 2018 Data]
* [Gridded Population of the World]
* [Kebeles]
* [Nighttime Lights: VIIRS_DMSPOLS_Intercalibrated]
* [Precipitation]
* [Temperature]
* [World Bank Boundaries]

## To replicate analysis

1. Clone/download this github repository
2. Download the project folder that includes data [here](https://www.dropbox.com/sh/l19l75ktxii7iba/AAB973jQioF9b8OiPPAlvA9Ba?dl=0)
3. In [_ethiopia_rsdp_ie_main.R](https://github.com/worldbank/ethiopia-rsdp-ie/blob/main/_ethiopia_rsdp_ie_main.R), change the following paths:
* [project_dir](https://github.com/worldbank/ethiopia-rsdp-ie/blob/0f3fa9b2f904bc76907efe12ad30fe354548b9f2/_ethiopia_rsdp_ie_main.R#L25) should point to the data folder
* [code_dir](https://github.com/worldbank/ethiopia-rsdp-ie/blob/0f3fa9b2f904bc76907efe12ad30fe354548b9f2/_ethiopia_rsdp_ie_main.R#L26) should point to the github repo
4. Run `_ethiopia_rsdp_ie_main.R`; this runs all scripts needed to replicate the analysis, including data cleaning and generating all tables and figures