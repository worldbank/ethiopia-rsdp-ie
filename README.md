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

### Organization 
Data can be found in the `/data` folder
* Within `/data`, there is a folder for each dataset. Each dataset folder generally contains a `/RawData` and `/FinalData` folder, where `/RawData` contains data downloaded from its source and `FinalData` contains data processed from code.
* `Panel Data RSDP Impacts` contains analysis datasets using different units of analysis and datasets of results (e.g., dataframe of regression coefficients that are later used to make figures). Within a dataset folder, `/merged_datasets` contains the final, analysis datasets. For example, `/Data/Panel Data RSDP Impacts/kebele/merged_datasets/panel_data_clean.Rds` contains cleaned Kebele-level panel data and `/longdiff_data_clean_base1996_end2016.Rds` contains cleaned Kebele-level data that uses a long difference from 1996 to 2016.

### Datasets that need to be manually downloaded

The following datasets need to be manually downloaded. Click on the link for instructions that include (1) the link to access and download the data and (2) a description of which folder to put the downloaded dataset into.

* [Electricity Network](https://github.com/worldbank/ethiopia-rsdp-ie/tree/main/data/Electricity%20Network/RawData)
* [Elevation](https://github.com/worldbank/ethiopia-rsdp-ie/tree/main/data/Elevation/RawData)
* [GlobCover: 1992 - 2015 Data](https://github.com/worldbank/ethiopia-rsdp-ie/tree/main/data/Globcover/RawData/1992_2015_data)
* [GlobCover: 2016 - 2018 Data](https://github.com/worldbank/ethiopia-rsdp-ie/tree/main/data/Globcover/RawData/2016_2018_data)
* [Gridded Population of the World](https://github.com/worldbank/ethiopia-rsdp-ie/tree/main/data/Gridded%20Population%20of%20the%20World/RawData)
* [Kebeles](https://github.com/worldbank/ethiopia-rsdp-ie/tree/main/data/Kebeles/RawData)
* [Nighttime Lights: VIIRS_DMSPOLS_Intercalibrated](https://github.com/worldbank/ethiopia-rsdp-ie/tree/main/data/VIIRS_DMSPOLS_Intercalibrated/RawData)
* [Precipitation](https://github.com/worldbank/ethiopia-rsdp-ie/tree/main/data/Precipitation/RawData)
* [Temperature](https://github.com/worldbank/ethiopia-rsdp-ie/tree/main/data/Temperature/RawData)
* [World Bank Boundaries](https://github.com/worldbank/ethiopia-rsdp-ie/tree/main/data/World%20Bank%20Boundaries/RawData)

## To replicate analysis

1. Clone/download this github repository
2. Download the data that needs to be manually downloaded and put in the appropriate folders (see instructions above)
3. In [_ethiopia_rsdp_ie_main.R](https://github.com/worldbank/ethiopia-rsdp-ie/blob/main/_ethiopia_rsdp_ie_main.R), [project_dir](https://github.com/worldbank/ethiopia-rsdp-ie/blob/3b9e3b6844c3a76c4f162195276e89864fa252bf/_ethiopia_rsdp_ie_main.R#L25) should point to this github repo
4. Run `_ethiopia_rsdp_ie_main.R`; this runs all scripts needed to replicate the analysis, including data cleaning and generating all tables and figures