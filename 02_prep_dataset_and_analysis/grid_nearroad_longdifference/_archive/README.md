# Panel Data RSDP Impacts

This folder is for data cleaning and analysis for examining the impact of the RSDP on local economic activity and land cover change. The main units of analysis are a 1x1km grid (based of the DMSP-OLS grid) and woredas.

## Github Organization

* __01_create_main_analysis_datasets:__ Prepares datasets for analysis. First, a blank dataset is created that other variables are merged into. There is code for each dataset or variable. For example, code for calculating distance to a road will include include `uid` (unique id), `year` (if panel) and any distance to road variables. Datasets across multiple variables are merged together in a later step. This process is done so that if another dataset needs to be added, only that code and the code for merging needs to be run.

	* __01_create_initial_unitlevel_dataset:__ Creates blank datasets that variables from other datasets are merged into.	* __02_extract_variables:__ Extracts variables from other datasets.	* __03_extract_distance_road:__ Extracts distance to road variables	* __04_compute_market_access:__ Computes market access 	* __05_merge_and_clean_data:__ Merges datasets together and cleans variables

* __02_performs_analysis:__ Performs analysis

	* __phase_summary_stats:__ Summary statistics
	* __diff_in_diff:__ Difference-in-difference/event study analysis.	* __iv_longdiff:__ Instrumental variable approach using a long difference
	* __analysis_ward_specific:__ Analysis specific to ward-level (NOTE: more exploratory analysis here and files need to be organized/cleaned)
	
## Dropbox Organization

The `[Dropbox]/Data/Panel Data RSDP Impacts` folder has the following high level folders: `Data` (for datasets) and `Outputs` (for figures, tables and writeup of results).

#### Data

The data folder is organized by the dataset. There are sometimes multiple datasets with the same unit of analysis. For example, `dmspols_grid_dataset` contains a dataset at the 1x1km level across all of Ethiopia, while `dmspols_grid_dataset_nearroad` also contains a 1x1km level dataset, but where we only include pixels within a certain distance to a road. Each of these folders has the following sub-folders:

* __individual_datasets:__ For variable specific datasets (e.g., dataset with distance to road, dataset with nighttime lights, dataset with globcover variables, etc).
* __merged_datasets:__ For datasets that have merged the above datasets together and for additional processed/cleaned datasets. There are also separate datasets depending on the time period considered. For example, if we only want to consider phase 2 of RSDP, we only incorporate roads improved during that time period. Restricting analysis to phase 2 will impact variables for the long difference (e.g., start and end years) and road improvement variables (e.g., when the variable turns from 0---not near improved road---to 1---near improved road, as we only consider roads in the phase 2 time period). We produce datasets for the entire time period (`_clean_all.Rds`), for each phase (`_clean_phase1.Rds`), for time period with DMSPOLS data, 1992 - 2013 (`_clean_dmspols.Rds`) and for the time period with VIIRS data, 2012 - present (`_clean_viirs.Rds`).
* __results:__ In some cases, code stores model coefficients and saves them to later generate figures (we store when estimating the models takes a long time, and is useful to have separate code for estimating models and producing figures). These datasets are stored in this folder. 




