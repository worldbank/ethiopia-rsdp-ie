# Analysis Master

analysis_dir <- file.path(code_file_path, "DataWork", "Panel Data RSDP Impacts", "03_figures_tables_across_units",
                          "analysis")

## Two Way FE
source(file.path(analysis_dir, "02a_event_study_results.R"))
source(file.path(analysis_dir, "02b_event_study_results_append.R"))
source(file.path(analysis_dir, "02c_event_study_figures_units_separate.R"))

## IV
source(file.path(analysis_dir, "03a_iv_longdiff.R"))
source(file.path(analysis_dir, "03b_iv_figures_tables.R"))

## MA
source(file.path(analysis_dir, "04_ma_analysis_levels.R"))
source(file.path(analysis_dir, "04_ma_analysis_longdiff.R"))

