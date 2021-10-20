# NTL Clusters Master

code_clusters_of_gc_urban <- file.path(rsdp_impact_analysis_code_file_path, "clusters_of_globcover_urban")

source(file.path(code_clusters_of_gc_urban, "01_merge_data.R"))
source(file.path(code_clusters_of_gc_urban, "02_clean_data.R"))
source(file.path(code_clusters_of_gc_urban, "03_map.R"))
source(file.path(code_clusters_of_gc_urban, "03_quickstats.R"))
source(file.path(code_clusters_of_gc_urban, "03_sum_stats_cluster_stats.R"))
source(file.path(code_clusters_of_gc_urban, "03_sum_stats_dep_vars.R"))
source(file.path(code_clusters_of_gc_urban, "03a_analysis_coef_each_year_results.R"))
source(file.path(code_clusters_of_gc_urban, "03b_analysis_coef_each_year_results_append.R"))
source(file.path(code_clusters_of_gc_urban, "03c_analysis_coef_each_year_figures.R"))







