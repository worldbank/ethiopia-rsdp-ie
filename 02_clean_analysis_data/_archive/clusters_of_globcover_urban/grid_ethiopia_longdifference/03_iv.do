* Main IV regressions estimated in R. However, using Stata to double check
* the results

use "/Users/robmarty/Dropbox/World Bank/IEs/Ethiopia IE/Data/Panel Data RSDP Impacts/Data/dmspols_grid_ethiopia/merged_datasets/longdiff_data_clean_base1996_end2012.dta"

reg dmspols_zhang_ihs distance_anyimproved_by2012, vce(cluster W_CODE)

ivregress 2sls dmspols_zhang_ihs (distance_anyimproved_by2012 = distance_mst), vce(cluster W_CODE)
estat firststage

ivregress 2sls dmspols_zhang_ihs (near_anyimproved_by2012_5km = near_mst_5km), vce(cluster W_CODE)
estat firststage
