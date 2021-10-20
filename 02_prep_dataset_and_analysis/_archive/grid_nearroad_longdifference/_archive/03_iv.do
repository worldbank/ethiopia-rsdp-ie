* Long Difference IV

global dropbox_file_path "/Users/robmarty/Dropbox/World Bank/IEs/Ethiopia IE"

global tables "$dropbox_file_path/Outputs/Results/Tables"

* Load Data --------------------------------------------------------------------
use "$dropbox_file_path/Data/FinalData/dmspols_grid_dataset_nearroad/merged_datasets/long_diff_data.dta", clear

label var dmspols_2013diff "DMSPOLS"
label var dmspols_ihs_2013diff "DMSPOLS Growth Rt"

label var dmspols_zhang_2013diff "DMSPOLS"

label var globcover_urban_2018diff "Urban"
label var globcover_cropland_2018diff "Cropland"
label var ndvi_2018diff "NDVI"
label var ndvi_cropland_2018diff "NDVI - Cropland"

label var near_anyimproved_ever "Near Imp Rd"
label var near_improved_bf2013_ever "Near Imp Rd"

label var temp_avg_2013diff "Temp"
label var precipitation_2013diff "Precip"

label var temp_avg_2018diff "Temp"
label var precipitation_2018diff "Precip"

label var temp_avg "Temp"
label var precipitation "Precip"

label var near_mst "Near Span Tree"


* OLS --------------------------------------------------------------------------
reg dmspols_2013diff         near_improved_bf2013_ever   temp_avg precipitation, vce(cluster woreda_hdx_w_uid) 
	outreg2 using "$tables/longdiff_ols.tex",  addtext(Woreda FE, N) tex(frag) label replace
	
reg dmspols_ihs_2013diff        near_improved_bf2013_ever    temp_avg precipitation  , vce(cluster woreda_hdx_w_uid)
	outreg2 using "$tables/longdiff_ols.tex",   addtext(Woreda FE, N) tex(frag) label append
	
reg globcover_urban_2018diff  near_anyimproved_ever  temp_avg precipitation , vce(cluster woreda_hdx_w_uid)
	outreg2 using "$tables/longdiff_ols.tex",   addtext(Woreda FE, N) tex(frag) label append
	
reg globcover_cropland_2018diff near_anyimproved_ever temp_avg precipitation , vce(cluster woreda_hdx_w_uid)
	outreg2 using "$tables/longdiff_ols.tex",   addtext(Woreda FE, N) tex(frag) label append
	
reg ndvi_2018diff          near_anyimproved_ever     temp_avg precipitation , vce(cluster woreda_hdx_w_uid)
	outreg2 using "$tables/longdiff_ols.tex",   addtext(Woreda FE, N) tex(frag) label append
	
reg ndvi_cropland_2018diff     near_anyimproved_ever temp_avg precipitation, vce(cluster woreda_hdx_w_uid)
	outreg2 using "$tables/longdiff_ols.tex",   addtext(Woreda FE, N) tex(frag) label append


* 2SLS -------------------------------------------------------------------------
* 
	
 * 3.77 near_improved_bf2013_ever

*ivregress 2sls dmspols_2013diff            temp_avg precipitation i.woreda_hdx_w_uid (near_improved_bf2013_ever = near_mst), vce(cluster woreda_hdx_w_uid) first
*estat firststage

*ivregress 2sls globcover_urban_2018diff    temp_avg precipitation i.woreda_hdx_w_uid (near_anyimproved_ever = near_mst), vce(cluster woreda_hdx_w_uid)
*estat firststage

ivregress 2sls dmspols_2013diff            temp_avg precipitation i.woreda_hdx_w_uid (near_improved_bf2013_ever = near_mst), vce(cluster woreda_hdx_w_uid)
	outreg2 using "$tables/iv_s2.tex",  addtext(Woreda FE, Y, First Stage F Stat, 3.97) drop(i.woreda_hdx_w_uid) tex(frag) label replace
		
ivregress 2sls dmspols_ihs_2013diff            temp_avg precipitation i.woreda_hdx_w_uid (near_improved_bf2013_ever = near_mst), vce(cluster woreda_hdx_w_uid)
	outreg2 using "$tables/iv_s2.tex",   addtext(Woreda FE, Y, First Stage F Stat, 3.97) drop(i.woreda_hdx_w_uid) tex(frag) label append
	
ivregress 2sls globcover_urban_2018diff    temp_avg precipitation i.woreda_hdx_w_uid (near_anyimproved_ever = near_mst), vce(cluster woreda_hdx_w_uid)
	outreg2 using "$tables/iv_s2.tex",   addtext(Woreda FE, Y, First Stage F Stat, 2.12) drop(i.woreda_hdx_w_uid) tex(frag) label append
	
ivregress 2sls globcover_cropland_2018diff temp_avg precipitation i.woreda_hdx_w_uid (near_anyimproved_ever = near_mst), vce(cluster woreda_hdx_w_uid)
	outreg2 using "$tables/iv_s2.tex",   addtext(Woreda FE, Y, First Stage F Stat, 2.12) drop(i.woreda_hdx_w_uid) tex(frag) label append
	
ivregress 2sls ndvi_2018diff               temp_avg precipitation i.woreda_hdx_w_uid (near_anyimproved_ever = near_mst), vce(cluster woreda_hdx_w_uid)
	outreg2 using "$tables/iv_s2.tex",   addtext(Woreda FE, Y, First Stage F Stat, 2.12) drop(i.woreda_hdx_w_uid) tex(frag) label append
	
ivregress 2sls ndvi_cropland_2018diff      temp_avg precipitation i.woreda_hdx_w_uid (near_anyimproved_ever = near_mst), vce(cluster woreda_hdx_w_uid)
	outreg2 using "$tables/iv_s2.tex",   addtext(Woreda FE, Y, First Stage F Stat, 2.12) drop(i.woreda_hdx_w_uid) tex(frag) label append


