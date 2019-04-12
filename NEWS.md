# Version 0.1.1.9003

## Functions added

 * `date_to_cmonth` Function to convert a date to a calendar month, either as numeric (default), or factor, with levels in calendar year order
 * `mo_tab_long` Function to create long-format monthly tables
 
## Minor changes

 * `ann_tab_long` Simplified syntax for this function. This will make it more flexible. 
 Following arguments have been removed: collection_string, org_code, metric_type, output
 `period_var` argument added
 The expectation is that the user will supply the period as an ordered factor and the function will then sort the data by that variable. The same applies to the `metric_var` argument.
 
 * Functions for comparing wide data tables have been moved out to a separate branch (`merge_everything`,  `merge_one`, `data_compare`, `highlight`, `convert_csv2r`). The change to reporting long format tables has obviated these functions. 

# Version 0.1.1.9002

## Minor changes

Added tidyr dependency, so that the new function `ant_tab_long` expands data to include unobserved periods for organisations. 

Functions added:

 * `ann_tab_long` Function to create long annual and quarterly tables 

Data added

 * `line_listing` Data giving simulated line listing data to be used in examples and function testing

# Version 0.1.1.9001

## Minor changes
Added two new functions for the preparation of the monthly factsheet and expanded mf_trend_data to add observations and organisms.

Functions added:

 * `mf_lag_trend` preps the data so that lagged values are calculated
 * `mf_create_text_values` creates objects giving changes in counts of cases for use in factsheet
 
Also minor fix to `apportion_prior_hc`.

## Minor changes 
Tweaks to `mf_fig1_fun` so that the x axis labels are at a 45 degree angle and so that MRSA and MSSA are no longer bold face.

Updated `subregions_sp_df` to 2018 geography. 
Previously 2017 geography. 

Updated `apportion_phc` so that if the case is not HOHA and admitted in past three months is No or NA, then the result goes to COCA.

# Version 0.1.0

This is the first public release of the HCAIDCS package.

## Major changes

 * Added functions for PHE colours copied from phecharts package.
 * rlang is now an imported package.
 * Added functions to produce annual tables in wide format (fixes github issue #3)
 * `mrsa_source_of_bacteraemia` is now deprecated in favour of `group_source_bacteraemia`
 * Added function `aec_age_trend_rate_pc_change` to produce new age/sex rate trend graphs

## Bug fixes

 * `aec_ec_primary_focus_plot` now accepts an arbitrary number of levels of primary focus (issue #22). 
 * `fy_long` is now vectorised.
 * `ordered_m_y` has been updated to use tidyeval and now works (fixes github issue #5)
 * `group_source_bacteraemia` now recodes `NA` to `Not reported`
 * fixed `apportion_prior_healthcare` so that contradictory information indicating no admission in past 3 months, but admission in past 12 weeks now apportions to COIA.
 * Updated documentation on built in data for assignment_data, cdi_prior_hc_data and assignment_data2
 * Removed Viridis package dependency as this is no longer used in any function
