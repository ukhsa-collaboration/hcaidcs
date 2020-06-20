# Version 0.1.1.9006

## New functions

Two new functions have been added: `age_sex_pyramid` and `graph_age_group`. 
`age_sex_pyramid` draws age-sex pyramids and `graph_age_group` produces ordered factors so that axes are plotted in order and the greater that/equal to operator is plotted correctly.

# Version 0.1.1.9005

## Minor changes and bug fix

Added function `graph_species_names` which will generate italicised species names for plotting. 

Fixed bug in `apportion_phc_date`. 
Previously this function only worked for CDI. 
It is now available for bacteraemias too. 

Some minor fixes to function documentation were added too. 

# Version 0.1.1.9004

## Major change

From 01 April 2019, the mandatory surveillance introduced a date of most recent previous discharge for *C. difficile* infections. 
This replaced the old set of three questions which asked for yes or no answers around previous trust exposure. 
Answers to these questions provided the data for the apportioning by prior healthcare.
The new function `apportion_phc_date` performs the apportioning by date and results can be merged with the `apportion_phc` results. 

## Minor changes

Added dependcy on cowplot so that annual graphs remain consistently formatted. 
Mostly this means I've added ` + theme_cowplot()` everywhere.
This does not change the input or output of the functions, but for reference this affects the following functions:

 * `aec_age_sex_plot`
 * `aec_age_trend_rate_pc_change`
 * `aec_age_trend_rate_bar`
 * `aec_ec_source_plot`
 * `aec_subregion_plot`
 * `aec_tto_plot`
 * `mf_fig1_fun`
 
# Version 0.1.1.9003

## Functions added

 * `date_to_cmonth` Function to convert a date to a calendar month, either as numeric (default), or factor, with levels in calendar year order
 * `mo_tab_long` Function to create long-format monthly tables
 * `factor_apportioned` Creates an ordered factor from apportioned vector
 * `factor_prior_hc` Creates an ordered factor from vector giving prior healthcare status 
 * `factor_apportion_both` Creates an ordered factor from output of either normal apportioning or prior trust exposure.

## Functions deprecated

 * `up_or_down` has been moved out to the [nicethings](https://simonthelwall.github.io/nicethings/) package.
 
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
