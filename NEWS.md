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
