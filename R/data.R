#' NHS Area Team geographies for England
#'
#' A dataframe containing the geographies for NHS Area Teams.
#'
#' @format A data frame with ~87.5k observations and 12 columns:
#' \describe{
#'  \item{long}{Longitude}
#'  \item{lat}{Lattitude}
#'  \item{group}{Group id for polygons}
#'  \item{id}{Numeric id for Area Team, 0-9}
#'  \item{NHSATCD}{ONS code for Area Team}
#'  \item{NHSATNM}{Area Team name}
#'  \item{OBJECTID}{Id for an object. Only used for plotting}
#'  \item{hole}{Logical value for whether geography is a hole. Only used for plotting}
#'  \item{piece}{Only used for plotting}
#'  \item{order}{Gives plotting order}
#'  \item{Shape_STAr}{Only used for plotting}
#'  \item{Shape_STLe}{Only used for plotting}
#' }
#' @source Office for National Statistics
"at_sp_df"

#' NHS subregion geographies for England
#'
#' A dataframe containing the geographies for NHS subregions.
#'
#' @format A data frame with ~71.5k observations and 16 columns:
#' \describe{
#'  \item{long}{Longitude}
#'  \item{lat}{Lattitude}
#'  \item{OBJECTID}{Id for an object. Only used for plotting}
#'  \item{hole}{Logical value for whether geography is a hole. Only used for plotting}
#'  \item{piece}{Only used for plotting}
#'  \item{order}{Gives plotting order}
#'  \item{group}{Group id for polygons}
#'  \item{id}{Numeric id for subregion, 0-9}
#'  \item{GSS_CD}{ONS code for subregion}
#'  \item{GSS_NM}{ONS name for subregion}
#'  \item{Shape_STAr}{Only used for plotting}
#'  \item{Shape_STLe}{Only used for plotting}
#'  \item{centroid_lat}{Latitude values for subregion centroids for plotting labels}
#'  \item{centroid_long}{Longitude values for subregion centroids for plotting labels}
#'  \item{ONS_P_NM}{ONS Parent Name}
#'  \item{ODS_CD}{ODS ODS code for subregion}
#'  \item{ODS_P_CD}{ODS ODS code for parent region}
#'  \item{ONS_P_CD}{ONS Parent Code}
#' }
#' @source Office for National Statistics
"subregions_sp_df"

#' Trends in age of patients with *C. difficile* infection
#'
#' A data set containing data on trends in population rates of patients with
#' *C. difficile* infection in England, between 2007/08 and 2015/16
#'
#' @format A data frame with 108 rows and 7 variables:
#' \describe{
#'   \item{fyear6}{Financial year}
#'   \item{age_group_new}{Mandatory surveillance age group}
#'   \item{n}{Count of cases for age group-year}
#'   \item{popn}{Count of population for England for age group-year}
#'   \item{rate}{Rate of CDI per 100,000 population}
#'   \item{age_sex_pc}{Per cent of cases for age group-year}
#'   \item{sex}{Sex}
#' }
#' @source \url{https://hcaidcs.phe.org.uk/WebPages/GeneralHomePage.aspx}
"age_trends_data"

#' Data for monthly CCG table for MRSA
#'
#' A sample data set for use with the mo_ccg_table function.
#' Is output in intermediate step in production of monthly tables and has counts of cases, by CCG and month in long format.
#' @format A data frame with 714 rows and 12 variables:
#' \describe{
#'     \item{data_collection}{String giving the data collection}
#'     \item{ccg_code}{String giving the ccg code}
#'     \item{fyear6}{String giving 6 character financial year}
#'     \item{year_no}{Numeric giving calendar year}
#'     \item{month_no}{Numeric giving month number}
#'     \item{total_cases}{Count of all cases for CCG and month}
#'     \item{apportioned}{Count giving apportioned cases for CCG and month}
#'     \item{nhs_trust_pir}{Count of trust-assigned PIR cases}
#'     \item{ccg_pir}{Count of CCG-assigned PIR cases}
#'     \item{third_party_pir}{Count of third party-assigned PIR cases}
#'     \item{fmonth}{Numeric giving financial year month, i.e. April = 1}
#'     \item{month_string}{Character string giving name of month}
#' }
"monthly_ccg_data_raw"

#' Data for testing assignement algorithm
#'
#' A sample data set for testing the assignment_algorithm function.
#' @format A data frame with 14 rows of 6 variables:
#' \describe{
#'  \item{pircasestatus}{string}
#'  \item{assignmentmethodcode}{Numeric giving assignment method}
#'  \item{patientlocation}{string}
#'  \item{provisionalorganisationname}{string}
#'  \item{finalpirassignedorganisation}{string}
#'  \item{patient_category}{string}
#' }
"assignment_data"

#' Data for testing assignement algorithm
#'
#' A sample data set for testing the assignment_algorithm function.
#' @format A data frame with 15 rows of 7 variables:
#' \describe{
#'  \item{pir_case_status}{string}
#'  \item{assignment_method_code}{Numeric giving assignment method}
#'  \item{patient_location}{string}
#'  \item{provisional_organisation_name}{string}
#'  \item{final_pir_assigned_organisation_type}{string}
#'  \item{patient_category}{string}
#'  \item{id}{numeric}
#' }
"assignment_data2"

#' Data for monthly factsheet figure 1 function
#'
#' A sample data set from the preparation of the monthly HCAI factsheet giving the monthly counts of collections under mandatory surveillance over time.
#' @format
#' \describe{
#'  \item{t}{A date giving the first of the month}
#'  \item{year}{Integer giving year}
#'  \item{month}{Integer for month}
#'  \item{cdi}{Integer giving counts of cases}
#'  \item{ecoli}{Integer giving counts of cases}
#'  \item{mrsa}{Integer giving counts of cases}
#'  \item{mssa}{Integer giving counts of cases}
#' }
"mf_trend_data"
