#' query dcs live
#'
#' @examples
#'

query_dcs <- function(collection, cols = c(), date_from, date_to,
                      reporting_org = "all",
                      ccg = "all", test = FALSE){
  select_cols <- dcs_cols_lookup(cols)
  select_cols <- collapse_select(select_cols)
  dcs_query <- paste0(select_cols, " from INFECTION_EPISODE as IE (nolock)
                      left join DATA_COLLECTION (nolock) on DATA_COLLECTION.DATA_COLLECTION_CODE = IE.DATA_COLLECTION_CODE
                      left join organisation as org1 (nolock) on  org1.ORGANISATION_CODE = IE.reporting_organisation_code
                      left join organisation as org2 (nolock) on  org2.ORGANISATION_CODE = IE.attributed_organisation_code
                      left join organisation as org3 (nolock) on  org3.ORGANISATION_CODE = IE.LABORATORY_ORGANISATION_CODE
                      left join organisation as org4 (nolock) on  org4.ORGANISATION_CODE = IE.APPORTIONED_TO_ORGANISATION_CODE
                      left join organisation as org5 (nolock) on  org5.ORGANISATION_CODE = IE.ADMITTED_TO_ORGANISATION_CODE
                      left join PATIENT (nolock) on IE.patient_id = patient.patient_id
                      left join REFERENCE_TABLE_ITEM AS ref_tab_1 (NOLOCK) ON IE.PATIENT_LOCATION = ref_tab_1.REF_TABLE_ITEM_CODE
                      left join REFERENCE_TABLE_ITEM AS ref_tab_2 (NOLOCK) ON IE.PATIENT_CATEGORY = ref_tab_2.REF_TABLE_ITEM_CODE
                      left join REFERENCE_TABLE_ITEM AS ref_tab_3 (NOLOCK) ON ie.EPISODE_CATEGORY = ref_tab_3.REF_TABLE_ITEM_CODE
                      left join REFERENCE_TABLE_ITEM AS ref_tab_4 (NOLOCK) ON IE.MAIN_SPECIALTY = ref_tab_4.REF_TABLE_ITEM_CODE
                      left join REFERENCE_TABLE_ITEM AS ref_tab_5 (NOLOCK) ON IE.TREATMENT_SPECIALTY = ref_tab_5.REF_TABLE_ITEM_CODE
                      left join SP (nolock) on
                      left join ORGANISATION (NOLOCK) ON IE.REPORTING_ORGANISATION_CODE = ORGANISATION.ORGANISATION_CODE
where ie.is_deleted=0
and data_collection.data_collection_description in
                      ")
  if(test){
    return(dcs_query)
  }else{
    return(RODBCext::sqlExecute(channel = dcs_live, dcs_query, fetch = TRUE))
  }
}

#' dcs_cols_lookup
#'
#' convert desired column names into DCS database column names, with table prefix
#'
#' An internal function that provides a vectorised lookup of column names.
#' Non-matching column names are converted to NA and filtered out
#' @param cols A vector of desired column names
#' @examples
#' dcs_cols_lookup(cols = c("reporting_organisation_code", "ccg"))

dcs_cols_lookup <- function(cols){
  z <- case_when(
    cols == "reporting_organisation_code" ~ "IE.reporting_organisation_code",
    cols == "ccg" ~ "org2.ORGANISATION_NAME as 'ccg'",
    cols == "hospital_site_name" ~ "org5.ORGANISATION_NAME as 'hospital_site_name'",
    cols == "hospital_site_code" ~ "IE.ADMITTED_TO_ORGANISATION_CODE as 'hospital_site_code'",
    cols == "ccg_code" ~ "IE.attributed_organisation_code as 'ccg_code'",
    cols == "nhs_number" ~ "patient.NHSNO as 'nhs_number'",
    cols == "date_of_birth" ~ "PATIENT.DOB as 'date_of_birth'",
    cols == "sex" ~ "PATIENT.SEX as 'sex'",
    cols == "hospital_number" ~ "IE.HOSPITAL_NUMBER as 'hospital_number'",
    cols == "specimen_no" ~ "IE.LABORATORY_NUMBER as 'specimen_no'",
    cols == "laboratory_where_specimen_processed" ~ "org3.ORGANISATION_NAME as 'laboratory_where_specimen_processed'",
    cols == "forename" ~ "patient.FORENAME as 'forename'",
    cols == "surname" ~ "patient.SURNAME as 'surname'",
    cols == "patient_location" ~ "ref_tab_1.REF_VALUE as 'patient_location'",
    cols == "patient_category" ~ "ref_tab_2.REF_VALUE as 'patient_category'",
    cols == "episode_category" ~ "ref_tab_3.REF_VALUE as 'episode_category'",
    cols == "main_specialty" ~ "ref_tab_4.REF_VALUE as 'main_specialty'",
    cols == "treatment_specialty" ~ "ref_tab_5.REF_VALUE as 'treatment_specialty'",
    cols == "apportioned_organisation_code" ~ "ie.APPORTIONED_TO_ORGANISATION_CODE as 'apportioned_organisation_code'",
    cols == "apportioned_organisation_name" ~ "org4.ORGANISATION_NAME AS 'apportioned_organisation_name'",
    cols == "date_entered" ~ "ie.created_date as 'date_entered'",
    cols == "date_admitted" ~ "ie.DATE_ADMITTED as 'date_admitted'",
    cols == "last_update_date" ~ "ie.LAST_SYSTEM_UPDATED_DATE as 'last_update_date'",
    cols == "species_please_select_species" ~ "sp.question_option_description as 'species_please_select_species'",
    TRUE ~ NA_character_
  )
  z <- z[!is.na(z)]
  return(z)
}

#' collapse_select
#'
#' collapse output from \code{dcs_cols_lookup} into the start of a select query
#' @param cols vector output of \code{dcs_cols_lookup}
#' @examples
#' cols <- dcs_cols_lookup(cols = c("reporting_organisation_code", "ccg"))
#' collapse_select(cols)
collapse_select <- function(cols){
  z <- paste0("SELECT IE.INFECTION_EPISODE_ID as 'id',
              data_collection.data_collection_description as 'data_collection',
              IE.SPECIMEN_DATE as 'specimen_date', ",
              paste(cols, collapse = ", "))
  return(z)
}
