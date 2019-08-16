#' Apportion CDI cases on basis of prior healthcare interactions
#'
#' The mandatory surveillance launched a new apportioning algorithm on 01/04/2017.
#' This new apportioning algorithm takes into account prior healthcare interactions
#'   as recorded by trust users to apportion to one of four categories:
#' \itemize{
#'  \item Healthcare onset - healthcare associated
#'  \item Community onset - healthcare associated
#'  \item Community onset - indeterminate association
#'  \item Community onset - community associated
#' }
#' Records created prior to 01/04/2017 should not be apportioned.
#'
#' @param patient_location The patient's location at time of sample
#' @param patient_category The patient's category at time of sample
#' @param adm_date Date of admission
#' @param spec_date Date of specimen
#' @param adm_3_mo Was the patient an inpatient at the trust in previous three months. String yes, no or don't know
#' @param adm_4_weeks Was the patient admitted at any point 4 weeks prior to specimen. String yes or no
#' @param adm_12_weeks Was the patient admitted at any point 12 weeks prior to specimen. String yes or no
#' @param date_record_created Date record entered onto DCS
#'
#' @return A string variable giving the apportioning type; one of hoha, coha,
#'   coia or coca, all_blank if all of the prior healthcare exposures were NA,
#'   unknown_3_mo if prior health care in past three months was "Don't know" or
#'   NA if date record created < 01/04/2017
#' @export
#' @examples
#' testdat <- data.frame(
#'   patient_location = "NHS Acute Trust",
#'   patient_category = "In-patient",
#'   date_admitted = lubridate::dmy("01-01-2017"),
#'   specimen_date = lubridate::dmy("05-01-2017"), admission_in_3 = "yes",
#'   adm_4_wks = "yes", adm_12_wks = "yes",
#'   date_entered = lubridate::dmy("01-04-2017"),
#'   stringsAsFactors = FALSE)
#' testdat
#'
#' testdat$apportioned_prior_hc <- apportion_prior_healthcare(
#'   patient_location = testdat$patient_location,
#'   patient_category = testdat$patient_category,
#'   adm_date = testdat$date_admitted,
#'   spec_date = testdat$specimen_date,
#'   adm_3_mo = testdat$admission_in_3,
#'   adm_4_weeks = testdat$adm_4_wks,
#'   adm_12_weeks = testdat$adm_12_wks,
#'   date_record_created = testdat$date_entered
#' )
#' testdat
#' \dontrun{
#' testdat$apportioned_prior_hc <- NULL
#' testdat2 <- data.frame(
#'   patient_location = rep("NHS Acute trust", 3),
#'   patient_category = rep("In-patient", 3),
#'   date_admitted = rep(lubridate::dmy("01-01-2017"), 3),
#'   specimen_date = rep(lubridate::dmy("01-01-2017"), 3),
#'   admission_in_3 = c("yes", "yes", "yes"),
#'   adm_4_wks = c("yes", "no", "no"),
#'   adm_12_wks = c("no", "yes", "no"),
#'   date_entered = lubridate::dmy("01-04-2017"),
#'   stringsAsFactors = FALSE)
#' testdat <- dplyr::bind_rows(testdat, testdat2)
#' testdat
#'
#' testdat$apportioned_prior_hc <- apportion_prior_healthcare(
#'   patient_location = testdat$patient_location,
#'   patient_category = testdat$patient_category,
#'   adm_date = testdat$date_admitted,
#'   spec_date = testdat$specimen_date,
#'   adm_3_mo = testdat$admission_in_3,
#'   adm_4_weeks = testdat$adm_4_wks,
#'   adm_12_weeks = testdat$adm_12_wks,
#'   date_record_created = testdat$date_entered
#' )
#' testdat
#' }
#'

apportion_prior_healthcare <- function(patient_location, patient_category,
                                      adm_date, spec_date, adm_3_mo, adm_4_weeks,
                           adm_12_weeks, date_record_created){
  if(!requireNamespace("lubridate", quietly = TRUE)) {
    stop("lubridate needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if(!requireNamespace("assertthat", quietly = TRUE)) {
    stop("assertthat needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # Add assertions here

  assertthat::assert_that(class(adm_date) == "Date",
                          msg = "adm_date must be a date")
  # cat(class(spec_date))
  assertthat::assert_that(class(spec_date) == "Date",
                          msg = "spec_date must be a date")
  assertthat::assert_that(class(date_record_created) == "Date",
                          msg = "date_record_created must be a date")

  assertthat::assert_that(
    length(adm_3_mo[!(tolower(adm_3_mo) %in% c("yes", "no", "don't know", "", NA))]) == 0,
    msg = "adm_3_mo must be one of yes, no, don't know, \"\" or NA")



  days_diff <- spec_date - adm_date
  adm_4_weeks <- tolower(adm_4_weeks)
  adm_12_weeks <- tolower(adm_12_weeks)
  adm_3_mo <- tolower(adm_3_mo)

  adm_4_weeks <- ifelse(adm_4_weeks == "", NA_character_, adm_4_weeks)
  adm_12_weeks <- ifelse(adm_12_weeks == "", NA_character_, adm_12_weeks)
  adm_3_mo <- ifelse(adm_3_mo == "", NA_character_, adm_3_mo)

  hoha <- is_hoha(patient_location, patient_category, adm_date, spec_date, days_diff,
                  date_record_created)
  coha <- is_coha(days_diff, date_record_created, adm_3_mo, adm_4_weeks)
  coia <- is_coia(days_diff, date_record_created, adm_3_mo, adm_4_weeks, adm_12_weeks)
  coca <- is_coca(days_diff, date_record_created, adm_3_mo, adm_4_weeks, adm_12_weeks)

  z <- ifelse(hoha == 1, "hoha",
              ifelse(hoha == 0 & coha == 1, "coha",
                     ifelse(hoha == 0 & coha == 0 & coia == 1, "coia",
                            ifelse(hoha == 0 & coha == 0 & coia == 0 & coca == 1, "coca", NA)
                     )))

  z <- ifelse(adm_3_mo == "don't know" & !is.na(adm_3_mo) & z != "hoha", "unknown_3_mo", z)
  z <- ifelse(adm_3_mo == "no" & !is.na(adm_3_mo) & z != "hoha", "coca", z)
  z <- ifelse(is.na(adm_3_mo) & z != "hoha" &
                !(is.na(adm_3_mo) & is.na(adm_4_weeks) & is.na(adm_12_weeks)),
              "coca", z)
  z <- ifelse(z != "hoha" & (is.na(adm_3_mo) & is.na(adm_4_weeks) & is.na(adm_12_weeks)),
                             "all_blank", z)

  return(z)
}

is_hoha <- function(patient_location, patient_category, adm_date, spec_date, days_diff,
                    date_record_created){
  trust_cats <- c("In-patient", "Day patient", "Emergency Assessment", "Unknown", "")
  # trust_locs <- c("NHS Acute Trust", "", "Unknown")
  trust_locs <- c("NHS Acute Trust", "")

  z <- ifelse(is.na(adm_date) == TRUE,
              # value if is.na(adm_date) == TRUE
              ifelse( (patient_location %in% trust_locs | (patient_location == "Unknown" & date_record_created >= lubridate::dmy("26-10-2015")) ) &
                        (patient_category %in% trust_cats | is.na(patient_category)), 1, 0),
              # value if is.na(adm_date) == FALSE
              ifelse( (days_diff >= 2) &
                        (patient_location %in% trust_locs | (patient_location == "Unknown" & date_record_created >= lubridate::dmy("26-10-2015")) ) &
                        patient_category %in% trust_cats,
                      1, 0))
  return(z)
}

is_coha <- function(days_diff, date_record_created, adm_3_mo, adm_4_weeks, adm_12_weeks){
  z <- ifelse(
    # cases >= 2 days post-admission
    (!is.na(days_diff) & days_diff < 2 & (tolower(adm_4_weeks) == "yes" & !is.na(adm_4_weeks)) & date_record_created >= lubridate::dmy("01/04/2017")) |
      # cases not admitted and default to COHA when don't know entered for admitted in past three months
      ( (adm_3_mo == "don't know" | is.na(adm_3_mo)) & date_record_created >= lubridate::dmy("01/04/2017")) |
      ( tolower(adm_3_mo) == "yes" & tolower(adm_4_weeks) == "yes" & date_record_created >= lubridate::dmy("01/04/2017") ),
                    1, 0)
  return(z)
}

is_coia <- function(days_diff, date_record_created, adm_3_mo, adm_4_weeks, adm_12_weeks){
  z <- ifelse((!is.na(days_diff) & tolower(adm_3_mo) == "yes" & tolower(adm_12_weeks) == "yes" & !is.na(tolower(adm_12_weeks)) & date_record_created >= lubridate::dmy("01/04/2017")) |
                (tolower(adm_3_mo) == "yes" & (tolower(adm_12_weeks) == "yes" | is.na(tolower(adm_12_weeks))) ) & date_record_created >= lubridate::dmy("01/04/2017") |
                (adm_3_mo == "yes" & adm_4_weeks == "no" & adm_12_weeks == "no" & date_record_created >= lubridate::dmy("01/04/2017")),
              1, 0)
  return(z)
}

is_coca <- function(days_diff, date_record_created, adm_3_mo, adm_4_weeks, adm_12_weeks){
  z <- ifelse((!is.na(days_diff) & tolower(adm_4_weeks) == "no" & tolower(adm_12_weeks) == "no" & date_record_created >= lubridate::dmy("01/04/2017")) |
                # cases where patients have not been admitted in past 3 months
                adm_3_mo == "no" & date_record_created >= lubridate::dmy("01/04/2017"),
              1, 0)
  return(z)
}
