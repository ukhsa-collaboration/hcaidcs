#' Determine whether record is from an acute trust inpatient for AEC
#'
#' Inpatients have a patient location of 'NHS Acute Trust' and are one of:
#' inpatient, day patient or emergency assessment.
#' Assumes that where the date of admission is greater than the date of specimen, the patient was not actually an inpatient.
#' @param patient_location Location of patient at time of specimen.
#' @param patient_category Category of patient at time of specimen. One of:
#' "In-patient", "Day patient" or "Emergency Assessment"
#' @param specimen_date Date specimen taken
#' @param date_admitted Date of admission to NHS Acute Trust
#' @return A numeric value
#' @examples
#' patient_cat <- "In-patient"
#' patient_loc <- "NHS Acute Trust"
#' date_admitted <- lubridate::dmy("01/01/2016")
#' specimen_date <- lubridate::dmy("02/01/2016")
#' aec_inpatient(patient_loc, patient_cat, specimen_date, date_admitted)
#' \dontrun{
#' date_admitted <- NA
#' aec_inpatient(patient_loc, patient_cat, specimen_date, date_admitted)
#' date_admitted <- lubridate::dmy("03/01/2016")
#' aec_inpatient(patient_loc, patient_cat, specimen_date, date_admitted)
#' specimen_date <- NA
#' aec_inpatient(patient_loc, patient_cat, specimen_date, date_admitted)
#' }
#' @export

aec_inpatient <- function(patient_location, patient_category, specimen_date,
                          date_admitted){
  trust_cats <- c("In-patient", "Day patient", "Emergency Assessment")
  z <- ifelse(date_admitted > specimen_date | is.na(date_admitted) == TRUE, 0,
              ifelse(patient_location == "NHS Acute Trust" & patient_category %in% trust_cats,
                     1, 0))
  return(z)
}
