#' inpatient
#'
#' Determines whether a record meets the criteria for an inpatient.
#' Will return 1 if the patient is an inpatient, day patient or emergency
#' assessment and is in an NHS acute trust at time of specimen.
#'
#' @param patient_category character giving the admission category
#' @param patient_locationcharacter giving the patient location at time of specimen
#' @export
#'
#' @return Numeric variable 1 for inpatients, 0 for other
#'
#' @examples
#' data(line_listing)
#'
#' line_listing$inpatient <- inpatient(
#'     as.character(line_listing$patient_category),
#'     as.character(line_listing$patient_location))

inpatient <- function(patient_category, patient_location){
  assertthat::assert_that(is.character(patient_category))
  assertthat::assert_that(is.character(patient_location))

  trust_cats <- c("In-patient", "Day patient", "Emergency Assessment")
  z <- ifelse( patient_category %in% trust_cats & patient_location == "NHS Acute Trust", 1, 0)
  return(z)
}
