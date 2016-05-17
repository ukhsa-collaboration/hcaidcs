#' Calculates time to onset for inpatients
#'
#' Only applicable for inpatients. Where date of specimen is before the date of
#' admission, \code{NA} will be returned.
#' @param date_admitted Admission date in date format
#' @param date_specimen Date specimen taken in date format
#' @param patient_category Category of patient at time of specimen. One of:
#' "In-patient", "Day patient" or "Emergency Assessment"
#' @return A numeric value
#' @examples
#' onset <- lubridate::dmy("05-01-2011")
#' admitted <- lubridate::dmy("01-01-2011")
#' patient_cat <- "In-patient"
#' patient_loc <- "NHS Acute Trust"
#' time_to_onset(admitted, onset, patient_cat, patient_loc)
#' dat <- data.frame(onset = onset, admitted = admitted, patient_cat = patient_cat,
#' patient_loc = patient_loc)
#' dat$tto <- time_to_onset(dat$admitted, dat$onset, dat$patient_cat, dat$patient_loc)
#' \dontrun{
#' ## Should return NA
#' onset <- NA
#' time_to_onset(admitted, onset, patient_cat, patient_loc)
#' patient_cat <- ""
#' onset <- lubridate::dmy("05-01-2011")
#' time_to_onset(admitted, onset, patient_cat)
#' }
#' @export

time_to_onset <- function(date_admitted, date_specimen, patient_category, patient_location){
  trust_cats <- c("In-patient", "Day patient", "Emergency Assessment")
  z <- ifelse(is.na(date_admitted) == TRUE | is.na(date_specimen) == TRUE,
              NA,
              ifelse(date_specimen < date_admitted, NA,
                     ifelse( patient_category %in% trust_cats & patient_location == "NHS Acute Trust",
                             as.numeric(date_specimen - date_admitted), NA
                      )
                     )
  )
  return(z)
}
