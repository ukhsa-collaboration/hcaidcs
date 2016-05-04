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
#' time_to_onset(admitted, onset, patient_cat)
#' dat <- data.frame(onset = onset, admitted = admitted, patient_cat = patient_cat)
#' dat$tto <- time_to_onset(dat$onset, dat$admitted, dat$patient_cat)
#' \dontrun{
#' ## Should return NA
#' onset <- NA
#' time_to_onset(admitted, onset, patient_cat)
#' patient_cat <- ""
#' onset <- lubridate::dmy("05-01-2011")
#' time_to_onset(admitted, onset, patient_cat)
#' }
#' @export

time_to_onset <- function(date_admitted, date_specimen, patient_category){
  trust_cats <- c("In-patient", "Day patient", "Emergency Assessment")
  z <- ifelse(is.na(date_admitted) == TRUE | is.na(date_specimen) == TRUE,
              NA,
              ifelse(date_specimen < date_admitted, NA,
                     ifelse( patient_category %in% trust_cats,
                             as.numeric(date_specimen - date_admitted), NA
                      )
                     )
  )
  return(z)
}
