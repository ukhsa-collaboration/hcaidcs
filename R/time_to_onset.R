#' Calculates time to onset for inpatients
#'
#' Only applicable for inpatients. Where date of specimen is before the date of
#' admission, \code{NA} will be returned.
#' @param date_admitted Admission date in date format
#' @param date_specimen Date specimen taken in date format
#' @param patient_location Location of patient at time of specimen. One of:
#' "In-patient", "Day patient" or "Emergency Assessment"
#' @return A numeric value
#' @examples
#' onset <- dmy("05-01-2011")
#' admitted <- dmy("01-01-2011")
#' patient_loc <- "In-patient"
#' time_to_onset(admitted, onset, patient_loc)
#' \dontrun{
#' ## Should return NA
#' onset <- NA
#' time_to_onset(admitted, onset, patient_loc)
#' patient_loc <- ""
#' onset <- dmy("05-01-2011")
#' time_to_onset(admitted, onset, patient_loc)
#' }
#' @export

time_to_onset <- function(date_admitted, date_specimen, patient_location){
  trust_cats <- c("In-patient", "Day patient", "Emergency Assessment")
  z <- ifelse(is.na(date_admitted) == TRUE | is.na(date_specimen) == TRUE,
              NA,
              ifelse(date_specimen < date_admitted, NA,
                     ifelse( patient_location %in% trust_cats,
                             as.numeric(date_specimen - date_admitted), NA
                      )
                     )
  )
  return(z)
}
