#' Apportion CDI and MSSA records
#'
#' @include check_date_class.R
#' @param collection The collection (MRSA, MSSA, CDI, E. coli) to which the record belongs.
#' @param patient_location The patient's location at time of sample
#' @param patient_category The patient's category at time of sample
#' @param date_admitted The date the patient was admitted for records from inpatients
#' @param specimen_date The date of the specimen (POSIXct, POSIXt or Date format)
#' @param date_entered Date record created normally 'data collection date' (POSIXct, POSIXt or Date format)
#' @return A binary vector giving 1 for Trust-apportioned records and 0 for non-Trust apportioned records
#' @examples
#' apportion("mssa", "NHS Acute Trust", "In-patient",
#' date_admitted = lubridate::dmy("01-01-2015"), specimen_date =
#' lubridate::dmy("05-01-2015"), date_entered = lubridate::dmy("26-10-2015"))
#' \dontrun{
#' apportion("mssa", "NHS Acute Trust", "In-patient", date_admitted = NA,
#' specimen_date = lubridate::dmy("01-01-2015"), date_entered =
#' lubridate::dmy("26-10-2015"))
#' apportion("mssa", "NHS Acute Trust", "In-patient", date_admitted =
#' lubridate::dmy(""), specimen_date = lubridate::dmy("01-01-2015"),
#' date_entered = lubridate::dmy("26-10-2015"))
#' }
#' apportion("mssa", "NHS Acute Trust", "In-patient", date_admitted =
#' lubridate::dmy("01-01-2015"),  specimen_date = lubridate::dmy("01-01-2015"),
#' date_entered = lubridate::dmy("26-10-2015"))
#' apportion("mssa", "NHS Acute Trust", "Outpatient", date_admitted =
#' lubridate::dmy("01-01-2015"),  specimen_date = lubridate::dmy("05-01-2015"),
#' date_entered = lubridate::dmy("26-10-2015"))
#' apportion("cdi", "NHS Acute Trust", "In-patient", date_admitted =
#' lubridate::dmy("01-01-2015"),  specimen_date = lubridate::dmy("05-01-2015"),
#' date_entered = lubridate::dmy("26-10-2015"))
#' @export


apportion <- function(collection, patient_location, patient_category, date_admitted, specimen_date, date_entered){
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("lubridate is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  check_date_class(date_admitted)
  check_date_class(specimen_date)
  check_date_class(date_entered)
  if(collection != "mssa" & collection != "cdi"){
    stop("Collection is not either \"mssa\" or \"cdi\" ")
  }
  if(length(collection) == 1) {
    collection <- rep(collection, length(patient_location))
  }
  trust <- ifelse(collection == "mssa",
                  mssa_appt(patient_location, patient_category, date_admitted, specimen_date, date_entered),
                  cdi_appt(patient_location, patient_category, date_admitted, specimen_date, date_entered)
                  )

  return(trust)
}


mssa_appt <- function(patient_location, patient_category, date_admitted, specimen_date, date_entered){
  trust_cats <- c("In-patient", "Day patient", "Emergency Assessment", "Unknown", "")
  trust_locs <- c("NHS Acute Trust", "")
  z <- ifelse(is.na(date_admitted) == TRUE,
              ifelse( (patient_location %in% trust_locs | (patient_location == "Unknown" & date_entered >= lubridate::dmy("26-10-2015")) ) &
                  patient_category %in% trust_cats, 1, 0),
              ifelse( (specimen_date - date_admitted + 1 >= 3) &
                (patient_location %in% trust_locs | (patient_location == "Unknown" & date_entered >= lubridate::dmy("26-10-2015")) ) &
                patient_category %in% trust_cats,
              1, 0))
  return(z)
}

cdi_appt <- function(patient_location, patient_category, date_admitted, specimen_date, date_entered){
  trust_cats <- c("In-patient", "Day patient", "Emergency Assessment", "Unknown", "")
  trust_locs <- c("NHS Acute Trust", "")
  z <- ifelse(is.na(date_admitted) == TRUE,
              ifelse( (patient_location %in% trust_locs | (patient_location == "Unknown" & date_entered >= lubridate::dmy("26-10-2015")) ) &
                        patient_category %in% trust_cats, 1, 0),
              ifelse( (specimen_date - date_admitted + 1 >= 4) &
                        (patient_location %in% trust_locs | (patient_location == "Unknown" & date_entered >= lubridate::dmy("26-10-2015")) ) &
                        patient_category %in% trust_cats,
              1, 0))
  return(z)
}
