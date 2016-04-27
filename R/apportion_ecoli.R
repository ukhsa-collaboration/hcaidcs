#' Provisional algorithm for apportioning *E. coli* BSI to NHS Trusts or not
#'
#' Based on apportionment algorithm for MSSA and CDI, but with two options for Trust-apportioned cases.
#' p48 of \href{https://hcaidcs.phe.org.uk/ContentManagement/LinksAndAnnouncements/HCAIDCS_Mandatory_Surveillance_Protocol_v4.0.pdf}{the mandatory surveillance protocol}.
#'
#' @include check_date_class.R
#' @param collection The collection (MRSA, MSSA, CDI, E. coli) to which the record belongs.
#' @param patient_location The patient's location at time of sample
#' @param patient_category The patient's category at time of sample
#' @param date_admitted The date the patient was admitted for records from inpatients
#' @param specimen_date The date of the specimen (POSIXct, POSIXt or Date format)
#' @param date_entered Date of admission (POSIXct, POSIXt or Date format)
#' @return An ordered factor giving non-Trust; Trust, 48 hours to 7 days; Trust, > 7 days or Trust, no date of admission.
#' @examples
# apportion_ecoli("NHS Acute Trust", "In-patient",
#           date_admitted = lubridate::dmy("01-01-2015"), specimen_date =
#             lubridate::dmy("05-01-2015"),
#           date_entered = lubridate::dmy("26-10-2015"))
#' \dontrun{
#' library(dplyr)
#' dat <- structure(list(
#'   patient_location = c("NHS Acute Trust", "NHS Acute Trust", "NHS Acute Trust",
#'                        "NHS Acute Trust", "NHS Acute Trust", "", "", "", "", "",
#'                        "NHS Acute Trust", "NHS Acute Trust", "NHS Acute Trust",
#'                        "NHS Acute Trust", "NHS Acute Trust", "", "", "", "", "",
#'                        "NHS Acute Trust", "", "Nursing Home", "Nursing Home",
#'                        "NHS Acute Trust", "NHS Acute Trust", "NHS Acute Trust",
#'                        "Nursing Home"),
#'   patient_category = c("In-patient", "Day patient", "Emergency Assessment",
#'                        "Unknown", "", "In-patient", "Day patient",
#'                        "Emergency Assessment", "Unknown", "", "In-patient",
#'                        "Day patient", "Emergency Assessment", "Unknown", "",
#'                        "In-patient", "Day patient", "Emergency Assessment",
#'                        "Unknown", "", "A&E only", "A&E only", "In-patient",
#'                        "In-patient", "In-patient", "Day patient",
#'                        "Emergency Assessment", "In-patient"),
#'   date_admitted = structure(c(16749, 16749, 16749, 16749, 16749, 16749, 16749,
#'                               16749, 16749, 16749, 16749, 16749, 16749, 16749,
#'                               16749, 16749, 16749, 16749, 16749, 16749, 16749,
#'                               16749, 16749, 16749, 16749, NA, NA, NA),
#'                             class = "Date"),
#'   specimen_date = structure(c(16749, 16751, 16760, 16751, 16751, 16749, 16751,
#'                               16760, 16751, 16751, 16749, 16751, 16760, 16751,
#'                               16751, 16749, 16751, 16760, 16751, 16751, 16749,
#'                               16749, 16749, 16749, 16749, 16751, 16760, 16749),
#'                             class = "Date"),
#'   date_entered = structure(c(16765, 16765, 16765, 16765, 16765, 16765, 16765,
#'                              16765, 16765, 16765, 16728, 16728, 16728, 16728,
#'                              16728, 16728, 16728, 16728, 16728, 16728, 16765,
#'                              16728, 16765, 16765, 16765, 16765, 16765, 16765),
#'                            class = "Date")), class = "data.frame",
#'   .Names = c("patient_location", "patient_category", "date_admitted",
#'   "specimen_date", "date_entered"), row.names = c(NA, -28L))
#'  dat %>% mutate(apportioned = apportion_ecoli(patient_location,
#'  patient_category, date_admitted, specimen_date, date_entered))
#'  }
#' @export

apportion_ecoli <- function(patient_location, patient_category,
                            date_admitted, specimen_date, date_entered){
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("lubridate is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  trust_cats <- c("In-patient", "Day patient", "Emergency Assessment", "Unknown", "")
  trust_locs <- c("NHS Acute Trust", "")
  check_date_class(date_admitted)
  check_date_class(specimen_date)
  check_date_class(date_entered)
  # Apportion
  z <- ifelse(ec_apportion_step1(patient_location, date_entered) == 0, "Non-Trust",
         ifelse(ec_apportion_step2(patient_category) == 0, "Non-Trust",
                ifelse(is.na(date_admitted) == FALSE,
                       ec_apportion_step3a(specimen_date, date_admitted),
                       ec_apportion_step3b(patient_location, patient_category)
                       )))
  z <- factor(z, levels = c("Non-Trust", "Trust, 48 hrs to 7 days", "Trust, >7 days", "Trust, no date of admission"))
  return(z)
}
