#' Apportion cases on basis of prior healthcare interactions by date of most recent discharge
#'
#' The mandatory surveillance launched a new apportioning algorithm on 01/04/2019.
#' This new apportioning algorithm takes into account the date of the most recent
#' discharge from the reporting trust.
#'
#' Depending on the data collection under surveillance (CDI or bacteraemia), cases
#' are then apportioned to one of the following groups:
#'
#' \itemize{
#'  \item Healthcare onset - healthcare associated
#'  \item Community onset - healthcare associated
#'  \item Community onset - indeterminate association (CDI only)
#'  \item Community onset - community associated
#'  \item Community onset - missing
#'  \item Community onset - unknown
#' }
#'
#' @param data_collection String giving "CDI" or any other value - CDI has a different PTE rule
#' @param patient_location The patient's location at time of sample
#' @param patient_category The patient's category at time of sample
#' @param adm_date Date of admission
#' @param spec_date Date of specimen
#' @param adm_3_mo Was the patient an inpatient at the trust in previous three months. String yes, no or don't know
#' @param date_discharge date the patient was most recently discharged from the reporting trust
#' @param date_record_created date the record was entered onto the system
#'
#' @return A string variable giving the apportioning type; one of hoha, coha,
#'   coia or coca, all_blank if all of the prior healthcare exposures were NA,
#'   unknown_3_mo if prior health care in past three months was "Don't know" or
#'   NA if date record created < 01/04/2017
#'
#' @examples
#'
#' phc_dat <- data.frame(stringsAsFactors=FALSE,
#'  id = c(1, 2, 3, 4, 5),
#'  organism = rep("CDI", 5),
#'  patient_location = c("NHS acute trust", "NHS acute trust",
#'                       "NHS acute trust", "NHS acute trust",
#'                       "NHS acute trust"),
#'  patient_category = c("Inpatient", "Inpatient", "Inpatient",
#'                       "Inpatient", "Inpatient"),
#'  admission_date = lubridate::dmy(c("01/06/2019", "01/06/2019",
#'                                    "01/06/2019", "01/06/2019",
#'                                    "01/06/2019")),
#'  specimen_date = lubridate::dmy(c("01/06/2019", "01/06/2019",
#'                                   "01/06/2019", "01/06/2019",
#'                                   "01/06/2019")),
#'  patient_admitted_at_three_months = c("yes", "yes", "yes", "yes", "no"),
#'  date_most_recent_discharge = lubridate::dmy(c("31/05/2019", "21/05/2019",
#'                                                "03/05/2019", "08/03/2019", NA)),
#'  date_record_created = lubridate::dmy(c("15/06/2019", "15/06/2019",
#'                                         "15/06/2019", "15/06/2019",
#'                                         "15/06/2019"))
#'  )
#'
#' phc_dat$days_diff <- phc_dat$specimen_date - phc_dat$date_most_recent_discharge
#' phc_dat$apportion_phc_date <- apportion_phc_date(
#'     data_collection = phc_dat$organism,
#'     patient_location = phc_dat$patient_location,
#'     patient_category = phc_dat$patient_category,
#'     adm_date = phc_dat$admission_date,
#'     spec_date = phc_dat$specimen_date,
#'     adm_3_mo = phc_dat$patient_admitted_at_three_months,
#'     date_discharge = phc_dat$date_most_recent_discharge,
#'     date_record_created = phc_dat$date_record_created
#'     )
#' # view the results
#' phc_dat[, c("days_diff", "apportion_phc_date")]
#'
#' @export

apportion_phc_date <- function(data_collection,
  patient_location, patient_category, adm_date,
                               spec_date, adm_3_mo, date_discharge,
                               date_record_created){
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
  assertthat::assert_that(class(date_discharge) == "Date",
                          msg = "date_discharge must be a date")

  assertthat::assert_that(
    length(adm_3_mo[!(tolower(adm_3_mo) %in% c("yes", "no", "don't know", "", NA))]) == 0,
    msg = "adm_3_mo must be one of yes, no, don't know, \"\" or NA")

  days_diff <- spec_date - adm_date
  adm_3_mo <- tolower(adm_3_mo)
  adm_3_mo <- ifelse(adm_3_mo == "", NA_character_, adm_3_mo)

  hoha <- is_hoha(patient_location, patient_category, adm_date, spec_date, days_diff,
                  date_record_created)

  dis_diff <- spec_date - date_discharge

  inpatient <- inpatient(patient_location = patient_location,
                        patient_category = patient_category)

  # modified_apportionment <- ifelse(hoha == 0 & inpatient == 1 & days_diff == 2 &
  #                                      !is.na(days_diff), 1, 0)
  z <- ifelse(hoha == 1, "hoha", NA_character_)
  z <- ifelse(adm_3_mo == "yes" &
                suppressWarnings(dplyr::between(dis_diff, 0, 27)) &
                  hoha != 1, "coha", z)
  z <- ifelse(adm_3_mo == "yes" &
                suppressWarnings(dplyr::between(dis_diff, 28, 83)) &
                  hoha != 1, "coia", z)
  z <- ifelse(adm_3_mo == "yes" & dis_diff >= 84 & !is.na(dis_diff) &
                  hoha != 1, "coca", z)
  z <- ifelse((adm_3_mo == "" | is.na(adm_3_mo)) & is.na(dis_diff) &
                (hoha != 1 | is.na(hoha)), "all_blank", z)
  z <- ifelse(is.na(dis_diff) & (hoha != 1 | is.na(hoha)), "all_blank", z)
    # cases without prior hc admission and are not hoha are coca
  z <- ifelse(adm_3_mo == "no" & !is.na(adm_3_mo) & hoha != 1, "coca", z)
  z <- ifelse(hoha != 1 & (dis_diff < 0 | is.na(dis_diff)) &
                (tolower(adm_3_mo) != "no" & !is.na(adm_3_mo)), "unknown_3_mo", z)
  z <- ifelse(adm_3_mo == "don't know" & !is.na(adm_3_mo) &
                (z != "hoha" | is.na(z)),
                "unknown_3_mo", z)

  # fix Dec 2019 so that this can be used for bacteraemia collections
  z <- ifelse(
    !(data_collection %in% c("CDI", "C. difficile", "Clostridioides difficile")) & z == "coia",
    "coca", z
    )
  z <- ifelse(
    !(data_collection %in% c("CDI", "C. difficile", "Clostridioides difficile")) & z == "unknown_3_mo",
    "unknown_1_mo", z
  )

  return(z)
}


