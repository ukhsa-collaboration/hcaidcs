#' Replicate the PIR assignment algorithm
#'
#' @param pircasestatus Text string of either 'Final assignment' or 'Provisional assignment'
#' @param assignmentmethodcode Numeric variable giving assignment method
#' @param patientlocation Text string giving patient location
#' @param provisionalorganisationname Text string giving the name of the provisionally assigned organisation
#' @param finalpirassignedorganisationtype Text string giving organisation type of finally assigned pir organisation. One of either "NHS Trust" or "Clinical Commissioning Group"
#' @return string giving the name of the finally assigned organisation or one of "NHS Trust" or "CCG"
#' @examples
#' dat <- structure(list(
#'   pircasestatus = c("Final assignment", "Final assignment","Final assignment",
#'                     "Final assignment", "Provisional assignment",
#'                     "Provisional assignment", "Provisional assignment",
#'                     "Provisional assignment", "Provisional assignment"),
#'   assignmentmethodcode = c(9L, 10L, 10L, 8L, 10L, 10L, 13L, 15L, 15L),
#'   patientlocation = c("NHS Acute Trust", "NHS Acute Trust",
#'                       "Independent Sector Provider",
#'                       "Independent Sector Provider",
#'                                           "NHS Acute Trust",
#'                       "Independent Sector Provider", "NHS Acute Trust",
#'                                           "NHS Acute Trust",
#'                       "NHS Acute Trust"),
#'   provisionalorganisationname = c(
#'     "Cambridge University Hospitals NHS Foundation Trust",
#'     "Cambridge University Hospitals NHS Foundation Trust",
#'     "Cambridge University Hospitals NHS Foundation Trust",
#'     "Cambridge University Hospitals NHS Foundation Trust",
#'     "Cambridge University Hospitals NHS Foundation Trust",
#'     "NHS Central London CCG",
#'     "Cambridge University Hospitals NHS Foundation Trust",
#'     "Cambridge University Hospitals NHS Foundation Trust",
#'     "NHS Central London CCG"),
#'   finalpirassignedorganisationtype = c(
#'     "NHS Trust",
#'     "NHS Trust",
#'     "Clinical Commissioning Group",
#'     "Clinical Commissioning Group",
#'     "NHS Trust",
#'     "NHS Trust",
#'     "NHS Trust",
#'     "NHS Trust",
#'     "NHS Trust")),
#'   .Names = c("pircasestatus", "assignmentmethodcode", "patientlocation",
#'              "provisionalorganisationname", "finalpirassignedorganisation"),
#'                  class = "data.frame", row.names = c(NA, -9L))
#'
#' dat$new <- assignment_algorithm(dat$pircasestatus,
#'          dat$assignmentmethodcode, dat$patientlocation, dat$provisionalorganisationname,
#'            dat$finalpirassignedorganisation)
#' dat
#' @export

assignment_algorithm <- function(pircasestatus, assignmentmethodcode, patientlocation,
                        provisionalorganisationname, finalpirassignedorganisationtype){
  z <- ifelse(
    tolower(pircasestatus) == "final assignment" & assignmentmethodcode <= 9,
    finalpirassignedorganisationtype,
    ifelse(
    (tolower(pircasestatus) == "final assignment" & assignmentmethodcode == 10 & patientlocation == "NHS Acute Trust") |
    (tolower(pircasestatus) == "provisional assignment" & (stringr::str_detect(tolower(provisionalorganisationname),"trust") == TRUE)) |
    (assignmentmethodcode == 15 & (stringr::str_detect(tolower(provisionalorganisationname),"trust") == TRUE)),
    "NHS Trust",
    ifelse(
      (tolower(pircasestatus) == "final assignment" & assignmentmethodcode == 10 & patientlocation != "NHS Acute Trust") |
        (tolower(pircasestatus) == "provisional assignment" & (stringr::str_detect(tolower(provisionalorganisationname),"ccg") == TRUE)) |
        (assignmentmethodcode == 15 & (stringr::str_detect(tolower(provisionalorganisationname),"ccg") == TRUE)), "Clinical Commissioning Group",
      ifelse(assignmentmethodcode == 13 | assignmentmethodcode == 14, "Third Party", NA))))
  return(z)
}
