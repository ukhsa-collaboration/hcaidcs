#' Replicate the PIR assignment algorithm
#'
#' Provisionally assigned cases are counted as if final assignment status, i.e.
#' a case provisionally assigned to a Trust will be reported as Trust-assigned
#' in any report.
#'
#' @param pircasestatus Text string of either 'Final assignment' or 'Provisional assignment'
#' @param assignmentmethodcode Numeric variable giving assignment method
#' @param patientlocation Text string giving patient location
#' @param patientcategory Text string giving patient inpatient status (one of "", "A&E only", "Day patient", "Emergency Assessment" "In-patient", "Other", "Outpatient", "Regular Attender", "Unknown" )
#' @param provisionalorganisationname Text string giving the name of the provisionally assigned organisation
#' @param finalpirassignedorganisationtype Text string giving organisation type of finally assigned pir organisation. One of either "NHS Trust" or "Clinical Commissioning Group"
#' @return string giving the name of the finally assigned organisation or one of "NHS Trust" or "CCG"
#' @examples
#'
#' data(assignment_data)
#' assignment_data$new <- assignment_algorithm(pircasestatus = assignment_data$pircasestatus,
#'         assignmentmethodcode = assignment_data$assignmentmethodcode,
#'         patientlocation = assignment_data$patientlocation,
#'         patientcategory = assignment_data$patient_category,
#'         provisionalorganisationname = assignment_data$provisionalorganisationname,
#'         finalpirassignedorganisationtype = assignment_data$finalpirassignedorganisation)
#' assignment_data
#'
#' dat2 <- assignment_data[13, ]
#' dat2$new <- NULL
#' dat2$new <- assignment_algorithm(pircasestatus = dat2$pircasestatus,
#'         assignmentmethodcode = dat2$assignmentmethodcode,
#'         patientlocation = dat2$patientlocation,
#'         patientcategory = dat2$patient_category,
#'         provisionalorganisationname = dat2$provisionalorganisationname,
#'         finalpirassignedorganisationtype = dat2$finalpirassignedorganisation)
#' dat2
#'
#' data(assignment_data2)
#' assignment_data2$new <- assignment_algorithm(pircasestatus = assignment_data2$pir_case_status,
#'         assignmentmethodcode = assignment_data2$assignment_method_code,
#'         patientlocation = assignment_data2$patient_location,
#'         patientcategory = assignment_data2$patient_category,
#'         provisionalorganisationname = assignment_data2$provisional_organisation_name,
#'         finalpirassignedorganisationtype = assignment_data2$final_pir_assigned_organisation_type)
#' assignment_data2
#' @export

assignment_algorithm <- function(pircasestatus, assignmentmethodcode, patientlocation,
                                 patientcategory, provisionalorganisationname,
                                 finalpirassignedorganisationtype){
  # Factors will stuff up the assignment algorithm.
  pircasestatus <- tolower(as.character(pircasestatus))
  patientlocation <- as.character(patientlocation)
  provisionalorganisationname <- tolower(as.character(provisionalorganisationname))
  finalpirassignedorganisationtype <- tolower(as.character(finalpirassignedorganisationtype))
  trust_pat_locs <- c("In-patient", "Day patient", "Emergency Assessment", "Unknown", "")
  z <- ifelse(
    pircasestatus == "final assignment" & assignmentmethodcode <= 9,
    finalpirassignedorganisationtype,
    ifelse(
      ((pircasestatus == "final assignment" & assignmentmethodcode == 10 & patientlocation == "NHS Acute Trust") |
        (pircasestatus == "provisional assignment" & (stringr::str_detect(provisionalorganisationname,"trust") == TRUE)) & !(assignmentmethodcode %in% c(13, 14)) |
        (assignmentmethodcode %in% 11 & # %in% necessary here to get FALSE for NA values
           (patientlocation == "NHS Acute Trust" & patientcategory %in% trust_pat_locs)
           ) |
        (assignmentmethodcode %in% 15 & stringr::str_detect(provisionalorganisationname,"trust") == TRUE)),
      "NHS Trust",
      ifelse(
        (pircasestatus == "final assignment" & assignmentmethodcode == 10 & patientlocation != "NHS Acute Trust") |
          (pircasestatus == "provisional assignment" & stringr::str_detect(provisionalorganisationname,"ccg") == TRUE) & !(assignmentmethodcode %in% c(13, 14)) |
          (assignmentmethodcode %in% c(11) &
             (patientlocation == "NHS Acute Trust" & !(patientcategory %in% trust_pat_locs))) |
          ((assignmentmethodcode %in% c(11)) & patientlocation != "NHS Acute Trust")  |
          (assignmentmethodcode %in% c(15) & stringr::str_detect(provisionalorganisationname,"trust") == FALSE),
        "Clinical Commissioning Group",
        ifelse(assignmentmethodcode %in% c(13, 14),
               "Third Party", "something else")
        ))
    )
  z <- ifelse(z == "nhs trust", "NHS Trust", z)
  z <- ifelse(z == "clinical commissioning group", "Clinical Commissioning Group", z)
  return(z)
}


# assignment_algorithm <- function(pircasestatus, assignmentmethodcode, patientlocation,
#                                  provisionalorganisationname, finalpirassignedorganisationtype){
#   # Factors will stuff up the assignment algorithm.
#   pircasestatus <- as.character(pircasestatus)
#   patientlocation <- as.character(patientlocation)
#   provisionalorganisationname <- as.character(provisionalorganisationname)
#   finalpirassignedorganisationtype <- as.character(finalpirassignedorganisationtype)
#   z <- ifelse(
#     tolower(pircasestatus) == "final assignment" & assignmentmethodcode <= 9,
#     finalpirassignedorganisationtype,
#     ifelse(
#       (tolower(pircasestatus) == "final assignment" & assignmentmethodcode == 10 & patientlocation == "NHS Acute Trust") |
#         (tolower(pircasestatus) == "provisional assignment" & (stringr::str_detect(tolower(provisionalorganisationname),"trust") == TRUE)) |
#         ( assignmentmethodcode %in% c(11, 15) & (stringr::str_detect(tolower(provisionalorganisationname),"trust") == TRUE)),
#       "NHS Trust",
#       ifelse(
#         (tolower(pircasestatus) == "final assignment" & assignmentmethodcode == 10 & patientlocation != "NHS Acute Trust") |
#           (tolower(pircasestatus) == "provisional assignment" & (stringr::str_detect(tolower(provisionalorganisationname),"ccg") == TRUE)) |
#           ( assignmentmethodcode %in% c(11, 15) & (stringr::str_detect(tolower(provisionalorganisationname),"ccg") == TRUE)),
#         "Clinical Commissioning Group",
#         ifelse(assignmentmethodcode == 13 | assignmentmethodcode == 14, "Third Party", NA))))
#   return(z)
# }






