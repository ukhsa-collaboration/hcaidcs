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
#' dat <- structure(list(pircasestatus = c("Final assignment", "Final assignment",
#' "Final assignment", "Final assignment", "Provisional assignment",
#' "Provisional assignment", "Provisional assignment", "Provisional assignment",
#' "Provisional assignment", "Provisional assignment", "Provisional assignment",
#' "Provisional assignment", "Provisional assignment", "Provisional assignment"),
#' assignmentmethodcode = c(9L, 10L, 10L, 8L, 10L, 10L, 13L,15L, 15L, 11L, 11L, 11L, NA, NA),
#' patientlocation = c("NHS Acute Trust", "NHS Acute Trust",
#' "Independent Sector Provider", "Independent Sector Provider",
#'     "NHS Acute Trust", "Independent Sector Provider", "NHS Acute Trust",
#'     "NHS Acute Trust", "NHS Acute Trust", "NHS Acute Trust", "NHS Acute Trust",
#'     "NHS Acute Trust", "NHS Acute Trust", "NHS Acute Trust"),
#' provisionalorganisationname = c("Cambridge University Hospitals NHS Foundation Trust",
#'                                                                "Cambridge University Hospitals NHS Foundation Trust", "Cambridge University Hospitals NHS Foundation Trust",
#'                                                                "Cambridge University Hospitals NHS Foundation Trust", "Cambridge University Hospitals NHS Foundation Trust",
#'                                                                "NHS Central London CCG", "Cambridge University Hospitals NHS Foundation Trust",
#'                                                                "Cambridge University Hospitals NHS Foundation Trust", "NHS Central London CCG",
#'                                                                "Cambridge University Hospitals NHS Foundation Trust", "NHS Central London CCG", "NHS Central London CCG",
#'                                                                "NHS Central London CCG", "NHS Central London CCG"
#'                             ), finalpirassignedorganisation = c("NHS Trust", "NHS Trust",
#'                                                                 "Clinical Commissioning Group", "Clinical Commissioning Group",
#'                                                                 "NHS Trust", "NHS Trust", "NHS Trust", "NHS Trust", "NHS Trust",
#'                                                                 "NHS Trust", "NHS Trust", "NHS Trust", "NHS Trust", "NHS Trust"),
#'                              patient_category = c("In-patient", "In-patient", "In-patient", "In-patient", "In-patient", "In-patient",
#'                              "In-patient", "In-patient", "In-patient", "In-patient", "A&E only", "In-patient", "In-patient", "In-patient"
#'                              )), .Names = c("pircasestatus", "assignmentmethodcode", "patientlocation",
#'                                                                                "provisionalorganisationname", "finalpirassignedorganisation",
#'                                                                                "patient_category"), row.names = c(NA, -14L), class = "data.frame")
#'
#' dat$new <- assignment_algorithm(pircasestatus = dat$pircasestatus,
#'         assignmentmethodcode = dat$assignmentmethodcode,
#'         patientlocation = dat$patientlocation,
#'         patientcategory = dat$patient_category,
#'         provisionalorganisationname = dat$provisionalorganisationname,
#'         finalpirassignedorganisationtype = dat$finalpirassignedorganisation)
#' dat
#'
#' dat2 <- dat[13, ]
#' dat2$new <- NULL
#' dat2$new <- assignment_algorithm(pircasestatus = dat2$pircasestatus,
#'         assignmentmethodcode = dat2$assignmentmethodcode,
#'         patientlocation = dat2$patientlocation,
#'         patientcategory = dat2$patient_category,
#'         provisionalorganisationname = dat2$provisionalorganisationname,
#'         finalpirassignedorganisationtype = dat2$finalpirassignedorganisation)
#' dat2
#'
#' dat3 <- structure(list(pir_case_status = c("Provisional Assignment",
#' "Provisional Assignment", "Provisional Assignment", "Provisional Assignment",
#' "Provisional Assignment", "Provisional Assignment", "Provisional Assignment",
#' "Provisional Assignment", "Provisional Assignment", "Provisional Assignment",
#' "Provisional Assignment", "Provisional Assignment", "Provisional Assignment",
#' "Provisional Assignment", "Provisional Assignment"), assignment_method_code = c(NA_real_,
#'                                                                                 NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
#'                                                                                 NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_
#' ), patient_location = c("NHS Acute Trust", "NHS Acute Trust",
#'                         "NHS Acute Trust", "NHS Acute Trust", "NHS Acute Trust", "NHS Acute Trust",
#'                         "NHS Acute Trust", "NHS Acute Trust", "NHS Acute Trust", "NHS Acute Trust",
#'                         "NHS Acute Trust", "NHS Acute Trust", "NHS Acute Trust", "NHS Acute Trust",
#'                         "NHS Acute Trust"), patient_category = c("In-patient", "In-patient",
#'                                                                  "In-patient", "Emergency Assessment", "In-patient", "Emergency Assessment",
#'                                                                  "In-patient", "In-patient", "In-patient", "In-patient", "Emergency Assessment",
#'                                                                  "In-patient", "In-patient", "Emergency Assessment", "Day patient"
#'                         ), provisional_organisation_name = c("NHS TOWER HAMLETS CCG",
#'                                                              "NHS CHILTERN CCG", "NHS GLOUCESTERSHIRE CCG", "NHS BRISTOL CCG",
#'                                                              "NHS DARTFORD, GRAVESHAM AND SWANLEY CCG", "NHS CRAWLEY CCG",
#'                                                              "NHS BRACKNELL AND ASCOT CCG", "NHS SOMERSET CCG", "NHS SOUTH GLOUCESTERSHIRE CCG",
#'                                                              "NHS HIGH WEALD LEWES HAVENS CCG", "NHS BRISTOL CCG", "NHS TOWER HAMLETS CCG",
#'                                                              "NHS NORTHUMBERLAND CCG", "NHS BRISTOL CCG", "NHS WEST ESSEX CCG"
#'                         ), final_pir_assigned_organisation_type = c("", "", "", "", "",
#'                                                                     "", "", "", "", "", "", "", "", "", ""), id = 1:15), .Names = c("pir_case_status",
#'                                                                                                                                     "assignment_method_code", "patient_location", "patient_category",
#'                                                                                                                                     "provisional_organisation_name", "final_pir_assigned_organisation_type",
#'                                                                                                                                     "id"), row.names = c(NA, -15L), class = "data.frame")
#' dat3$new <- assignment_algorithm(pircasestatus = dat3$pir_case_status,
#'         assignmentmethodcode = dat3$assignment_method_code,
#'         patientlocation = dat3$patient_location,
#'         patientcategory = dat3$patient_category,
#'         provisionalorganisationname = dat3$provisional_organisation_name,
#'         finalpirassignedorganisationtype = dat3$final_pir_assigned_organisation_type)
#' dat3
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
        (pircasestatus == "provisional assignment" & (stringr::str_detect(provisionalorganisationname,"trust") == TRUE)) |
        (assignmentmethodcode %in% 11 & # %in% necessary here to get FALSE for NA values
           (patientlocation == "NHS Acute Trust" & patientcategory %in% trust_pat_locs)
           ) |
        (assignmentmethodcode %in% 15 & stringr::str_detect(provisionalorganisationname,"trust") == TRUE)),
      "NHS Trust",
      ifelse(
        (pircasestatus == "final assignment" & assignmentmethodcode == 10 & patientlocation != "NHS Acute Trust") |
          (pircasestatus == "provisional assignment" & stringr::str_detect(provisionalorganisationname,"ccg") == TRUE) |
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






