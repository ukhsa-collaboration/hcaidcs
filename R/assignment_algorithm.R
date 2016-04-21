#' Replicate the PIR assignment algorithm
#'
#' @param pircasestatus Text string of either 'Final assignment' or 'Provisional assignment'
#' @param assignmentmethodcode Numeric variable giving assignment method
#' @param patientlocation Text string giving patient location
#' @param provisionalorganisationname Text string giving the name of the provisionally assigned organisation
#' @param finalpirassignedorganisation Text string giving name of finally assigned pir organisation
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
#'   finalpirassignedorganisation = c(
#'     "Cambridge University Hospitals NHS Foundation Trust",
#'     "Cambridge University Hospitals NHS Foundation Trust",
#'     "NHS Central London CCG",
#'     "NHS Central London CCG",
#'     "Cambridge University Hospitals NHS Foundation Trust",
#'     "Cambridge University Hospitals NHS Foundation Trust",
#'     "Cambridge University Hospitals NHS Foundation Trust",
#'     "Cambridge University Hospitals NHS Foundation Trust",
#'     "Cambridge University Hospitals NHS Foundation Trust")),
#'   .Names = c("pircasestatus", "assignmentmethodcode", "patientlocation",
#'              "provisionalorganisationname", "finalpirassignedorganisation"),
#'                  class = "data.frame", row.names = c(NA, -9L))
#' dat$new <- assignment_algorithm(dat$pircasestatus, dat$assignmentmethodcode,
#'            dat$patientlocation, dat$provisionalorganisationname,
#'            dat$finalpirassignedorganisation)
#' dat
#' @export

assignment_algorithm <- function(pircasestatus, assignmentmethodcode, patientlocation,
                        provisionalorganisationname, finalpirassignedorganisation){
  z <- ifelse(is_third_party(assignmentmethodcode) == TRUE, "Third party",
              ifelse(assignmentmethodcode == 15, provis_orgname(provisionalorganisationname),
                      ifelse(pircasestatus == "Provisional assignment", provis_orgname(provisionalorganisationname),
                             ifelse(pircasestatus == "Final assignment",
                                    ifelse(assignmentmethodcode >= 9 & assignmentmethodcode <= 11, finalpirassignedorganisation,
                                           ifelse(assignmentmethodcode == 10, patloc_for_pir_assignment(patientlocation), NA)),
                                    NA))))
  return(z)
}
