#' Check whether a patient location is an NHS Trust or not
#'
#' Not exported, called from assignment function.
#' @param x A string value giving the patient location
#' @return "NHS Trust" or "CCG"
#' @examples
#' \dontrun{
#' patloc_for_pir_assignment("NHS Acute Trust")
#' patloc_for_pir_assignment("Non-acute NHS Provider")
#' }

patloc_for_pir_assignment <- function(x){
  z <- ifelse(tolower(x) == "nhs acute trust", "NHS Trust", "CCG")
  return(z)
}
