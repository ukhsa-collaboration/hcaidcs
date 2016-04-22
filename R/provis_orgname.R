#' Check whether provisional organisation is Trust or CCG
#'
#' @param x A numeric value from assignment method
#' @return "Trust" or "CCG"
#' @examples
#' \dontrun{
#' provis_orgname("Cambridge University Hospitals NHS Foundation Trust")
#' provis_orgname("NHS Central London CCG")
#' }

provis_orgname <- function(x){
  z <- ifelse(grepl("trust", tolower(x), fixed = TRUE) == TRUE, "NHS Trust",
              ifelse(grepl("ccg", tolower(x), fixed = TRUE) == TRUE, "CCG", NA))
  return(z)
}
