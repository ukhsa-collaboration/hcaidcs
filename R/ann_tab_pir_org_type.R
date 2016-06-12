#' Format pir org type for annual tables
#'
#' @param x A string vector giving the final pir assignment type
#' @return A factor variable with four levels: Total Reported Cases,
#' Trust Assigned Cases, CCG Assigned Cases and Third Party Cases
#' @examples
#' x <- "NHS Trust"
#' ann_tab_pir_org_type(x)
#' @export

ann_tab_pir_org_type <- function(x){
  z <- ifelse(x == "Third Party", "Third Party Cases",
              ifelse(x == "NHS Trust", "Trust Assigned Cases",
                     ifelse(x == "Clinical Commissioning Group", "CCG Assigned Cases",
                            ifelse(x == "Total Reported Cases", "Total Reported Cases", NA))))
  z <- factor(z, levels = c("Total Reported Cases", "Trust Assigned Cases",
                            "CCG Assigned Cases", "Third Party Cases"))
  return(z)
}
