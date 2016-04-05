#' Create ordered factor variable for PIR organisation type.
#'
#' As imported, PIR organisation types are character variables.
#' For the correct ordering when reshaping wide, this needs to be a factor variable.
#'
#' @param x The column of a data frame giving the PIR organisation type.
#' @examples
#' x <- data.frame(pir = c("", "NHS Trust", "Clinical Commissioning Group",
#' "Total Reported Cases", NA))
#'
#' x$pir2 <- pir_org_type(x$pir)
#' @export

pir_org_type <- function(x){
  z <- ifelse(x == "", "Third Party Cases",
              ifelse(x == "NHS Trust", "Trust Assigned Cases",
                     ifelse(x == "Clinical Commissioning Group", "CCG Assigned Cases",
                            ifelse(x == "Total Reported Cases", "Total Reported Cases", NA))))
  z <- factor(z, levels = c("Total Reported Cases", "Trust Assigned Cases",
                            "CCG Assigned Cases", "Third Party Cases"))
  return(z)
}
