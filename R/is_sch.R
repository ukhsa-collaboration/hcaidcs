#' Identifies specialist commissioning hubs
#'
#' Function to identify specialist commissioning hubs.
#' @param x A string giving the CCG name
#' @return A numeric vector giving 1 for a commissioning hub and zero for anything else.
#' @examples
#' x <- "YORKSHIRE AND HUMBER COMMISSIONING HUB"
#' is_sch(x)
#' x <- "NHS NEWCASTLE GATESHEAD CCG"
#' is_sch(x)
#' @export

is_sch <- function(x){
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("stringr is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  z <- ifelse(stringr::str_detect(stringr::str_to_lower(x), "hub"), 1, 0)
  return(z)
}
