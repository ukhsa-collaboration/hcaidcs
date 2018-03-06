#' Adds KH03 boilerplate text columns to a data frame in preparation of kh03 data for upload to DCS
#'
#' DCS denominator upload requires files in a certain format.
#' This is a function to add common variables to a data frame
#'
#' @param x A data frame to which columns will be added
#' @return A data frame with new columns added
#'
#' @examples
#' dat <- data.frame(trustcode = rep("AAA", 2), period = c(201415, 201516), stringsAsFactors = FALSE)
#' dat <- kh03_boilerplate(dat)
#' @export

kh03_boilerplate <- function(x){
  x$denominator_code <- "KH03BEDYR"
  x$denominator_descriptionnotrequd <- "KH03 Bed Days Year"
  x$denominator_type_code <- "BEDDAY"
  x$denominator_type_descriptionnotd <- "Bed Days"
  x$range_from <- 0
  x$range_to <- 0
  x$per_factor <- 100000
  return(x)
}
