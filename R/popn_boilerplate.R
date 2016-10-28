#' Adds population boilerplate text columns to a data frame in preparation of population denominator data for upload to DCS
#'
#' DCS denominator upload requires files in a certain format.
#' This is a function to add common variables to a data frame
#'
#' @param x A data frame to which columns will be added
#' @return A data frame with new columns added
#'
#' @examples
#' dat <- data.frame(trustcode = rep("AAA", 2), period = c(201415, 201516), stringsAsFactors = FALSE)
#' dat <- popn_boilerplate(dat)
#' @export

popn_boilerplate <- function(x){
  x$denominator_code <- "POPULATION"
  x$denominator_descriptionnotrequd <- "Mid-year Population per year"
  x$denominator_type_code <- "POPULATION"
  x$denominator_type_descriptionnotd <- "Population"
  x$range_from <- "-"
  x$range_to <- "-"
  x$per_factor <- 100000
  return(x)
}
