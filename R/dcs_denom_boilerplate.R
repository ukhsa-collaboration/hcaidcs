#' Create population boilerplate columns for DCS denominator upload
#'
#' Adds columns that are common between all organisation types when preparing denominators for upload to DCS
#'
#' @param x a data frame to which columns will be added
#' @return A data frame with denominator_code, denominator_descriptionnotrequd,
#' denominator_type_code, range_from, range_to and per_factor
#' @export

dcs_popn_boilerplate <- function(x){
  x$denominator_code <- "POPULATION"
  x$denominator_descriptionnotrequd <- "Mid-year Population per year"
  x$denominator_type_code <- "POPULATION"
  x$denominator_type_descriptionnotd <- "Population"
  x$range_from <- "-"
  x$range_to <- "-"
  x$per_factor <- 100000
  return(x)
}
