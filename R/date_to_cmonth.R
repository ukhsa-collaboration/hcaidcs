#' date_to_cmonth
#'
#' Convert a date to a calendar month, either as a numeric value, or as an ordered
#' factor.
#'
#' @param x A date
#' @param as_factor TRUE or FALSE, defaults to FALSE
#' @return Either a numeric value giving the month of the year (1 = January) or a factor with the name of the month of the year.
#' @export
#' @examples
#'
#' x <- as.Date("01/01/2019", format = "%d/%m/%Y")
#' date_to_cmonth(x)
#' date_to_cmonth(x, as_factor = TRUE)

date_to_cmonth <- function(x, as_factor = FALSE){
  assertthat::assert_that(lubridate::is.Date(x), msg = "x must be a date")
  assertthat::assert_that(as_factor %in% c(TRUE, FALSE),
             msg = "as_factor must be either TRUE or FALSE")

  if(as_factor == FALSE){
    x <- as.numeric(format(x, "%m"))
  }else if(as_factor == TRUE){
    x <- factor(format(x, "%B"), levels = month.name)
  }
  return(x)
}
