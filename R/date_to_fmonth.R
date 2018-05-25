#' date_to_fmonth
#'
#' Convert dates to months of financial year
#'
#' @param x a date value
#' @return a numeric value for the month of financial year, e.g. 1 for April, and 12 for March
#'
#' @examples
#' date_to_fmonth(lubridate::dmy("01-01-2018"))
#' date_to_fmonth(lubridate::dmy("01-02-2018"))
#' date_to_fmonth(lubridate::dmy("01-03-2018"))
#' date_to_fmonth(lubridate::dmy("01-04-2018"))
#' date_to_fmonth(lubridate::dmy("01-12-2018"))
#' @export

date_to_fmonth <- function(x){
  assertthat::assert_that(assertthat::is.date(x) == TRUE,
                          msg = "x must be a date")
  cmonth <- as.numeric(format(x, "%m"))
  fmonth <- ifelse(cmonth < 4, cmonth + 9,
                   cmonth - 3)
  return(fmonth)
}
