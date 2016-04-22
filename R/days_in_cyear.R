#' Get the number of days in a calendar year
#'
#' Need to obtain the number of days in a year
#'
#' @param  x A calendar year in format yyyy
#' @return The number of days in the calendar year
#' @examples
#' days_in_cy(2012)
#' days_in_cy(2011)
#' @export

days_in_cy <- function(x){
  days_in_yr <- ifelse(lubridate::leap_year(x), 366, 365)
  return(days_in_yr)
}
