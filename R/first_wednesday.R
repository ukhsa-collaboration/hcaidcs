#' First Wednesday
#'
#' Returns the first Wednesday of a month as a date in YYYY-MM-DD.
#' Used in monthly outputs for determining publication date.
#' Relies on lubridate for date calculations.
#' @param x A date
#' @return A date giving the first Wednesday of the month.
#' @export
#' @examples
#' x <- lubridate::dmy("01/01/1970")
#' first_wednesday(x)
#' weekdays(first_wednesday(x))

first_wednesday <- function(x){
  # Returns first wednesday of a month
  # from http://stackoverflow.com/a/13449787/2633645
  date <- lubridate::ymd(x)
  first <- lubridate::floor_date(date, "month")
  dow <- sapply(seq(0, 6), function(x) lubridate::wday(first + lubridate::days(x)))
  first_wed <- first + lubridate::days(which(dow == 4) - 1)
  return(first_wed)
}
