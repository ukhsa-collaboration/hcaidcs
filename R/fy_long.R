#' Format date into long text financial year.
#'
#' Formats date from date or POSIX class to a text string giving the financial
#' year as 'April yyyy to March yyyy'
#'
#' @include check_date_class.R
#' @param date_var A date variable in class Date or POSIX
#' @return A text string giving the financial year.
#' @seealso \code{\link{fy_short}}
#' @seealso \code{\link{fy_long_short}}
#' @examples
#' x <- lubridate::dmy("01/01/2001")
#' fy_long(x)
#' @export

fy_long <- function(date_var){
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("lubridate is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  check_date_class(date_var)
  qtr <- lubridate::quarter(date_var)
  this_year <- as.numeric(lubridate::year(date_var))
  if (qtr > 1) {
    start_year <- this_year
    end_year <- this_year+1
  } else {
    start_year <- this_year-1
    end_year <- this_year
  }
  z <- paste("April", start_year, "to March", end_year)
  return(z)
}
