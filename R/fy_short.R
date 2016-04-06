#' Format date into short text financial year.
#'
#' Formats date from date or POSIX class to a text string giving the financial
#' year as 'yy/yy'
#'
#' @include check_date_class.R
#' @param date_var A date variable in class Date or POSIX
#' @return A text string giving the financial year.
#' @seealso \code{\link{fy_long}}
#' @examples
#' x <- lubridate::dmy("01/01/2001")
#' fy_short(x)
#' @export

fy_short <- function(date_var){
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("lubridate is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  check_date_class(date_var)
  qtr <- lubridate::quarter(date_var)
  date_var <- as.Date(date_var)
  this_year <- as.numeric(substr(as.character(lubridate::year(date_var)), 3, 4))
  if (qtr > 1) {
    z <- paste(sprintf("%02i", this_year), sprintf("%02i", this_year + 1), sep = "/" )
  } else {
    z <- paste(sprintf("%02i", this_year - 1), sprintf("%02i", this_year), sep = "/" )
  }
  return(z)
}
