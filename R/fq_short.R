#' Format date into short text financial quarter.
#'
#' Formats date from date or POSIX class to a text string giving the financial
#' quarter as integer from 1 to 4.
#'
#' @include check_date_class.R
#' @param date_var A date variable in class Date or POSIX
#' @return A text string giving the financial quarter, where April to June is
#' quarter 1 and January to March is quarter 4.
#' @examples
#' x <- lubridate::dmy("01/01/2001")
#' fq_short(x)
#' @export

fq_short <- function(date_var){
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("lubridate is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  check_date_class(date_var)
  qtr <- lubridate::quarter(date_var)
  qtr <- ifelse(qtr == 1, 4, qtr - 1)
  return(qtr)
}
