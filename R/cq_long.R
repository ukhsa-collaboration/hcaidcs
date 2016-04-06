#' Format date into calendar year quarter.
#'
#' Formats date from date or POSIX class to a text string giving the calendar
#' quarter
#'
#' @include check_date_class.R
#' @param date_var A date variable in class Date or POSIX
#' @return A text string giving the quarter and calendar year.
#' @seealso \code{\link{fy_long}}
#' @examples
#' x <- lubridate::dmy("01/01/2001")
#' cal_q_long(x)
#' @export

cal_q_long <- function(date_var){
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("lubridate is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  check_date_class(date_var)
  qtr <- lubridate::quarter(date_var)
  year <- lubridate::year(date_var)
  text <- ifelse(qtr == 1, "January to March",
                 ifelse(qtr == 2, "April to June",
                        ifelse(qtr == 3, "July to September",
                               ifelse(qtr == 4, "October to December", NA))))
  text <- paste(text, year)
  return(text)
}
