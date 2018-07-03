#' Format date into short text financial quarter.
#'
#' Formats date from date or POSIX class to a text string giving the financial
#' quarter as integer from 1 to 4.
#'
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
  assertthat::assert_that(lubridate::is.Date(date_var),
               msg = "date_var must be a date")
  qtr <- lubridate::quarter(date_var)
  qtr <- ifelse(qtr == 1, 4, qtr - 1)
  return(qtr)
}

#' fq_short_to_date
#'
#' Convert short financial quarter to date
#' @seealso \code{\link{fq_short}}
#'
#' @param x A short financial year quarter e.g. 20171
#' @return The first day of the first month of the financial year quarter
#' @examples
#' fq_short_to_date(20124)
#' @export

fq_short_to_date <- function(x){
  # assertthat::assert_that(assertthat::are_equal(nchar(as.character(x)), 5),
  #                         msg = "x must be 5 digits long")

  qtr <- as.numeric(substr(x, 5, 5))
  year <- as.numeric(substr(x, 1, 4))
  month <- ifelse(qtr == 1, "April",
                  ifelse(qtr == 2, "July",
                         ifelse(qtr == 3, "October", "January")))
  cyear <- ifelse(qtr == 4, year + 1, year)
  z <- lubridate::dmy(paste0("01-", month, "-", cyear))
  return(z)
}
