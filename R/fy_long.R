#' Format date into long text financial year.
#'
#' Formats date from date or POSIX class to a text string giving the financial
#' year as 'April yyyy to March yyyy'
#'
#' @param date_var A date variable in class Date or POSIX
#' @return A text string giving the financial year.
#' @seealso \code{\link{fy_short}}
#' @seealso \code{\link{fy_long_short}}
#' @seealso \code{\link{ordered_fin_qtr}}
#' @seealso \code{\link{fy_six_to_long}}
#' @examples
#' x <- lubridate::dmy("01/01/2001")
#' fy_long(x)
#' x <- lubridate::ymd("2016-01-01")
#' fy_long(x)
#' @export

fy_long <- function(date_var){
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("lubridate is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  assertthat::assert_that(lubridate::is.Date(date_var),
                          msg = "date_var is not a date")
  qtr <- lubridate::quarter(date_var)
  this_year <- as.numeric(lubridate::year(date_var))
  start_year <- ifelse(qtr > 1, this_year, this_year - 1)
  end_year <- ifelse(qtr > 1, this_year + 1, this_year)
  z <- paste("April", start_year, "to March", end_year)
  return(z)
}

#' Format date into long text financial year.
#'
#' Converts six-character financial year to long financial
#' year as 'April yyyy to March yyyy'.
#' Assumes that all dates will be >= 2000
#'
#' @param x Either a numeric or character variable giving the financial year as
#' YYYYYY or YYYY/YY
#' @return A text string giving the financial year.
#' @seealso \code{\link{fy_short}}
#' @seealso \code{\link{fy_long_short}}
#' @seealso \code{\link{ordered_fin_qtr}}
#' @seealso \code{\link{fy_long}}
#' @examples
#' x <- 201718
#' fy_six_to_long(x)
#' x <- "2017/18"
#' fy_six_to_long(x)
#' @export

fy_six_to_long <- function(x){
  x <- as.character(x)
  x <- gsub("/", "", x)
  start_year <- substr(x, 1, 4)
  end_year <- substr(x, 5, 6)
  z <- paste0("April ", start_year, " to March 20", end_year)
  return(z)
}
