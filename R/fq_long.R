#' Format date into long quarter.
#'
#' Formats date from date or POSIX class to a text string giving the the quarter
#' as 'January to March yyyy'
#'
#' @include check_date_class.R
#' @param date_var A date variable in class Date or POSIX
#' @return A text string giving the quarter and year.
#' @seealso \code{\link{fy_short}}
#' @seealso \code{\link{fy_long_short}}
#' @seealso \code{\link{ordered_fin_qtr}}
#' @examples
#' x <- lubridate::dmy("01/01/2001")
#' fq_long(x)
#' @export

fq_long <- function(date_var){
  qtr <- lubridate::quarter(date_var)
  year <- lubridate::year(date_var)
  text <- ifelse(qtr == 1, "January to March",
                 ifelse(qtr == 2, "April to June",
                        ifelse(qtr == 3, "July to September",
                               ifelse(qtr == 4, "October to December", NA))))
  text <- paste(text, year)
  return(text)
}
