#' Convert a date to five-character financial quarter
#'
#' Takes a date and returns a five character financial year and quarter (i.e. yyyyq).
#'
#' @param x A date variable
#' @return A numeric value with five digits giving the financial year in the first four digits and the quarter in the final digit.
#' @examples
#' x <- lubridate::dmy("01/01/2012")
#' fq5(x)
#' x <- lubridate::dmy("01/04/2012")
#' fq5(x)
#' @export

fq5 <- function(x){
  qtr <- fq_short(x)
  yr <- ifelse(qtr == 4,
               lubridate::year(x) - 1,
               lubridate::year(x))
  z <- as.numeric(paste0(yr, qtr))
  return(z)
}
