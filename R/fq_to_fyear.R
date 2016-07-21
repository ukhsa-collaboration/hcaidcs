#' Convert financial quarter to six-character financial year
#'
#' Takes financial quarter in format yyyyq and returns yyyy/yy format financial year.
#' @param x Either string or numeric value financial year/quarter, e.g. 20151 = April-June 2015
#' @return Numeric value for financial year, e.g. 201516
#' @examples
#' fq_to_fyear(20151)
#' @export

fq_to_fyear <- function(x){
  yr_one <- as.numeric(substr(as.character(x), 1, 4))
  yr_two <- substr(as.character(yr_one + 1), 3, 4)
  z <- as.numeric(paste0(yr_one, yr_two))
  return(z)
}
