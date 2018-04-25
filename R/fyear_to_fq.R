#' Fyear to long financial quarter
#'
#' Converts a five digit financial year into a long text string
#' @param fyear A numeric or string variable giving the financial year and quarter, e.g. 20112 - July-September 2011
#' @return A text string giving the quarter and then the year.
#' @seealso \code{\link{kh03_year}}, \code{\link{cyear_to_cq}}
#' @examples
#' fyear_to_fq(20112)
#' @export

fyear_to_fq <- function(fyear){
  .Deprecated("fyq_to_fq")
  year <- substr(fyear, 1, 4)
  qtr <- substr(fyear, 5, 5)
  long_qtr <- ifelse(qtr == 1, "April-June",
                     ifelse(qtr == 2, "July-September",
                            ifelse(qtr == 3, "November-December",
                                   ifelse(qtr == 4, "January-March", NA))))
  z <- paste(long_qtr, year)
  return(z)
}

#' Fyear-quarter to long financial quarter
#'
#' Converts a five digit financial year and quarter into a long text string
#' @param fyq A numeric or string variable giving the finacial year and quarter, e.g. 20112 - July-September 2011
#' @return A text string giving the quarter and then the year.
#' @seealso \code{\link{kh03_year}}, \code{\link{cyear_to_cq}}
#' @examples
#' fyear_to_fq(20112)
#' @export
fyq_to_fq <- function(fyear){
  year <- substr(fyear, 1, 4)
  qtr <- substr(fyear, 5, 5)
  long_qtr <- ifelse(qtr == 1, "April-June",
                     ifelse(qtr == 2, "July-September",
                            ifelse(qtr == 3, "November-December",
                                   ifelse(qtr == 4, "January-March", NA))))
  z <- paste(long_qtr, year)
  return(z)
}
