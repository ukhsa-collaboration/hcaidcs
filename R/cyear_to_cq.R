#' Cyear to long calendar quarter
#'
#' Converts a five digit financial year into a long text string
#' @param cyear A numeric or string variable giving the calendar year and quarter, e.g. 20112 - July-September 2011
#' @return A text string giving the quarter and then the year.
#' @seealso \code{\link{kh03_year}}, \code{\link{fyear_to_fq}}
#' @examples
#' cyear_to_cq(20112)
#' @export

cyear_to_cq <- function(cyear){
  year <- substr(cyear, 1, 4)
  qtr <- substr(cyear, 5, 5)
  long_qtr <- ifelse(qtr == 2, "April-June",
                     ifelse(qtr == 3, "July-September",
                            ifelse(qtr == 4, "November-December",
                                   ifelse(qtr == 1, "January-March", NA))))
  z <- paste(long_qtr, year)
  return(z)
}
