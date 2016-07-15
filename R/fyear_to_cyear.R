#' Convert financial year to calendar year
#'
#' Intended for the production of the fingertips annual data.
#' In fingertips the current year is the first year of the financial year.
#' i.e. for data from financial year 2015/16, the year is 2015.
#' It's worth noting that R will sort numbers stored as characters well.
#' @seealso \code{\link{cyear_to_fyear}}
#' @param x Numeric financial year
#' @examples
#' fyear_to_cyear(201213)
#' @return Character string giving the four digit calendar year
#' @export

fyear_to_cyear <- function(x){
  cyear <- substr(x,1,4)
  return(cyear)
}
