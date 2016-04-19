#' Get the number of days in the quarter of a financial year
#'
#' For working with KH03, quarters are in format "yyyyq".
#' Want number of days per quarter and this varies by leap year.
#' This method takes leap year into account.
#'
#' \strong{Note:} This function must be used in conjunction with \code{\link[dplyr]{rowwise()}} from dplyr or \code{\link[base]{sapply()}}.
#' @param  x A financial year quarter in format "yyyyq"
#' @return The number of days in the quarter, accounting for leap years.
#' @examples
#' days_in_fq("20131")
#' days_in_fq("20134")
#' # 2016 is a leap year, should have 91 days in quarter 1 (i.e. 20154)
#' days_in_fq("20154")
#' @export

days_in_fq <- function(x){
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("lubridate is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  the_year <- lubridate::dmy(paste0("01-01-", as.numeric(substr(x,1,4)) + 1 )) # plus one, because for fy 2014, q4 is jan-mar 2015
  the_qtr <- as.numeric(substr(x, 5,5))
  if(the_qtr == 4){
    days <- 31 + 28 + 31 + lubridate::leap_year(the_year)
  }else if(the_qtr == 1){
    days <- 30 + 31 +30
  }else if(the_qtr == 2){
    days <- 31 + 31 + 30
  }else if(the_qtr == 3){
    days <- 31 + 30 + 31
  }else{
    days <- NA
  }
  return(days)
}
