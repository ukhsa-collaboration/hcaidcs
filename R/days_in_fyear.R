#' Get the number of days in a financial year
#'
#' For working with KH03, years between 2007/08 and 2009/10 are in format "y1y1y1y1y2y2".
#' Need to obtain the number of days in a year
#'
#' @param  x A financial year in format "y1y1y1y1y2y2"
#' @return The number of days in the financial year, from 1st April to 31st March, as a numeric value.
#' @examples
#' days_in_fy("200708")
#' @export

days_in_fy <- function(x){
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("lubridate is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  year_one <- lubridate::dmy(paste0("01-04-", substr(x, 1,4)))
  if(year_one < as.Date("1999-04-01") ){
    year_two <- lubridate::dmy(paste0("31-03-19", substr(x, 5,6)))
  }else {
    year_two <- lubridate::dmy(paste0("31-03-20", substr(x, 5,6)))
  }
  days_in_yr <- as.numeric(year_two - year_one) + 1
  return(days_in_yr)
}
