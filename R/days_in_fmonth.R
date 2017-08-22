#' Calculates number of days in a financial month.
#'
#' Used for calculating denominators in monthly scripts.
#' @param month_no numeric month of financial year, where 1 = April
#' @param fyear 4 character financial year where 2012 is year 2012/13
#' @examples
#' days_in_fmonth(1, 2017)
#' x <- days_in_fmonth(1, 2017)
#' class(x)
#' days_in_fmonth(11, 2012) # February on a leap year
#' days_in_fmonth(11, 2013) # February on an ordinary year
#' @return numeric value for number of days in month
#' @export

days_in_fmonth <- function(month_no, fyear){
  z <- dplyr::case_when(
    month_no == 1  ~ 30, # April
    month_no == 2  ~ 31,
    month_no == 3  ~ 30,
    month_no == 4  ~ 31,
    month_no == 5  ~ 31,
    month_no == 6  ~ 30,
    month_no == 7  ~ 31,
    month_no == 8  ~ 30,
    month_no == 9  ~ 31,
    month_no == 10 ~ 31, # January
    month_no == 11 ~ 28 + lubridate::leap_year(lubridate::dmy(paste0("01/01/",
                                                                     fyear + 1))),
    month_no == 12 ~ 31  # March
  )
  return(z)
}
