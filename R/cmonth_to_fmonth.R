#' cmonth_to_fmonth
#'
#' Converts calendar months to financial year months
#' @param numeric_month A numeric value for a calendar month where January = 1
#' @return A numeric value where April = 1 and January = 10
#' @examples
#' cmonth_to_fmonth(1) # January
#' cmonth_to_fmonth(2) # February
#' cmonth_to_fmonth(4) # April
#' cmonth_to_fmonth(12) # December
#' @export

cmonth_to_fmonth <- function(numeric_month){
  z <- ifelse(numeric_month < 3,
              13 - numeric_month,
              numeric_month - 3)
  return(z)
}
