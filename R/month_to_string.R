#' cmonth_to_string
#'
#' Convert numeric calendar month to string.
#' Mostly important for monthly reports
#'
#' @param x A numeric monthly value where 1 = January
#' @return A string giving month name
#' @examples
#' cmonth_to_string(1)
#' cmonth_to_string(12)
#' cmonth_to_string(13)
#' @export

cmonth_to_string <- function(x){
  z <- as.Date(paste0("01-", sprintf("%02d", x), "-2016"), format = "%d-%m-%Y")
  z <- format(z, "%B")
  return(z)
}

#' fmonth_to_string
#'
#' Convert numeric FY month to string.
#' Mostly important for monthly reports
#'
#' @param x A numeric monthly value where 1 = April
#' @return A string giving month name
#' @examples
#' #April
#' fmonth_to_string(1)
#' fmonth_to_string(10)
#' fmonth_to_string(11)
#' # March
#' fmonth_to_string(12)
#' @export

fmonth_to_string <- function(x){
  y <- ifelse(x > 9, as.numeric(substring(as.character(x), 2, 2)) + 1, x + 3)
  y <- ifelse(x > 12, NA_real_, y)
  z <- format(as.Date(paste0("01-", sprintf("%02d", as.integer(y)), "-2016"),
                      format = "%d-%m-%Y"), "%B")
  return(z)
}
