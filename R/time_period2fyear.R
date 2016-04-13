#' Convert time period variable to fyear for merging with KH03 data
#'
#' HCAI DCS QMLR data sets include a time period variable in the format "qqq-qqq yyyy".
#' To merge with KH03 data, this needs to be converted to "yyyyq".
#' For fyear, q1 is April-June and q4 is Jan-Feb.
#'
#' @param x A text string, in format "qqq-qqq yyyy"
#' @return A text string giving the quarter in fyear format.
#' @examples
#' x <- "Oct-Dec 2015"
#' time_period2fyear(x)
#' @export

time_period2fyear <- function(x){
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("stringr is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  this_year <- stringr::str_extract(x, "\\d{4}$")
  text_quarter <- gsub("(.*?)\\s\\d{4}$", "\\1", x)
  q <- ifelse(text_quarter == "Apr-Jun", 1,
              ifelse(text_quarter == "Jul-Sep", 2,
                     ifelse(text_quarter == "Oct-Dec", 3,
                            ifelse(text_quarter == "Jan-Feb", 4, NA))))
  fyear <- paste0(this_year, q)
  return(fyear)
}
