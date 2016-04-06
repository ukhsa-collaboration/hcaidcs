#' Format fy_long into short financial year.
#'
#' Formats date from long financial as 'yyyyyy+1'.
#' Used for linking HCAI DCS data to formatted kh03 data.
#'
#' @include check_date_class.R
#' @param x A string variable giving long financial year created using
#' \code{\link{fy_long}}
#' @return A text string giving the financial year.
#' @seealso \code{\link{fy_long}}
#' @examples
#' x <- lubridate::dmy("01/01/2001")
#' x <- fy_long(x)
#' fy_long_short(x)
#' @export

fy_long_short <- function(x){
  y <- stringr::str_extract(x, "\\d{4}")
  y <- paste0(y, as.numeric(substr(y,3,4))+1)
  return(y)
}
