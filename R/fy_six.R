#' Create six character financial year from date
#'
#' Takes date variables and returns six character financial years (e.g. 201213) for merging with kh03 and other data.
#' Needs further work to return ordered factor.
#' @param x A date character
#' @return Six character string giving the financial year, e.g. 201213
#' @examples
#' x <- lubridate::dmy("01-01-2012")
#' fy_six(x)
#' @export

fy_six <- function(x){
  fq <- fq_short(x)
  cyear <- lubridate::year(x)
  z <- ifelse(fq <= 3,
              paste0(cyear, substr(cyear + 1, 3, 4)),
              paste0(cyear - 1, substr(cyear, 3, 4))
              )
  return(z)
}
