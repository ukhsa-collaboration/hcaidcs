#' Check whether assignment is third party
#'
#' @param x A numeric value from assignment method
#' @return TRUE or FALSE
#' @examples
#' is_third_party(9)
#' is_third_party(13)

is_third_party <- function(x){
  z <- x == 13 | x == 14
  return(z)
}
