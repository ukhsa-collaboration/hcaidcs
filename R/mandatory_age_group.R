#' Create age group from age
#'
#' Creates age group as ordered factor according to standard ages for HCAIDCS
#'
#' @param x a numeric vector
#' @return A factor vector with levels <1 1-14 15-44 45-64 65-74 75-84 ge85
#' @seealso \code{\link{mandatory_age}}
#' @examples
#' mandatory_age_group(0)
#' mandatory_age_group(14)
#' mandatory_age_group(15)
#' mandatory_age_group(seq(1,100, 1))
#' @export

mandatory_age_group <- function(x){
  stopifnot(is.numeric(x) == TRUE)
  z <- cut(x, breaks = c(-Inf, 1, 14, 44, 64, 74, 84, Inf),
           labels = c("<1", "1-14", "15-44", "45-64", "65-74", "75-84", "ge85"))
  return(z)
}


