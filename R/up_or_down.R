#' Calculate whether there has been either an increase, decrease or no change
#' between two values
#'
#' @param from The stating value
#' @param to The final value
#' @return Text string, one of "decrease", "increase" or "no change"
#' @examples
#' up_or_down(100, 200)
#' up_or_down(200, 100)
#' up_or_down(100, 100)
#' @export

up_or_down <- function(from, to){
  z <- ifelse(to - from < 0, "decrease",
         ifelse(to - from > 0, "increase", "no change"))
  return(z)
}
