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
  .Deprecated("up_or_down", package = "nicethings",
              msg = "I've moved up_or_down out of the hcaidcs package as it is a better fit for the nicethings package")
  z <- ifelse(to - from < 0, "decrease",
         ifelse(to - from > 0, "increase", "no change"))
  return(z)
}
