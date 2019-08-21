#' Gives the absolute value of the per cent or total change between two values
#'
#' @param from The starting value
#' @param to The finishing value
#' @param type Either "percent" or "value"
#' @return A numeric value giving the percent change or absolute change between the two values
#' @examples
#' change(200, 100, "percent")
#' change(200, 100, "value")
#' change(100, 200, "percent")
#' @export

change <- function(from, to, type){
  z <- ifelse(type == "percent", round( abs(((to - from)/from) * 100), 1),
              abs(to - from))
  return(z)
}
