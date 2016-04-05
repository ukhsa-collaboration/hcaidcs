#' Format text to mixed case capitalisation
#'
#' Formats text strings to mixed case text.
#' From https://stat.ethz.ch/R-manual/R-devel/library/base/html/chartr.html
#'
#' @param x A text string
#' @examples
#' x <- "NHS ENGLAND NORTH"
#' simple_cap(x)
#' @export
#'
simple_cap <- function(x) {
  x <- tolower(x)
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
