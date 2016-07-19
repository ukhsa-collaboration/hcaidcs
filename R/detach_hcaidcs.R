#' Convenience function to detach hcaidcs package
#'
#' I often need to detach this package to reload an updated version, this should do so nice and quickly.
#' I don't see other uses for it.
#' Inspiration for this function: \href{http://stackoverflow.com/questions/6979917/how-to-unload-a-package-without-restarting-r}{kohske on StackOverflow.com}
#' @export

detach_hcaidcs <- function(){
  detach("package:hcaidcs", unload=TRUE)
}
