#' Formats estimates and 95\% confidence intervals for nice printing.
#'
#' Rounds estimates to 1 decimal place and copies similarly formatted confidence intervals inside brackets.
#' @param estimate An estimate such as a rate ratio
#' @param lci The lower confidence interval
#' @param uci The upper confidence interval
#' @return A string in format d.d (95\% CI: d.d-d.d)
#' @examples
#' nice_estimate(100.111, 90.0, 110.000002)
#' nice_estimate(0.9, 0.8001, 0.95)
#' @export

nice_estimate <- function(estimate, lci, uci){
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("stringr is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  estimate <- stringr::str_trim(sprintf("%7.1f", estimate))
  lci <- stringr::str_trim(sprintf("%7.1f", lci))
  uci <- stringr::str_trim(sprintf("%7.1f", uci))
  z <- paste0(estimate, " (95% CI:", lci, "-", uci, ")" )
  return(z)
}
