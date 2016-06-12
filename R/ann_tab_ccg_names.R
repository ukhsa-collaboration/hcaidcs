#' Format CCG names for printing in the annual tables.
#'
#' This turns CCG names into title case format and removes strings 'NHS' and 'CCG'.
#' @param A vector of CCG names
#' @examples
#' x <- "NHS DARLINGTON CCG"
#' ann_tab_ccg_names(x)
#' @export

ann_tab_ccg_names <- function(x){
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("stringr is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  x <- stringr::str_to_title(x)
  x <- stringr::str_replace_all(x, "Nhs", "")
  x <- stringr::str_replace_all(x, "Ccg", "")
  x <- stringr::str_replace_all(x, "And", "and")
  x <- stringr::str_trim(x)
  return(x)
}
