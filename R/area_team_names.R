#' Format CCG and NHS area team names into print format.
#'
#' Removes brackets and sorts capitalisation.
#' Also replace ampersands with "and" and hyphenates name places such as Stockton-on-Tees.
#'
#' @param area_team_name A vector of area team names
#' @seealso \code{\link{ann_tab_ccg_names}}
#' @examples
#' x <- "NHS ENGLAND NORTH (YORKSHIRE AND HUMBER)"
#' area_team_names(x)
#' area_team_names("nhs Stockton on Tees ccg")
#' area_team_names("Nhs Fylde & Wyre CCG")
#' @export

area_team_names <- function(area_team_name){
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("stringr is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  area_team_name <- stringr::str_trim(area_team_name)
  area_team_name <- stringr::str_to_title(area_team_name)
  area_team_name <- stringr::str_replace_all(area_team_name, "Nhs", "NHS")
  area_team_name <- stringr::str_replace_all(area_team_name, "Ccg", "CCG")
  area_team_name <- stringr::str_replace_all(area_team_name, "And", "and")
  area_team_name <- stringr::str_replace_all(area_team_name, "&", "and")
  area_team_name <- stringr::str_replace_all(area_team_name, "On", "on")
  area_team_name <- stringr::str_replace_all(area_team_name, " on ", "-on-")
  return(area_team_name)
}
