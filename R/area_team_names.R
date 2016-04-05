#' Format CCG and NHS area team names into print format.
#'
#' Removes brackets and sorts capitalisation.
#'
#' @param area_team_name A vector of area team names
#' @examples
#' x <- "NHS ENGLAND NORTH (YORKSHIRE AND HUMBER)"
#' area_team_names(x)
#' @export

area_team_names <- function(area_team_name){
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("stringr is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  area_team_name <- stringr::str_trim(stringr::str_replace(area_team_name, "\\(.*?\\)", "")) # remove bracketed text and trim white space
  area_team_name <- simple_cap(area_team_name)
  area_team_name <- stringr::str_replace_all(area_team_name, "Nhs", "NHS")
  area_team_name <- stringr::str_replace_all(area_team_name, "Ccg", "CCG")
  return(area_team_name)
}
