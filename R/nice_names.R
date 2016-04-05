#' Format variable names into standard format.
#'
#' Turns column names of data frames into standardised format.
#' Dots are replaced with underscores, trailing underscores are removed and everything is made lower case.
#'
#' @param x A data frame
#' @examples
#' data(mtcars)
#' names(mtcars) <- toupper(names(mtcars))
#' names(mtcars) <- nice_names(mtcars)
#' @export

nice_names <- function(x){
  names(x) <- gsub("\\.", "\\_", tolower(names(x)))
  names(x) <- gsub("\\_{2,}", "\\_", names(x)) # where multiple underscores occur, are replaced by one.
  names(x) <- gsub("\\_$", "", names(x)) # remove trailing underscores.
}
