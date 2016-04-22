#' Clear the console
#'
#' Clears the console so that you can start the next step of coding with a fresh screen.
#' Has no arguments.
#'
#' @examples
#' cat(rep("This is a line. \n", 50)) # print some text to clear
#' clear_console()
#' @export

clear_console <- function(){
  cat("\014")
}
