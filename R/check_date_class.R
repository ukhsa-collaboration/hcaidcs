#' Check that date variables are in required format
#'
#' Checks that dates are in class date or POSIX. Called by other functions.
#'
#' @param date_var A date variable to be checked.

check_date_class <- function(date_var){
  date_formats <- c("POSIXct", "POSIXt", "Date")
  if(is.na(date_var) == FALSE){if(class(date_var)[1] %in% date_formats != TRUE){stop("date_var is not in date format")}}
}
