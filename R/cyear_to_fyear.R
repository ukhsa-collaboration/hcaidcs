#' Convert calendar year to financial year
#'
#' examples
#' cyear_to_fyear(2012)

cyear_to_fyear <- function(x){
  fyear <- paste0(x, substr(x + 1, 3, 4))
  return(fyear)
}
