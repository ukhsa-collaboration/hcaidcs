#' date_to_fyear
#'
#' Convert a date object to a four-digit financial year
#' @param x A date object
#' @return A numeric value giving the four-digit financial year
#' @examples
#' x <- as.Date("01/01/2017", format = "%d/%m/%Y")
#' date_to_fyear(x)
#' @export

date_to_fyear <- function(x){
  z <- ifelse(as.numeric((format(x, "%m"))) < 4,
              as.numeric((format(x, "%Y"))) - 1,
              as.numeric((format(x, "%Y")))
  )
  return(z)
}
