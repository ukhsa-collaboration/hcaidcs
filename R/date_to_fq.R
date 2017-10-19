#' date_to_fq
#'
#' Convert a date to a five-character financial quarter
#' @param x A date
#' @return A numeric value giving YYYYQ
#'
#' @examples
#' x <- as.Date("01/01/2017", format = "%d/%m/%Y")
#' date_to_fq(x)
#' @export

date_to_fq <- function(x){
  q <- ifelse(as.numeric(format(x, "%m")) <= 3, 4,
              ifelse(as.numeric(format(x, "%m")) >3 & as.numeric(format(x, "%m")) <= 6, 1,
                     ifelse(as.numeric(format(x, "%m")) >6 & as.numeric(format(x, "%m")) <= 9, 2,
                            ifelse(as.numeric(format(x, "%m")) >9 & as.numeric(format(x, "%m")) <= 12, 3, NA
              ))))
  year <- ifelse(q == 4,
                 as.numeric(format(x, "%Y")) - 1,
                 format(x, "%Y")
                 )
  z <- as.numeric(paste0(year, q))
  return(z)
}
