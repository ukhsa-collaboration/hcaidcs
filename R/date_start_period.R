#' Get the string of the first date in a period
#'
#' Takes a period used in mandatory surveillance data and returns first date in the period.
#'
#' @param x A period, either string or numerif
#' @param period_type A string, one of "fq", "cq" or "fyear6"
#'
#' @return A \strong{string} in format dd/mm/yyyy
#'
#' @seealso \code{\link{date_end_period}}
#'
#' @examples
#' date_start_period(20154, "fq")
#' date_start_period(20154, "cq")
#' date_start_period(201516, "fyear6")
#' dat <- data.frame(x = c(20151, 20152, 20153, 20154))
#' date_start_period(dat$x, "fq")
#' date_start_period(dat$x, "cq")
#' @export

date_start_period <- function(x, period_type){
  if(period_type == "fq"){
    yr <- as.numeric(substr(as.character(x), 1, 4))
    q <- as.numeric(substr(as.character(x), 5, 5))
    z <- paste0(date_start_fq(q), ifelse(q == 4, yr+1, yr))
  }else if(period_type == "cq"){
    yr <- substr(as.character(x), 1, 4)
    q <- substr(as.character(x), 5, 5)
    z <- paste0(date_start_cq(q), yr)
  }else if(period_type == "fyear6"){
    yr <- substr(as.character(x), 1, 4)
    z <- paste0("01/04/", yr)
  }
  return(z)
}

date_start_fq <- function(x){
  z <- ifelse(x == 1, "01/04/",
              ifelse(x == 2, "01/07/",
                     ifelse(x == 3, "01/10/",
                            ifelse(x == 4, "01/01/", NA))))
}

date_start_cq <- function(x){
  z <- ifelse(x == 2, "01/04/",
              ifelse(x == 3, "01/07/",
                     ifelse(x == 4, "01/10/",
                            ifelse(x == 1, "01/01/", NA))))
}
