#' Get the string of the first date in a period
#'
#' Takes a period used in mandatory surveillance data and returns first date in the period.
#'
#' @param x A period, either string or numeric
#' @param period_type A string, one of "fq", "cq" or "fyear6"
#'
#' @seealso \code{\link{date_start_period}}
#' @return A \strong{string} in format dd/mm/yyyy
#'
#' @examples
#' date_end_period(20154, "fq")
#' date_end_period(20154, "cq")
#' date_end_period(201516, "fyear6")
#' dat <- data.frame(x = c(20151, 20152, 20153, 20154))
#' date_end_period(dat$x, "fq")
#' date_end_period(dat$x, "cq")
#' @export

date_end_period <- function(x, period_type){
  if(period_type == "fq"){
    yr <- as.numeric(substr(as.character(x), 1, 4))
    q <- as.numeric(substr(as.character(x), 5, 5))
    z <- paste0(date_end_fq(q), ifelse(q == 4, yr+1, yr))
  }else if(period_type == "cq"){
    yr <- substr(as.character(x), 1, 4)
    q <- substr(as.character(x), 5, 5)
    z <- paste0(date_end_cq(q), yr)
  }else if(period_type == "fyear6"){
    yr <- as.numeric(substr(as.character(x), 1, 4))
    z <- paste0("31/03/", yr + 1)
  }
  return(z)
}

date_end_fq <- function(x){
  z <- ifelse(x == 1, "30/06/",
              ifelse(x == 2, "30/09/",
                     ifelse(x == 3, "31/12/",
                            ifelse(x == 4, "31/03/", NA))))
}

date_end_cq <- function(x){
  z <- ifelse(x == 2, "30/06/",
              ifelse(x == 3, "30/09/",
                     ifelse(x == 4, "31/12/",
                            ifelse(x == 1, "31/03/", NA))))
}
