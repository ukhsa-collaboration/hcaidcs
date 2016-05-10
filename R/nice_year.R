#' Nicely print the most recent year
#'
#' @param x A year variable
#' @param year_format The format of x, one of "fyear6", "fyear4", "cyear2"
#' @return A string formatted year
#' @examples
#' x <- 201516
#' nice_year(x, "fyear6")
#' nice_year(97, "cyear2")
#' nice_year(12, "cyear2")
#' nice_year(0708, "fyear4")
#' nice_year("07/08", "fyear4")
#' @export

nice_year <- function(x, year_format){
  x <- gsub("[:alpha:] | [:punct:] | [:space:]" , "", x)
  x <-gsub("/", "", x)
  year_format_list <- c("fyear6", "fyear4", "cyear2")
  stopifnot(year_format %in% year_format_list)
  z <- ifelse(year_format == "fyear6",
              paste0(substr(x, 1,4), "/", substr(x, 5, 6)),
              ifelse(year_format == "fyear4",
                     ifelse(substr(x, 1, 2) < 90,
                            paste0(20, substr(x, 1,2), "/", substr(x, 3, 4)),
                            paste0(19, substr(x, 1,2), "/", substr(x, 3, 4))),
                     ifelse(year_format == "cyear2",
                            ifelse(x > 90, paste0("19", x), paste0("20", x)),
                            NA)
                     )
              )
  return(z)
}
