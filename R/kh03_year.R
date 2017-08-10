#' Convert KH03-formatted dates to Mandatory data-formatted dates.
#'
#' NHS England provide KH03 data periods as separate columns for financial year and quarter.
#' This converts to a format usable for the mandatory surveillance.
#'
#' @param year The year column from KH03 data. String of 7 characters in format "yyyy-yy"
#' @param quarter A string giving the final month of the quarter, one of March, June, September, December
#' @param format A string giving the format desired for the output, one of fyear or cyear
#'
#' @return A numeric variable giving year and quarter
#'
#' @examples
#' kh03_year("2010-11", "March", "fyear")
#' kh03_year("2010-11", "March", "cyear")
#' kh03_year("2010-11", "June", "cyear")
#' kh03_year("2010-11", "June", "fyear")
#' kh03_year("2000-01", "March", "cyear")
#' @export

kh03_year <- function(year, quarter, format){
  first_year <- as.numeric(substr(year, 1, 4))
  second_year <- as.numeric(substr(year, 6, 7))
  second_year <- ifelse(second_year > 80 & second_year <= 99,
                        as.numeric(paste0("19", second_year)),
                        ifelse(second_year == 0, 2000,
                               ifelse(second_year > 0 & second_year <= 80,
                                      as.numeric(paste0("20", sprintf("%02i", second_year))), NA))
  )
  fy_quarter <- ifelse(quarter %in% c("January", "February", "March"), 4,
                       ifelse(quarter %in% c("April", "May", "June"), 1,
                              ifelse(quarter %in% c("July", "August", "September"), 2,
                                                    ifelse(quarter %in% c("October", "November", "December"), 3, NA))))

  cy_quarter <- ifelse(quarter %in% c("January", "February", "March"), 1,
                       ifelse(quarter %in% c("April", "May", "June"), 2,
                              ifelse(quarter %in% c("July", "August", "September"), 3,
                                                    ifelse(quarter %in% c("October", "November", "December"), 4, NA))))

  z <- ifelse(format == "fyear",
              as.numeric(paste0(first_year, fy_quarter)),
              ifelse(format == "cyear",
                     ifelse(cy_quarter == 1,
                            as.numeric(paste0(second_year, cy_quarter)),
                            as.numeric(paste0(first_year, cy_quarter))),
                     stop("Please make sure format is one of cyear or fyear")
                     )
              )
  return(z)
}
