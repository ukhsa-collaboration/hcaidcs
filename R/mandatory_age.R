#' Calculate patient age in manner that conforms to mandatory surveillance requirements
#'
#' Calculates age as integer based on number of years lived.
#' Takes into account leap years.
#'
#' @param date_of_birth Patient's date of birth in date format
#' @param specimen_date Date of specimen in date format
#' @examples
#' library(lubridate)
#' dob <- dmy("31/01/2000")
#' spec <- dmy("21/01/2001")
#' mandatory_age(dob, spec)
#' # Counter example :
#' round(as.numeric(spec-dob)/365.25, 1)
#' @return Vector of integer ages
#' @export

mandatory_age <- function(date_of_birth, specimen_date){
  #http://stackoverflow.com/questions/3611314/calculating-ages-in-r
  from_lt = as.POSIXlt(date_of_birth)
  to_lt = as.POSIXlt(specimen_date)

  age = to_lt$year - from_lt$year
  # mday is day of month.
  # So, if month_to is less than month_from then age -1
  # & if same month, but day of month in to is less than that in from then age -1
  # i.e. 31 jan 2001 to 21 Jan 2002 is 1 year, but patient hasn't actually passed birthday, so -1
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}
