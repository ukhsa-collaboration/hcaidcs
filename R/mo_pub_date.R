#' Nicely formats date of publication for monthly data
#'
#' The monthly tables have a string formatted date of publication in the header of each table.
#' The publication date is always the first Wednesday of the month in which publication occurs.
#' Data for month *m* are extracted and crunched in month *m* + 1, then published in month *m* + 2.
#' So, this function assumes that the user is working in *m& + 1 and will publish in the subsequent month.
#' This assumption can be over-ridden by supplying an actual date as an argument.
#'
#' @param x A date object
#' @return A string object in format "Day, Month %d, YYYY"
#' @examples
#' mo_pub_date()
#' x <- lubridate::dmy("01/02/2015")
#' mo_pub_date(x)
#' @export

mo_pub_date <- function(x = lubridate::today() ){
  # add 1 month to date, if doing current date. Because publication date is in future.
  now <- ifelse(x == lubridate::today(), as.Date(x, origin = "1970-01-01") %m+% months(1), x)
  # ifelse returns numeric value
  now <- as.Date(now, origin = "1970-01-01")
  now <- lubridate::rollback(now, roll_to_first = TRUE)
  first_week <- seq(now, now + 6, by = "1 day")
  first_wed <- first_week[lubridate::wday(first_week, label = TRUE) == "Wed"]
  day <- as.character(lubridate::wday(first_wed, label = TRUE, abbr = FALSE))
  numeric_day <- sprintf("%02.0f", lubridate::day(first_wed))
  month <- as.character(lubridate::month(first_wed, label = TRUE, abbr = FALSE))
  year <- as.character(lubridate::year(first_wed))
  pub_date_string <- paste0(day, ", ", month, " ",  numeric_day, ", ", year)
  return(pub_date_string)
}
