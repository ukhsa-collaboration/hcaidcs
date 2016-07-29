#' Add ordered month and calendar year to a data frame
#'
#' Function to add vector of ordered month-year factor to a dataframe.
#' To be used for preparation of monthly outputs.
#'
#' @param data A data frame
#' @param specimen_date A specimen date, in date format, as part of the data frame
#' @seealso \code{\link{ordered_fin_qtr}}
#' @return A dataframe with an additional column giving and ordered month in format "January 2015", ...
#' @examples
#' dat <- data.frame(spec_date = c(dmy("01/05/2015", "01/06/2015",
#'                                     "01/07/2015", "01/08/2015", "01/09/2015",
#'                                     "01/10/2015", "01/11/2015", "01/12/2015",
#'                                     "01/01/2016", "01/02/2016", "01/03/2016",
#'                                     "01/04/2016", "01/05/2016", "01/04/2015")),
#'                   stringsAsFactors = FALSE)
#' dat <- ordered_m_y(dat, "spec_date")
#' dat
#' @importFrom magrittr "%>%"
#' @export

ordered_m_y <- function(data, specimen_date){
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("lubridate is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  z <- data %>%
    dplyr::arrange_(lazyeval::interp(~var, var = as.name(specimen_date))) %>%
    dplyr::mutate_(
      temp_year = lazyeval::interp(~lubridate::year(var), var = as.name(specimen_date)),
      temp_month = lazyeval::interp(~lubridate::month(var, label = TRUE, abbr = FALSE), var = as.name(specimen_date))) %>%
    dplyr::mutate(
            month_yr = paste(as.character(temp_month), temp_year),
            month_yr = factor(month_yr, levels = as.ordered(month_yr))
            ) %>%
    dplyr::select(-temp_year, -temp_month)
}

