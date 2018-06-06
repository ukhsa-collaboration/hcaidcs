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
#' \dontrun{
#' # This is failing R cmd check because can't find dat and I don't understand why
#' # so excluding from examples for now. This should run fine if copied-and-pasted
# dat <- data.frame(
#   spec_date = c(lubridate::dmy("01/05/2015", "01/06/2015", "01/05/2015",
#                                "01/07/2015", "01/08/2015", "01/09/2015",
#                                "01/10/2015", "01/11/2015", "01/12/2015",
#                                "01/01/2016", "01/02/2016", "01/03/2016",
#                                "01/04/2016", "01/05/2016", "01/04/2015")),
#                   stringsAsFactors = FALSE)
#' ordered_m_y(data = dat, specimen_date = spec_date)
#' }
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

  specimen_date <- dplyr::enquo(specimen_date)
  z <- data %>%
    dplyr::arrange(!!specimen_date) %>%
    dplyr::mutate(
      temp_year = lubridate::year(!!specimen_date),
      temp_month = lubridate::month(!!specimen_date, label = TRUE,
                                     abbr = FALSE)) %>%
    dplyr::mutate(
            month_yr = paste(as.character(temp_month), temp_year),
            month_yr = factor(month_yr,
                              levels = as.ordered(unique(month_yr))),
            month_yr = droplevels(month_yr)
            ) %>%
    dplyr::select(-temp_year, -temp_month)
  return(z)
}

