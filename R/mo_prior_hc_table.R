#' mo_prior_hc_table
#'
#' Create monthly table for prior healthcare exposures
#'
#' @param dat a data frame containing at least following columns:
#' data_collection, org_code, year_no, month_no, total_cases, hoha, coha, coia,
#' coca, month_string
#' @param collection A string giving the data collection
#' @return A dataframe with the table in long format
#' @examples
#' library(dplyr)
#' library(tidyr)
#' data(cdi_prior_hc_data)
#' mo_prior_hc_table(cdi_prior_hc_data)
#' @export

mo_prior_hc_table <- function(dat, collection){
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("tidyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  collection <- dplyr::enquo(collection)

  dat <- dat %>%
    dplyr::filter(data_collection == !!collection) %>%
    dplyr::select(org_code, year_no, month_no, total_cases, hoha, coha, coia,
                  coca, month_string) %>%
    dplyr::arrange(year_no, month_no) %>%
    tidyr:: gather(key = "Measure", value = `Count of cases`, -org_code,
                   -year_no,-month_no, -month_string)
  return(dat)
}
