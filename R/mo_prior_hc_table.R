#' mo_prior_hc_table
#'
#' Create monthly table for prior healthcare exposures
#'
#' @param dat a data frame containing at least following columns:
#' data_collection, org_code, year_no, month_no, total_cases, hoha, coha, coia,
#' coca, month_string
#' @param collection A string giving the data collection, default = "C. difficile"
#' @return A dataframe with the table in wide format
#' @examples
#' library(dplyr)
#' library(tidyr)
#' data(cdi_prior_hc_data)
#' mo_prior_hc_table(cdi_prior_hc_data, collection = "C. difficile")
#' @export

mo_prior_hc_table <- function(dat, collection = "C. difficile"){
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("tidyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("stringr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  assertthat::assert_that(is.data.frame(dat), msg = "dat must be a dataframe")
  collection <- dplyr::enquo(collection)

  out_dat <- dat %>%
    dplyr::filter(data_collection == !!collection) %>%
    dplyr::select(org_code, year_no, month_no, total_cases, hoha, coha, coia,
                  coca, phc_all_blank, phc_unknown_3_mo, month_string) %>%
    dplyr::arrange(year_no, month_no) %>%
    dplyr::mutate(month_year = paste(month_string, year_no, sep = "_")) %>%
    tidyr::gather(key = "measure", value = "counts", -org_code,
                   -year_no,-month_no, -month_string, -month_year)

  out_dat <- out_dat %>%
    dplyr::mutate(month_year = factor(.$month_year,
                                      levels = as.ordered(unique(.$month_year))),
                  measure = factor(.$measure,
                                   levels = c("total_cases", "hoha", "coha",
                                              "coia", "coca",
                                              "phc_unknown_3_mo",
                                              "phc_all_blank"))) %>%
    dplyr::select(-year_no, -month_no, -month_string) %>%
    dplyr::arrange(month_year, measure) %>%
    tidyr::unite(month_year, measure, col = month_year_measure, sep = "_") %>%
    dplyr::mutate(month_year_measure = factor(month_year_measure,
                                              levels = as.ordered(
                                                unique(month_year_measure)))) %>%
    dplyr::filter(stringr::str_detect(month_year_measure, "NA") == FALSE) %>%
    tidyr::spread(key = month_year_measure, value = counts, fill = 0)
    return(out_dat)
}
