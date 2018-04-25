#' Monthly CCG table
#'
#' Produce the monthly wide-format table for CCGs.
#' Takes long format data for CCGs and turns it wide with 13 months as variables.
#' Hard coded to operate on dataframe 'ccg_dat_all', it filters the data to a given collection, e.g. "MRSA" or "E. coli" as exported in the HCAI DCS line lists.
#'
#' @param data_fm The data frame on which to work, either ccg_dat_all or trust_dat_all
#' @param collection A text string giving the data collection
#' @param column A column which will be transposed
#' @return A wide data.frame with counts of cases per month in columns with CCGs as rows
#' @examples
#' library(dplyr)
#' data(monthly_ccg_data_raw)
#' ccg_dat_all <- monthly_ccg_data_raw %>%
#'     rename(org_code = ccg_code)
#' out_dat <- mo_std_table(data_fm = ccg_dat_all, collection = "MRSA",
#'     column = "total_cases")
#' head(out_dat)
#' @export

mo_std_table <- function(data_fm, collection, column){
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("tidyr needed for this function to work. Please install it.",
         call. = FALSE)
  }

  collection <- dplyr::enquo(collection)
  column <- dplyr::enquo(column)

  out_dat <- data_fm %>%
    dplyr::filter(data_collection == !!collection) %>%
    dplyr::select(org_code, year_no, month_no, !!column, month_string) %>%
    # dplyr::select(-apportioned, -nhs_trust_pir, -ccg_pir, -third_party_pir) %>%
    dplyr::arrange(year_no, month_no) %>%
    dplyr::mutate(month_year = paste(month_string, year_no, sep = "_"))

  out_dat <- out_dat %>%
    dplyr::mutate(month_year = factor(.$month_year,
                               levels = as.ordered(unique(.$month_year)))) %>%
    dplyr::select(-year_no, -month_no, -month_string) %>%
    # there was a problem here, hoping that it is to do with out-of-date tidyr (0.6)
    # will update
    tidyr::spread(key = month_year, value = !!column, fill = 0)
  return(out_dat)
}
