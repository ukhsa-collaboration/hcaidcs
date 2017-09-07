#' Monthly CCG table
#'
#' Produce the monthly wide-format table for CCGs.
#' Takes long format data for CCGs and turns it wide with 13 months as variables.
#' Hard coded to operate on dataframe 'ccg_dat_all', it filters the data to a given collection, e.g. "MRSA" or "E. coli" as exported in the HCAI DCS line lists.
#'
#' @param collection A text string giving the data collection
#' @examples
#' data(monthly_ccg_data_raw)
#' ccg_dat_all <- monthly_ccg_data_raw
#' out_dat <- mo_ccg_table(collection = "MRSA")
#' head(out_dat)
#' @export

mo_ccg_table <- function(collection){
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr needed for this function to work. Please install it.",
         call. = FALSE)
  }

  collection <- dplyr::enquo(collection)

  out_dat <- ccg_dat_all %>%
    dplyr::filter(data_collection == !!collection) %>%
    dplyr::select(-apportioned, -nhs_trust_pir, -ccg_pir, -third_party_pir) %>%
    dplyr::arrange(year_no, month_no) %>%
    dplyr::mutate(month_year = paste(month_string, year_no, sep = "_"))

  out_dat <- out_dat %>%
    dplyr::mutate(month_year = factor(.$month_year,
                               levels = as.ordered(unique(.$month_year)))) %>%
    dplyr::select(-data_collection, -fyear6, -year_no, -month_no, -fmonth,
           -month_string) %>%
    tidyr::spread(key = month_year, value = total_cases, fill = 0)
  return(out_dat)
}
