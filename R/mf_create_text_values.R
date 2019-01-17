#' function to create text objects for monthly factsheets
#'
#' The factsheet creates object which contain percentage change in the count of cases for use within the bullet points of the text.
#' These bullet points compare the count of cases with the counts of cases at various different time points.
#'
#' @param data A dataframe, in the context of the monthly factsheet, this will almost certainly be 'trend'
#' @param collection  A character string, one of "mrsa", "mssa", "ecoli", "kleb", "paer", "cdi"
#' @param output_reqd A character string, one of "12_month", "3_month", "sum_12"
#'
#' @return A numeric value giving the percentage difference to different comparisons.
#'
#' @examples
#'
#' data("mf_trend_data")
#' mf_trend_data <- mf_lag_trend(mf_trend_data)
#'
#' cdi_12_month <- create_mf_text(data = mf_trend_data, collection = "cdi",
#'   output_reqd = "12_month")

mf_create_text_values <- function(data = trend, collection, output_reqd){

  assertthat::assert_that(collection %in% c("mrsa", "mssa", "ecoli", "kleb",
                                            "paer", "cdi"),
                          msg = "collection must be one of mrsa, mssa, ecoli, kleb,  paer or cdi")
  assertthat::assert_that(output_reqd %in% c("12_month", "3_month", "sum_12"),
                          msg = "output_reqd must be one of 12_month, 3_month, sum_12")

  this_collection <- rlang::enquo(collection)

  data <- data %>% dplyr::arrange(year, month)

  if(output_reqd == "12_month"){
    denom_col <- paste0(collection, "_lag_12")

    numerator <- data %>%
      dplyr::filter(dplyr::row_number() == max(dplyr::row_number())) %>%
      dplyr::select(!!this_collection) %>% dplyr::pull()

    denominator <- data %>%
      dplyr::filter(dplyr::row_number() == max(dplyr::row_number())) %>%
      dplyr::select(contains(denom_col)) %>%
      dplyr::pull()

  }else if(output_reqd == "3_month"){

    num_col <- paste0(collection, "_sum_3")
    denom_col <- paste0(collection, "_sum_3_lag_3")

    numerator <- data %>%
      dplyr::filter(dplyr::row_number() == max(dplyr::row_number())) %>%
      dplyr::select(dplyr::ends_with(num_col)) %>% dplyr::pull()

    denominator <- data %>%
      dplyr::filter(dplyr::row_number() == max(dplyr::row_number())) %>%
      dplyr::select(dplyr::contains(denom_col)) %>%
      dplyr::pull()

  }else if(output_reqd == "sum_12"){

    num_col <- paste0(collection, "_sum_12")
    denom_col <- paste0(collection, "_sum_12_lag_12")

    numerator <- data %>%
      dplyr::filter(dplyr::row_number() == max(dplyr::row_number())) %>%
      dplyr::select(dplyr::ends_with(num_col)) %>%
      dplyr::pull()

    denominator <- data %>%
      dplyr::filter(dplyr::row_number() == max(dplyr::row_number())) %>%
      dplyr::select(dplyr::contains(denom_col)) %>%
      dplyr::pull()
  }

  x <- ((numerator - denominator) / denominator) * 100
  return(x)
}
