#' function to create text objects for monthly factsheets
#'
#' @examples
#'
#' data("mf_trend_data")
#' mf_trend_data <- mf_trend_data %>%
#'   mutate_at(
#'   vars(cdi, ecoli, mrsa, mssa),
#'     funs(lag_12 = lag(., 12))) %>%
#'   # compares June 2016 to June 2015
#'   mutate_at(vars(cdi, ecoli, mrsa, mssa),
#'             funs(sum_3 = RcppRoll::roll_sum(., 3, align = "right", fill = "NA"))
#'   ) %>%  # sum last three months
#'   mutate_at(
#'     vars(cdi_sum_3, ecoli_sum_3, mrsa_sum_3, mssa_sum_3),
#'     funs(lag_3 = dplyr::lag(., 12))
#'   ) %>% # compares summed three months with summed three months, 12 months earlier
#'   mutate_at(
#'     vars(cdi, ecoli, mrsa, mssa),
#'     funs(sum_12 = RcppRoll::roll_sum(., 12, align = "right", fill = "NA"))
#'   ) %>% # rolling 12 month sum
#'   mutate_at(
#'     vars(cdi_sum_12, ecoli_sum_12, mrsa_sum_12, mssa_sum_12),
#'     funs(lag_12 = dplyr::lag(., 12))
#'   )
#'
#' cdi_12_month <- create_mf_text(data = mf_trend_data, collection = "cdi",
#'   output_reqd = "12_month")

mf_create_text_values <- function(data = trend, collection, output_reqd){

  assertthat::assert_that(collection %in% c("mrsa", "mssa", "ecoli", "kleb",
                                            "paer", "cdi"),
                          msg = "collection must be one of mrsa, mssa, ecoli, kleb,  paer or cdi")
  assertthat::assert_that(output_reqd %in% c("12_month", "3_month", "12_avg"),
                          msg = "output_reqd must be one of 12_month, 3_month, 12_avg")

  this_collection <- enquo(collection)

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
      select(dplyr::contains(num_col)) %>% pull()

    denominator <- data %>%
      dplyr::filter(dplyr::row_number() == max(dplyr::row_number())) %>%
      dplyr::select(dplyr::contains(denom_col)) %>%
      dplyr::pull()

  }else if(output_reqd == "_sum_12"){

    num_col <- paste0(collection, "_sum_12")
    denom_col <- paste0(collection, "_sum_12_lag_12")

    numerator <- data %>%
      dplyr::filter(dplyr::row_number() == max(dplyr::row_number())) %>%
      dplyr::select(dplyr::contains(num_col)) %>%
      dplyr::pull()

    denominator <- data %>%
      dplyr::filter(dplyr::row_number() == max(dplyr::row_number())) %>%
      dplyr::select(dplyr::contains(denom_col)) %>%
      dplyr::pull()
  }

  x <- ((numerator - denominator) / denominator) * 100
  return(x)
}
