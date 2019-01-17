#' lag mf trend data
#'
#' Creates twelve month lag, three month sum, lagged by twelve months, and
#' twelve month sum, lagged by 12 months.
#'
#' @param x A data frame with columns for "cdi", "ecoli", "mrsa", "mssa",
#'   "kleb", "paer" and "ecoli_ta".
#' @return The same dataframe, but with new lagged variables added
#'
#' @export
#' @examples
#' data(mf_trend_data)
#' mf_trend_data <- mf_trend_data %>% mf_lag_trend()
#' head(mf_trend_data)

mf_lag_trend <- function(x = trend){
  x <- x %>%
    dplyr::mutate_at(
      dplyr::vars(cdi, ecoli, mrsa, mssa, kleb, paer ,ecoli_ta),
      dplyr::funs(lag_12 = lag(., 12))
    ) %>%  # compares June 2016 to June 2015
    dplyr::mutate_at(
      dplyr::vars(cdi, ecoli, mrsa, mssa, kleb, paer, ecoli_ta),
      dplyr::funs(sum_3 = RcppRoll::roll_sum(., 3, align = "right", fill = "NA"))
    ) %>%  # sum last three months
    dplyr::mutate_at(
      dplyr::vars(cdi_sum_3, ecoli_sum_3, mrsa_sum_3, mssa_sum_3, kleb_sum_3,
                  paer_sum_3, ecoli_ta_sum_3),
      dplyr::funs(lag_3 = lag(., 12))
    ) %>% # compares summed three months with summed three months, 12 months earlier
    dplyr::mutate_at(
      dplyr::vars(cdi, ecoli, mrsa, mssa, kleb, paer, ecoli_ta),
      dplyr::funs(sum_12 = RcppRoll::roll_sum(., 12, align = "right", fill = "NA"))
    ) %>% # rolling 12 month sum
    dplyr::mutate_at(
      dplyr::vars(cdi_sum_12, ecoli_sum_12, mrsa_sum_12, mssa_sum_12, kleb_sum_12,
           paer_sum_12, ecoli_ta_sum_12),
      dplyr::funs(lag_12 = lag(., 12))
    )
}
