#' Create long annual tables from line listing data
#'
#' From FY 2018/19, annual tables will be moving to a long format rather than a
#' wide format. This function takes line-listing data and aggregates by
#' organisation and time period, producing counts of cases by a given metric
#' (apportioning or prior healthcare exposure).
#'
#' The data are expanded to cover time periods in the data, but there may well
#' be organisations that are not in the line-listing data that will be merged in
#' later to make a complete table.
#'
#' In addition, PHEC and organisation names will need to be merged in, as will
#' denominator data, and rates will need to be calculated.
#'
#' @param dat A dataframe containing line-listing data
#' @param collection_var A string giving the data collection
#' @param org_var The variable giving the organisation code
#' @param metric_var Column in dat giving column for the metric
#' @param period_var Column giving the period by which counts should be aggregated
#'
#' @return A dataframe with counts of cases in long format, by organisation and time period
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' data(line_listing)
#' line_listing$fy_long <- fy_long(line_listing$specimen_date)
#'
#' line_listing %>%
#'   filter(collection == "C. difficile") %>%
#'   ann_tab_long(dat = ., collection_var = collection,
#'     org_var = reporting_organisation_code,
#'     metric_var = prior_hc, period_var = fy_long)

ann_tab_long <- function(dat, collection_var, org_var,
                         metric_var, period_var){

  # assertthat::assert_that(org_type %in% c("trust", "ccg"),
  #                         msg = "org_type must be one of trust or ccg")
  # assertthat::assert_that(metric_type %in% c("onset", "pir", "prior_hc"),
  #                         msg = "metric_type must be one of onset, pir or prior_hc")
  # assertthat::assert_that(output %in% c("annual", "quarterly"),
  #                         msg = "output must be one of quarterly or annual")

  collection_var <- rlang::enquo(collection_var)
  org_code <- rlang::enquo(org_var)
  #fyear <- rlang::enquo(fyear)
  # spec_date <- rlang::enquo(spec_date)
  metric_var <- rlang::enquo(metric_var)
  period_var <- rlang::enquo(period_var)


  totals <- dat %>%
      dplyr::group_by(!!org_code, !!period_var, !!collection_var) %>%
      dplyr::summarise(total_cases = dplyr::n()) %>%
    dplyr::ungroup() %>%
      dplyr::mutate(metric = "Total cases") %>%
      dplyr::select(collection = !!collection_var, organisation = !!org_code,
                    period = !!period_var, metric, count_cases = total_cases)


    dat <- dat %>%
      dplyr::mutate(n_cases = 1) %>%
      dplyr::group_by(!!org_code, !!period_var, !!metric_var, !!collection_var) %>%
      dplyr::summarise(count_cases = sum(n_cases)) %>%
      dplyr::ungroup() %>%
      dplyr::select(collection = !!collection_var, organisation = !!org_code,
                    period = !!period_var, metric = !!metric_var, count_cases)

    out <- dplyr::bind_rows(totals, dat) %>%
      tidyr::complete(organisation, period, metric,
                      fill = list(count_cases = 0)) %>%
      dplyr::arrange(organisation, period, metric)

  return(out)
}
