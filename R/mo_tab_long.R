#' mo_tab_long
#'
#' Takes line list data and aggregates to long data for saving as long-format monthly tables.
#' Outputs are also sorted by organisation, time and metric.
#'
#' It is expected that the metric variable is an ordered factor prior to the creation of the long tables.
#'
#' @param dat A dataframe containing line-listing data
#' @param collection_var A string giving the data collection
#' @param metric_var Column in dat giving column for the metric
#' @param org_var The variable giving the organisation code
#' @param year_var The variable giving the calendar year
#' @param month_var The variable giving the calendar month
#'
#' @seealso \code{\link{ann_tab_long}}, \code{\link{factor_apportioned}}, \code{\link{factor_prior_hc}}
#'
#' @examples
#' library(dplyr)
#' data(line_listing)
#' line_listing <- line_listing %>%
#'   mutate(cyear = format(specimen_date, "%Y"),
#'     cmonth = date_to_cmonth(specimen_date),
#'     prior_hc = factor(prior_hc))
#'
#' mo_tab_long(dat = line_listing, collection_var = collection,
#'   org_var = reporting_organisation_code,
#'   metric_var = prior_hc,
#'   year_var = cyear, month_var = cmonth
#'   )
#' @export

mo_tab_long <- function(dat, collection_var, metric_var, org_var, year_var, month_var){
  # assertthat::assert_that(metric_string %in% c("onset", "pir", "prior_hc"),
  #                         msg = "metric_string must be one of onset, pir or prior_hc")

  collection_var <- rlang::enquo(collection_var)
  org_code       <- rlang::enquo(org_var)
  metric_var     <- rlang::enquo(metric_var)
  year_var       <- rlang::enquo(year_var)
  month_var      <- rlang::enquo(month_var)

  metric_levels <- dat %>% filter(!!metric_var != "") %>%
    mutate(!!metric_var := droplevels(!!metric_var)) %>%  pull(!!metric_var) %>%
    levels()
  metric_levels <- c(metric_levels, "Total cases")
  # metric_levels <- forcats::fct_expand(metric_levels, "Total cases")

  totals <- dat %>%
    dplyr::group_by(!!org_code, !!collection_var, !!year_var, !!month_var) %>%
    dplyr::summarise(total_cases = dplyr::n()) %>%
    dplyr::mutate(metric = factor("Total cases", levels = metric_levels)) %>%
    dplyr::select(
      collection = !!collection_var, organisation = !!org_code,
      cyear = !!year_var, cmont = !!month_var, metric,
                  count_cases = total_cases) %>%
    dplyr::ungroup()

  dat <- dat %>%
    dplyr::mutate(n_cases = 1) %>%
    dplyr::group_by(!!org_code, !!collection_var, !!year_var, !!month_var, !!metric_var) %>%
    dplyr::summarise(count_cases = sum(n_cases)) %>%
    dplyr::select(collection = !!collection_var, organisation = !!org_code,
                  cyear = !!year_var, cmont = !!month_var, metric = !!metric_var,
                  count_cases) %>%
    dplyr::ungroup()

  out <- dplyr::bind_rows(totals, dat) %>%
    tidyr::complete(collection, organisation, cyear, cmont, metric,
                    fill = list(count_cases = 0)) %>%
    dplyr::arrange(collection, organisation, cyear, cmont, metric) %>%
    dplyr::select(collection, organisation, cyear, cmont, metric, count_cases)

  return(out)
}
