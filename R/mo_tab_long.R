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
#'     cmonth = date_to_cmonth(specimen_date))
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
  org_code <- rlang::enquo(org_var)
  #fyear <- rlang::enquo(fyear)
  # spec_date <- rlang::enquo(spec_date)
  metric_var <- rlang::enquo(metric_var)
  year_var <- rlang::enquo(year_var)
  month_var <- rlang::enquo(month_var)

  # if(metric_string == "prior_hc"){
  #   dat <- dat %>%
  #     dplyr::mutate(
  #       !!metric_var := dplyr::case_when(
  #         !!metric_var == "hoha, ha" ~ "HOHA, HA",
  #         !!metric_var == "coha, ha" ~ "COHA, HA",
  #         !!metric_var == "coia" ~ "COIA",
  #         !!metric_var == "coca" ~ "COCA",
  #         !!metric_var %in% c("unknown_3_mo", "unknown 3 months") ~ "Unknown 3 months",
  #         !!metric_var %in% c("all_blank", "all blank") ~ "No information"
  #       ))
  # }else if(metric_string == "onset"){
  #   dat <- dat %>%
  #     dplyr::mutate(
  #       !!metric_var := dplyr::case_when(
  #         !!metric_var %in% c("HO", 1) ~ "Hospital-onset",
  #         !!metric_var %in% c("CO", 0) ~ "Community-onset",
  #         TRUE ~ NA_character_
  #       ))
  # }
  #
  # metric_levels <- switch(metric_string,
  #                         onset = c("Total cases", "Hospital-onset", "Community-onset"),
  #                         pir = c("Total cases", "Trust-assigned", "CCG-assigned", "Third-party"),
  #                         prior_hc = c("Total cases", "HOHA, HA", "COHA, HA", "COIA",
  #                                      "COCA", "Unknown 3 months",
  #                                      "No information")
  # )
  #
  # dat <- dat %>%
  #   mutate(cyear = as.numeric(format(!!spec_date, "%Y")),
  #          cmont = as.numeric(format(!!spec_date, "%m")))

  totals <- dat %>%
    # dplyr::filter(!!collection_var == collection_string) %>%
    # dplyr::group_by(!!org_code, cyear, cmont) %>%
    dplyr::group_by(!!org_code, !!collection_var, !!year_var, !!month_var) %>%
    dplyr::summarise(total_cases = dplyr::n()) %>%
    dplyr::mutate(metric = "Total cases") %>%
    dplyr::select(
      collection = !!collection_var, organisation = !!org_code,
      cyear = !!year_var, cmont = !!month_var, metric,
                  count_cases = total_cases) %>%
    dplyr::ungroup()

  dat <- dat %>%
    # dplyr::filter(!!collection_var == collection_string) %>%
    dplyr::mutate(n_cases = 1) %>%
    dplyr::group_by(!!org_code, !!collection_var, !!year_var, !!month_var, !!metric_var) %>%
    dplyr::summarise(count_cases = sum(n_cases)) %>%
    dplyr::select(collection = !!collection_var, organisation = !!org_code,
                  cyear = !!year_var, cmont = !!month_var, metric = !!metric_var,
                  count_cases) %>%
    dplyr::ungroup()

  out <- dplyr::bind_rows(totals, dat) %>%
    tidyr::complete(organisation, cyear, cmont, metric,
                    fill = list(count_cases = 0)) %>%
    # dplyr::mutate(metric = factor(metric, levels = metric_levels)) %>%
    dplyr::arrange(collection, organisation, cyear, cmont, metric) %>%
    dplyr::select(collection, organisation, cyear, cmont, metric, count_cases)

  return(out)
}
