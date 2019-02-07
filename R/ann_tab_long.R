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
#' @param collection_string A string giving the data collection
#' @param org_code The variable giving the organisation code
#' @param metric_type A string giving the metric to be tabulated, one of "onset", "pir" or "prior_hc"
#' @param metric_var Column in dat giving column for the metric
#' @param output A string giving the output required. Either an annual table or quarterly
#' @param spec_date Column in dat giving specimen date
#'
#' @return A dataframe with counts of cases in long format, by organisation and time period
#'
#' @export
#'
#' @examples
#' #library(dplyr)
#' data(line_listing)
#' line_listing$fy_long <- fy_long(line_listing$specimen_date)
#'
#' ann_tab_long(dat = line_listing, collection_var = collection,
#'   collection_string = "C. difficile", org_code = reporting_organisation_code,
#'   metric_type = "prior_hc", metric_var = prior_hc, output = "annual",
#'   spec_date = specimen_date)

ann_tab_long <- function(dat, collection_var, collection_string, org_code,
                         metric_type,
                         metric_var, output,
                         # fyear, fquarter = NULL
                         spec_date){

  # assertthat::assert_that(org_type %in% c("trust", "ccg"),
  #                         msg = "org_type must be one of trust or ccg")
  assertthat::assert_that(metric_type %in% c("onset", "pir", "prior_hc"),
                          msg = "metric_type must be one of onset, pir or prior_hc")
  assertthat::assert_that(output %in% c("annual", "quarterly"),
                          msg = "output must be one of quarterly or annual")

  collection_var <- rlang::enquo(collection_var)
  org_code <- rlang::enquo(org_code)
  #fyear <- rlang::enquo(fyear)
  spec_date <- rlang::enquo(spec_date)
  metric_var <- rlang::enquo(metric_var)

  if(metric_type == "prior_hc"){
    dat <- dat %>%
      dplyr::mutate(
        !!metric_var := dplyr::case_when(
          !!metric_var == "hoha, ha" ~ "HOHA, HA",
          !!metric_var == "coha, ha" ~ "COHA, HA",
          !!metric_var == "coia" ~ "COIA",
          !!metric_var == "coca" ~ "COCA",
          !!metric_var %in% c("unknown_3_mo", "unknown 3 months") ~ "Unknown 3 months",
          !!metric_var %in% c("all_blank", "all blank") ~ "No information"
        ))
    }else if(metric_type == "onset"){
          dat <- dat %>%
            dplyr::mutate(
              !!metric_var := dplyr::case_when(
                !!metric_var %in% c("HO", 1) ~ "Hospital-onset",
                !!metric_var %in% c("CO", 0) ~ "Community-onset",
                TRUE ~ NA_character_
              ))
      }

  metric_levels <- switch(metric_type,
                          onset = c("Total cases", "Hospital-onset", "Community-onset"),
                          pir = c("Total cases", "Trust-assigned", "CCG-assigned", "Third-party"),
                          prior_hc = c("Total cases", "HOHA, HA", "COHA, HA", "COIA",
                                              "COCA", "Unknown 3 months",
                                              "No information")
                          )

  if(output == "annual"){

    period_levels <- dat %>%
      dplyr::mutate(period = fy_six(!!spec_date)) %>%
      dplyr::arrange(!!spec_date) %>%
      dplyr::mutate(period = fy_six(!!spec_date)) %>%
      dplyr::group_by(period) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup() %>%
      dplyr::pull(period)


    dat <- dat %>%
      dplyr::mutate(period = factor(fy_six(!!spec_date), levels  = period_levels))

    totals <- dat %>%
      dplyr::filter(stringr::str_trim(!!collection_var) == collection_string) %>%
      dplyr::group_by(!!org_code, period) %>%
      dplyr::summarise(total_cases = dplyr::n()) %>%
      dplyr::mutate(metric = "Total cases") %>%
      dplyr::select(organisation = !!org_code, period, metric,
                    count_cases = total_cases) %>%
      dplyr::ungroup()

    dat <- dat %>%
      dplyr::filter(stringr::str_trim(!!collection_var) == collection_string) %>%
      dplyr::mutate(n_cases = 1) %>%
      dplyr::group_by(!!org_code, period, !!metric_var) %>%
      dplyr::summarise(count_cases = sum(n_cases)) %>%
      dplyr::select(organisation = !!org_code, period, metric = !!metric_var,
                    count_cases) %>%
      dplyr::ungroup()

    out <- dplyr::bind_rows(totals, dat) %>%
      tidyr::complete(organisation, period, metric,
                      fill = list(count_cases = 0)) %>%
      dplyr::mutate(metric = factor(metric, levels = metric_levels)) %>%
      dplyr::arrange(organisation, period, metric)

  } else if(output == "quarterly"){
    period_levels <- dat %>%
      dplyr::mutate(period = fq_long(!!spec_date)) %>%
      dplyr::arrange(!!spec_date) %>%
      dplyr::mutate(period = fq_long(!!spec_date)) %>%
      dplyr::group_by(period) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup() %>%
      dplyr::pull(period)


    dat <- dat %>%
      dplyr::mutate(period = factor(fq_long(!!spec_date),
                                    levels  = period_levels),
                    fyear = fy_six(!!spec_date),
                    fq = fq_short(!!spec_date))

    totals <- dat %>%
      dplyr::filter(stringr::str_trim(!!collection_var) == collection_string) %>%
      dplyr::group_by(!!org_code, period, fyear, fq) %>%
      dplyr::summarise(total_cases = dplyr::n()) %>%
      dplyr::mutate(metric = "Total cases") %>%
      dplyr::select(organisation = !!org_code, financial_year = fyear,
                    financial_quarter = fq, period, metric,
                    count_cases = total_cases) %>%
      dplyr::ungroup()

    dat <- dat %>%
      dplyr::filter(stringr::str_trim(!!collection_var) == collection_string) %>%
      dplyr::mutate(n_cases = 1) %>%
      dplyr::group_by(!!org_code, period, !!metric_var, fyear, fq) %>%
      dplyr::summarise(count_cases = sum(n_cases)) %>%
      dplyr::select(organisation = !!org_code, financial_year = fyear,
                    financial_quarter = fq, period, metric = !!metric_var,
                    count_cases) %>%
      dplyr::ungroup()

    out <- dplyr::bind_rows(totals, dat) %>%
      tidyr::complete(organisation, metric, tidyr::nesting(financial_year,
                      financial_quarter, period),
                      fill = list(count_cases = 0)) %>%
      dplyr::mutate(metric = factor(metric, levels = metric_levels)) %>%
      dplyr::arrange(organisation, period, metric)
  }


  return(out)
}
