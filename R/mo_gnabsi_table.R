#' Monthly table for Gram-negative blood stream infection data
#'
#' Produce the monthly wide-format table for GNBSI .
#' Takes long format data for either trusts or CCGs and turns it wide with 13 months as variables.
#' The function will filter the data to a given collection, i.e. "K. pneumoniae", "E. coli" or "Pseudomonas" as exported in the HCAI DCS line lists.
#'
#' @param data_fm A data frame to work on, usually either trust_dat_all or ccg_dat_all
#' @param collection A text string giving the data collection
#' @param org_type A string giving the organisation type, one of trust or ccg. Lower case.
#' @return A wide data.frame with counts of cases by org and month.
#' @examples
#' library(dplyr)
#' data(monthly_ccg_data_raw)
#' head(monthly_ccg_data_raw)
#' ccg_dat_all <- monthly_ccg_data_raw %>%
#'     rename(org_code = ccg_code, ho = apportioned) %>%
#'     mutate(data_collection = "E. coli", co = total_cases - ho)
#'
#' my_out_dat <- mo_gnabsi_table(data_fm = ccg_dat_all, collection = "E. coli",
#'     org_type = "ccg")
#'
#' head(my_out_dat)
#' @export

mo_gnabsi_table <- function(data_fm, collection, org_type){
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("dplyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("dplyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(!(org_type %in% c("trust", "ccg"))){
    stop("org_type must be one of trust or ccg.", call. = FALSE)
  }

  collection <- dplyr::enquo(collection)

  # select variables of interest and concatenate year and month
  if(org_type == "trust"){
    out_dat <- data_fm %>%
      dplyr::filter(data_collection == !!collection) %>%
      dplyr::select(org_code, total_cases, ho, co, year_no, month_no,
                    month_string) %>%
      dplyr::mutate(month_year = paste(month_string, year_no, sep = "_"))

    out_dat <- out_dat %>%
      # turn year and month into a factor for sorting
      dplyr::mutate(month_year = factor(.$month_year,
                                        levels = as.ordered(unique(.$month_year)))) %>%
      # drop now unnecessary vars
      dplyr::select(-year_no, -month_no, -month_string) %>%
      # get long
      tidyr::gather(key = measure, value = count, -org_code, -month_year) %>%
      # Create factor for sorting
      dplyr::mutate(measure = factor(measure,
                                     levels = c("total_cases", "ho", "co"))) %>%
      #sort
      dplyr::arrange(month_year, measure) %>%
      # concatenate month_year with the outcome (all cases, hospital onset, community onset)
      tidyr::unite(month_year, measure, col = month_year_measure, sep = "_") %>%
      # create factor for sorting
      dplyr::mutate(month_year_measure = factor(month_year_measure,
                                                levels = as.ordered(unique(month_year_measure)))) %>%
      # drop unwanted periods
      dplyr::filter(stringr::str_detect(month_year_measure, "NA") == FALSE) %>%
      # get wide
      tidyr::spread(key = month_year_measure, value = count, fill = 0)
  }else if(org_type == "ccg"){
    out_dat <- data_fm %>%
      dplyr::filter(data_collection == !!collection) %>%
      dplyr::select(org_code, total_cases, ho, co, year_no, month_no,
                    month_string) %>%
      dplyr::mutate(month_year = paste(month_string, year_no, sep = "_"))

    out_dat <- out_dat %>%
      # turn year and month into a factor for sorting
      dplyr::mutate(month_year = factor(.$month_year,
                                        levels = as.ordered(unique(.$month_year)))) %>%
      # drop now unnecessary vars
      dplyr::select(-year_no, -month_no, -month_string) %>%
      # get long
      tidyr::gather(key = measure, value = count, -org_code, -month_year) %>%
      # Create factor for sorting
      dplyr::mutate(measure = factor(measure,
                                     levels = c("total_cases", "co", "ho"))) %>%
      #sort
      dplyr::arrange(month_year, measure) %>%
      # concatenate month_year with the outcome (all cases, hospital onset, community onset)
      tidyr::unite(month_year, measure, col = month_year_measure, sep = "_") %>%
      # create factor for sorting
      dplyr::mutate(month_year_measure = factor(month_year_measure,
                                                levels = as.ordered(unique(month_year_measure)))) %>%
      # drop unwanted periods
      dplyr::filter(stringr::str_detect(month_year_measure, "NA") == FALSE) %>%
      # get wide
      tidyr::spread(key = month_year_measure, value = count, fill = 0)
  }

  return(out_dat)
}
