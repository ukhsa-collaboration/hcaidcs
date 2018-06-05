#' ann_tab_pir_quarterly
#'
#' Prepare quarterly MRSA PIR tables
#'
#' @examples
#'
#' pir_qtr_testdat <- data.frame(stringsAsFactors=FALSE,
#'     time_period = c("April - June 2013", "July - September 2013",
#'                     "October - December 2013",
#'                     "January - March 2014",
#'                     "April - June 2013", "July - September 2013",
#'                     "October - December 2013",
#'                     "January - March 2014"),
#'     org_code = c("W1A", "W1A", "W1A", "W1A", "E17", "E17", "E17",
#'                  "E17"),
#'     denominator = c(100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L),
#'     total = c(10L, 20L, 30L, 40L, 50L, 0L, 40L, 30L),
#'     ho = c(8L, 16L, 22L, 31L, 40L, 0L, 35L, 15L),
#'     co = c(2L, 4L, 8L, 9L, 10L, 0L, 5L, 15L),
#'     pir_trust_assigned = c(4L, 10L, 5L, 30L, 10L, 0L, 5L, 5L),
#'     pir_ccg_assigned = c(3L, 5L, 20L, 5L, 40L, 0L, 30L, 20L),
#'     third_party = c(3L, 5L, 5L, 5L, 0L, 0L, 5L, 5L)
#'     )


ann_tab_pir_quarterly <- function(data, timeperiod, org_code, denominator,
                                  total, pir_trust_assigned, pir_ccg_assigned,
                                  third_party, org_type){

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("tidyr needed for this function to work. Please install it.",
         call. = FALSE)
  }

  assertthat::assert_that(is.data.frame(dat), msg = "dat must be a dataframe")
  collection <- dplyr::enquo(collection)

  # trust tables go total, trust, ccg, third part
  # ccg tables go total, ccg, trust, third party
    if(tolower(org_type) == "trust"){
    pir_levels <- c("Total reported cases*", "Trust assigned cases**",
                    "CCG assigned cases***", "Third party cases****")
  }else if(tolower(org_type == "ccg")){
    pir_levels <- c("Total reported cases*",  "CCG assigned cases**",
                     "Trust assigned cases***", "Third party cases****")
  }

  z <- data %>%
    mutate(rate = (total/denominator) * 100000)

}

#' ann_tab_onset_annual
#'
#' Produce annual tables by onset status
#'
#' @param dat A dataframe in long format
#' @param timeperiod A financial year as YYYYYY or YYYY/YY. Can be either numeric or character
#' @param org_code Character vector giving organisation code
#' @param denominator The denominator value
#' @param total Total count of cases
#' @param ho Count of hospital-onset cases
#' @param co Count of community-onset cases
#' @param org_type The only parameter which should not be a column in the data frame a character giving either "Trust" or "CCG"
#'
#' @return A wide dataframe with total counts, total rates, onset count, onset rate, other onset count by financial year
#'
#' @examples
#' ann_onset_testdat <- data.frame(stringsAsFactors=FALSE,
#'               time_period = c(201415L, 201516L, 201617L, 201718L, 201415L, 201516L,
#'                               201617L, 201718L),
#'                  org_code = c("W1A", "W1A", "W1A", "W1A", "E17", "E17", "E17",
#'                               "E17"),
#'               denominator = c(100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L),
#'                     this_is_the_total = c(10L, 20L, 30L, 40L, 50L, 0L, 40L, 30L),
#'                        ho = c(8L, 16L, 22L, 31L, 40L, 0L, 35L, 15L),
#'                        co = c(2L, 4L, 8L, 9L, 10L, 0L, 5L, 15L),
#'        pir_trust_assigned = c(4L, 10L, 5L, 30L, 10L, 0L, 5L, 5L),
#'          pir_ccg_assigned = c(3L, 5L, 20L, 5L, 40L, 0L, 30L, 20L),
#'               third_party = c(3L, 5L, 5L, 5L, 0L, 0L, 5L, 5L)
#'     )
#'  ann_tab_onset_annual(ann_onset_testdat, timeperiod = time_period,
#'     org_code = org_code, denominator = denominator, total= this_is_the_total, ho = ho,
#'     co = co, org_type = "trust")
#' @export

ann_tab_onset_annual <- function(dat, timeperiod, org_code, denominator,
                                  total, ho, co, org_type){

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("tidyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("rlang needed for this function to work. Please install it.",
         call. = FALSE)
  }

  assertthat::assert_that(is.data.frame(dat), msg = "dat must be a dataframe")
  timeperiod <- dplyr::enquo(timeperiod)
  org_code <- dplyr::enquo(org_code)
  denominator <- dplyr::enquo(denominator)
  total <- dplyr::enquo(total)
  ho <- dplyr::enquo(ho)
  co <- dplyr::enquo(co)
  org_type <- tolower(org_type)

  if(tolower(org_type) == "trust"){
    onset_levels <- c("Total cases*", "Total rate", "HO cases**", "HO rate",
                    "CO cases***")
  }else if(tolower(org_type == "ccg")){
    onset_levels <- c("Total cases*", "Total rate","CO cases**", "CO rate",
                      "HO cases***")
  }

  z <- dat %>%
    dplyr::select(!!timeperiod, !!org_code, !!denominator, !!total, !!ho,
                  !!co) %>%
    dplyr::mutate(table_type = org_type,
      total_rate = (!!total/!!denominator) * 100000,
      onset_rate = ifelse(table_type == "trust",
                          (!!ho / !!denominator) * 100000,
                          (!!co / !!denominator) * 100000
                          ),
      timeperiod2 = fy_six_to_long(!!timeperiod),
      onset_rate_type = ifelse(table_type == "trust", "HO", "CO")
      ) %>%
    dplyr::select(-!!denominator, -table_type) %>%
    tidyr::gather(key = "measure", value = "value", -!!timeperiod, -timeperiod2,
                  -!!org_code, -onset_rate_type) %>%
    dplyr::mutate(measure = dplyr::case_when(
      measure == gsub("~", "", rlang::expr_text(total)) ~ "Total cases*",
      measure == gsub("~", "", rlang::expr_text(ho)) & onset_rate_type == "HO"  ~ "HO cases**",
      measure == gsub("~", "", rlang::expr_text(ho)) & onset_rate_type == "CO" ~ "HO cases***",
      measure == gsub("~", "", rlang::expr_text(co)) & onset_rate_type == "CO"  ~ "CO cases**",
      measure == gsub("~", "", rlang::expr_text(co)) & onset_rate_type == "HO"  ~ "CO cases***",
      measure == "total_rate" ~ "Total rate",
      measure == "onset_rate" ~ paste0(onset_rate_type, " rate"),
      TRUE ~ measure
    )) %>%
    dplyr::select(-onset_rate_type)


    z <- z %>%
      dplyr::mutate(measure = factor(measure, levels = onset_levels)) %>%
      dplyr::arrange(time_period, measure) %>%
      tidyr::unite(col = time_measure, timeperiod2, measure) %>%
      dplyr::mutate(time_measure =
                      factor(.$time_measure,
                             levels = as.ordered(unique(.$time_measure)))) %>%
      dplyr::select(-time_period) %>%
      tidyr::spread(key = time_measure, value = value)

  return(z)
}
