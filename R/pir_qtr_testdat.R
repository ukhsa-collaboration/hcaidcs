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
