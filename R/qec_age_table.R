#' Create age group table for QEC
#'
#' Produces rates (per 100,000) by age group for the QEC.
#' It's **really** important that the value for the denominator is the same across all records for the same age group
#'
#' @param data A data frame
#' @param age_var A variable giving the age group for the record
#' @param denom A variable giving the denominator for the age **group**
#' @return a tibble containing the aggregated data
#' @examples
#' dat <- data.frame(age_group = c(1, 1, 1, 2, 2, 2, 2),
#'                   denom = c(500000,500000,500000, 600000, 600000, 600000, 600000))
#' qec_age_table(data = dat, age_var = "age_group", denom = "denom")
#' @export

qec_age_table <- function(data, age_var, denom){
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("lazyeval", quietly = TRUE)) {
    stop("dplyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  z <- data %>%
    dplyr::group_by_(as.name(age_var)) %>%
    dplyr::summarise_(count = "dplyr::n()",
               denom = lazyeval::interp(~mean(var), var = as.name(denom))
    ) %>%
    dplyr::mutate(rate_per_100k = (count / denom) * 100000)
  return(z)
}
