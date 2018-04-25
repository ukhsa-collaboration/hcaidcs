#' Create age group table for QEC
#'
#' Produces rates (per 100,000) by age group for the QEC.
#' It's **really** important that the value for the denominator is the same across all records for the same age group
#'
#' @param df A data frame
#' @param age_var A variable giving the age group for the record
#' @param denom A variable giving the denominator for the age **group**
#' @return a tibble containing the aggregated data
#' @examples
#' dat <- data.frame(age_group = c(1, 1, 1, 2, 2, 2, 2),
#'                   denominator = c(500000,500000,500000, 600000, 600000, 600000, 600000))
#' qec_age_table(df = dat, age_var = age_group, denom = denominator)
#' @export

qec_age_table <- function(df, age_var, denom){
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("lazyeval", quietly = TRUE)) {
    stop("dplyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  group_var <- dplyr::enquo(age_var)
  denom_var <- dplyr::enquo(denom)

  z <- df %>%
    dplyr::group_by(!!group_var) %>%
    dplyr::summarise(
      # a_count = "dplyr::n()",
      a_count = n(),
      denom = min(!!denom_var)
               )  %>%
    dplyr::mutate(rate_per_100k = (a_count / denom) * 100000)
  return(z)
}
