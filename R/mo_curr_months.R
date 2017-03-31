#' Create months for monthly outputs
#'
#' Use this to create a vector containing the 13 months covered by the mandatory surveillance monthly outputs.
#' @param x A date
#' @return A vector containing 13 months
#' @examples
#' mo_curr_months(lubridate::today())
#' @export

mo_curr_months <- function(x){
  x <- x - lubridate::years(1)
  mo_df <- data.frame(mo_seq = seq(1, 13, 1),
                      mo_now = lubridate::rollback(x, roll_to_first = TRUE))
  mo_df$first_day_text <- mo_df$mo_now %m+% months(mo_df$mo_seq - 1)
  mo_df$the_month <- paste(as.character(
    lubridate::month(mo_df$first_day_text, label = TRUE, abbr = FALSE)),
    lubridate::year(mo_df$first_day_text))
  mo_df$the_month <- factor(mo_df$the_month, levels = as.ordered(mo_df$the_month))
  the_months <- mo_df$the_month
  return(the_months)
}
