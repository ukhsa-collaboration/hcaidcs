#' AEC time to onset plot
#'
#' Plots the stacked area chart for time-to-onset for the AEC
#' Although the function takes a single argument (a data frame containing the proportion of cases for inpatients, by time to onset and financial year,
#' it relies on the presence of the following variables to that data frame, which must be precisely named:
#' * \code{fyear6} The financial year in format YYYY/YY
#' * \code{tto_group} Sex variable, to allow side-by-side bars
#' * \code{pc} Proportion of cases
#' @param x A data frame as specified above.
#' @return A ggplot object
#' @examples
#' \dontrun{
#' structure(list(fyear6 = c(201011L, 201011L, 201011L, 201516L, 201516L,
#' 201516L),
#' tto_group = structure(c(3L, 2L, 1L, 3L, 2L, 1L),
#' .Label = c("< 2 days", "2-6 days", ">= 7 days"), class = "factor"),
#' pc = c(33L, 33L, 34L, 20L, 30L, 50L)), class = "data.frame",
#' row.names = c(NA, -6L), .Names = c("fyear6", "tto_group", "pc"))
#'
#' p <- aec_tto_plot(dat)
#' p
#' }
#' @export

aec_tto_plot <- function(x){
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  ggplot2::ggplot(data = x,
                  ggplot2::aes(x = fyear6, y = pc)) +
    ggplot2::geom_area(position = "stack", ggplot2::aes(fill = tto_group)) +
    ggplot2:: scale_x_continuous("Financial year",
                       breaks = unique(x$fyear6),
                       labels = paste0(substr(unique(x$fyear6), 1,4), "/", substr(unique(x$fyear6), 5, 6))
    ) +
    ggplot2::scale_y_continuous("Per cent inpatient cases") +
    ggplot2::scale_fill_discrete("", labels = c("< 2 days", "2 - 6 days", "\u2265 7 days"))
}

