#' AEC time to onset plot
#'
#' Plots the stacked area chart for time-to-onset for the AEC
#' Although the function takes only two arguments (a data frame containing the proportion of cases for inpatients, by time to onset and financial year),
#' it relies on the presence of the following variables to that data frame, which must be precisely named:
#' \describe{
#' \item{\code{fyear6}}{The financial year in format YYYY/YY}
#' \item{\code{tto_group}}{Grouped time to onset variable, to allow side-by-side bars}
#' \item{\code{pc}}{Proportion of cases}
#' }
#'
#' @param x A data frame as specified above.
#' @param collection One of "mrsa", "mssa", "cdi" or "ecoli"
#' @return A ggplot object
#' @examples
#' \dontrun{
#' dat <- structure(list(fyear6 = c(201011L, 201011L, 201011L, 201516L, 201516L,
#' 201516L),
#' tto_group = structure(c(3L, 2L, 1L, 3L, 2L, 1L),
#' .Label = c("< 2 days", "2-6 days", ">= 7 days"), class = "factor"),
#' pc = c(33L, 33L, 34L, 20L, 30L, 50L)), class = "data.frame",
#' row.names = c(NA, -6L), .Names = c("fyear6", "tto_group", "pc"))
#'
#' p <- aec_tto_plot(dat)
#' p
#' p <- aec_tto_plot(dat, collection = "mrsa")
#' p
#' p <- aec_tto_plot(dat, collection = "cdi")
#' p
#' }
#' @export

aec_tto_plot <- function(x, collection = NULL){
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  bacteraemias <- c("mrsa", "mssa", "ecoli")
  if(missing(collection) == TRUE || tolower(collection) %in% bacteraemias){
    ggplot2::ggplot(data = x,
                    ggplot2::aes(x = fyear6, y = pc)) +
      ggplot2::geom_area(position = "stack", ggplot2::aes(fill = tto_group, colour = tto_group)) +
      ggplot2:: scale_x_continuous("Financial year",
                                   breaks = unique(x$fyear6),
                                   labels = paste0(substr(unique(x$fyear6), 1,4), "/", substr(unique(x$fyear6), 5, 6))
      ) +
      ggplot2::scale_y_continuous("Per cent inpatient cases") +
      ggplot2::scale_fill_brewer("", palette = "Blues", labels = c("< 2 days", "2 - 6 days", "\u2265 7 days")) +
      ggplot2::scale_colour_brewer("", palette = "Blues", labels = c("< 2 days", "2 - 6 days", "\u2265 7 days")) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.5, vjust = 0.5)) +
      cowplot::theme_cowplot()
  }else{
    if(collection == "cdi"){
      ggplot2::ggplot(data = x,
                      ggplot2::aes(x = fyear6, y = pc)) +
        ggplot2::geom_area(position = "stack", ggplot2::aes(fill = tto_group, colour = tto_group)) +
        ggplot2:: scale_x_continuous("Financial year",
                                     breaks = unique(x$fyear6),
                                     labels = paste0(substr(unique(x$fyear6), 1,4), "/", substr(unique(x$fyear6), 5, 6))
        ) +
        ggplot2::scale_y_continuous("Per cent inpatient cases") +
      ggplot2::scale_fill_brewer("", palette = "Blues", labels = c("< 3 days", "3 - 6 days", "\u2265 7 days")) +
        ggplot2::scale_colour_brewer("", palette = "Blues", labels = c("< 3 days", "3 - 6 days", "\u2265 7 days")) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.5, vjust = 0.5)) +
        cowplot::theme_cowplot()
    }
  }
}
