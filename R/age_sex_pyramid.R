#' Age sex pyramid
#'
#' Create an age-sex pyramid for commentaries.
#' Credit where it's due, much of this has been cribbed from Seb Fox's
#' fingertipscharts package.
#' \url{https://github.com/publichealthengland/fingertipscharts/blob/master/R/quick_charts.R}
#'
#' @param x A dataframe providing the data
#'
#' @return A ggplot object
#' @export
#' @examples
#' data(age_trends_data)
#' head(age_trends_data)
#'
#' ## To get the right sorting and labels for the age groups
#'
#' age_trends_data$age_group2 <- graph_age_groups(age_trends_data$age_group_new,
#'   data_collection = "cdi")
#'
#' p <- age_sex_pyramid(dat = subset(age_trends_data, fyear6 == 200708),
#'   age_group = age_group2, sex_var = sex, y_var = rate,
#'   y_var_label = "Rate, per 100,000 population")
#' p

age_sex_pyramid <- function(dat, age_group, sex_var, y_var, y_var_label){
  age_group <- rlang::enquo(age_group)
  sex_var <- rlang::enquo(sex_var)
  y_var<- rlang::enquo(y_var)

  pyramid_string <- ggplot2::aes_string(x = rlang::quo_text(age_group),
                                        y = rlang::quo_text(y_var),
                                        group = 1,
                                        fill = rlang::quo_text(sex_var)
                                        )

  extremex <- scales::breaks_pretty(n = 3)(0:max(abs(dplyr::pull(dat, !!y_var)),
                                         na.rm = T))

  dat <- dplyr::mutate(dat, (!!y_var) := ifelse(!!sex_var == "Male", -
                                                  (!!y_var), !!y_var))

  z <- ggplot2::ggplot(
      data = dplyr::filter(dat, (!!sex_var) == "Female"), pyramid_string) +
    ggplot2::geom_col() +
    ggplot2::geom_col(
      data = dplyr::filter(dat, (!!sex_var) == "Male"), pyramid_string) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      breaks = c(rev(-extremex), extremex[2:length(extremex)]),
      # labels = abs(c(rev(extremex), extremex[2:length(extremex)]))
      labels = abs_comma(abs(c(rev(extremex), extremex[2:length(extremex)])))
    ) +
    ggplot2::scale_x_discrete("Age group") +
    ggplot2::scale_fill_manual(name = "",
                      values = c("Male" = "#00B092",
                                 "Female" = "#822433"),
                      breaks = c("Male", "Female"),
                      labels = c("Male", "Female")
                      ) +
    cowplot::theme_cowplot() +
    ggplot2::theme(legend.position = 'bottom',
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::ylab(y_var_label)

  z
}

abs_comma <- function (x, ...) {
  format(abs(x), ..., big.mark = ",", scientific = FALSE, trim = TRUE)
}
