#' Graph trend in percentage of cases by age group
#'
#' Prepares a graph for the trend in the proportion of cases that are from a particular age group.
#' Intended to be used in conjunction with \code{\link{aec_age_trend_rate}} to plot a side-by-side figure.
#' See \code{\link{age_trends_data}} for example of format for data to be used with this function.
#' @seealso \code{\link{aec_age_trend_rate}}
#'
#' @param collection One of CDI or Bacteraemia
#' @param data A data frame
#' @param x Variable giving financial year
#' @param y Variable giving percentage of cases the age group makes for all cases for year
#' @param sex Variable giving the sex group
#' @param group Variable giving age group (using mandatory age groupings)
#' @param log_scale Logical for whether a log y scale should be used. Defaults to FALSE
#' @examples
#' data(age_trends_data)
#' p <- aec_age_trend_pc(collection = "CDI", data = age_trends_data,
#'     x = "fyear6", y = "age_sex_pc", sex = "sex", group = "age_group_new")
#' p
#' @return A ggplot2 object
#' @export

aec_age_trend_pc <- function(collection, data, x, y, sex, group, log_scale = FALSE){
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(tolower(collection) == "cdi"){
    age_group_values <- c('#034e7b','#0570b0','#3690c0','#74a9cf','#a6bddb','#d0d1e6')
  }else if(tolower(collection) == "bacteraemia" ){
    age_group_values <- c('#007EE5','#228BE5','#4599E5','#68A7E5','#8AB5E5',
                          '#ADC3E5', '#D0D1E6')
  }else{
    stop("Please enter either CDI or Bacteraemia")
  }
  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y, group = group)) +
    ggplot2::geom_area(position = "stack", ggplot2::aes(fill = age_group_new)) +
    ggplot2::facet_grid(sex ~ .) +
    ggplot2::scale_fill_manual("Age group", values = age_group_values) +
    # ,
    #                       labels = c("2-14", "15-44", "45-64", "65-74", "75-84",
    #                                  expression(phantom(x) >=85))) +
    ggplot2::scale_y_continuous("Per cent cases") +
    ggplot2::guides(fill = FALSE) +
    ggplot2::scale_x_continuous("Financial year",
                       breaks = unique(data$fyear6),
                       labels = paste0(substr(unique(data$fyear6), 1,4), "/",
                                       substr(unique(data$fyear6), 5, 6)) ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
          strip.background = ggplot2::element_rect(fill="white"),
          strip.text.x = ggplot2::element_text(face = "bold"))
  return(p)
}

#' Graph trend in rate of cases by age and sex
#'
#' Prepares a graph for the trend in the rate of cases that are from a particular age group.
#' Intended to be used in conjunction with \code{\link{aec_age_trend_pc}} to plot a side-by-side figure.
#' See \code{\link{age_trends_data}} for example of format for data to be used with this function.
#' @seealso \code{\link{aec_age_trend_pc}}
#' @seealso \code{\link{aec_age_trend_rate_pc_change}}
#'
#' @param collection One of CDI or Bacteraemia
#' @param data A data frame
#' @param x Variable giving financial year
#' @param y Variable giving rate for age group by sex and year
#' @param sex Variable giving the sex group
#' @param group Variable giving age group (using mandatory age groupings)
#' @param log_scale Logical for whether a log y scale should be used. Defaults to FALSE
#'
#' @importFrom ggrepel geom_text_repel
#' @return A ggplot2 object
#' @examples
#' data(age_trends_data)
#' q <- aec_age_trend_rate(collection = "CDI", data = age_trends_data,
#'     x = "fyear6", y = "rate", sex = "sex", group = "age_group_new",
#'     log_scale = TRUE)
#' q
#' @export

aec_age_trend_rate <- function(collection, data, x, y, sex, group,log_scale = FALSE){
  if(!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(!requireNamespace("scales", quietly = TRUE)) {
    stop("The package scales is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(!requireNamespace("ggrepel", quietly = TRUE)) {
    stop("The package ggrepel is needed for this function to work. Please install it.",
         call. = FALSE)
  }

  assertthat::assert_that("age_group_new" %in% names(data),
                          msg = "data must contain a variable named age_group_new")


  if(tolower(collection) == "cdi"){
    age_group_values <- c('#034e7b','#0570b0','#3690c0','#74a9cf','#a6bddb','#d0d1e6')
    age_group_labels <- c("2-14", "15-44", "45-64", "65-74", "75-84",
                          expression(phantom(x) >=85))
  }else if(tolower(collection) == "bacteraemia" ){
    age_group_values <- c('#007EE5','#228BE5','#4599E5','#68A7E5','#8AB5E5',
                          '#ADC3E5', '#D0D1E6')
    age_group_labels <- c("<1", "1-14", "15-44", "45-64", "65-74", "75-84",
                          expression(phantom(x) >=85))
  }else{
    stop("Please enter either CDI or Bacteraemia")
  }

  # substitute ge85 for >= symbol

  data$age_group_new <- ifelse(data$age_group_new == "ge85",
                               sprintf('\u226585'), data$age_group_new)

  q <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y, group = group)) +
    ggplot2::geom_line(ggplot2::aes_string(colour = group), size = 1.5) +
    # facet_wrap(~ sex, ncol = 1) +
    # ggplot2::facet_grid(sex ~ .) +
    ggplot2::facet_grid(. ~ sex) + # change, want this to have two columns now
    ggplot2::scale_colour_manual("Age group", values = age_group_values,
                                 labels = age_group_labels) +
    ggplot2::scale_y_continuous("Rate, per 100,000 population",
                                trans = ifelse(log_scale == TRUE, "log10", "identity"),
                                labels = scales::comma) +
#    ggplot2::scale_y_continuous("Rate, per 100,000 population") +
    # guides(colour = FALSE) +
    ggplot2::scale_x_continuous("Financial year",
                       breaks = unique(data$fyear6),
                       labels = paste0(substr(unique(data$fyear6), 1,4), "/",
                                       substr(unique(data$fyear6), 5, 6)),
                       limits = c(min(data$fyear6), max(data$fyear6) + 101)
                       ) +
    ggrepel::geom_text_repel(data = subset(data, fyear6 == max(fyear6)),
                             ggplot2::aes(label = age_group_new),
                             nudge_x = 5.0,
                             point.padding = grid::unit(1.0, "lines")
                             ) +
    #  guides(colour = FALSE) +
    ggplot2::theme(legend.position = c(1,1),
                   legend.justification = c(0.5,0),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                                       vjust = 1),
                   strip.background = ggplot2::element_rect(fill="white"),
                   strip.text.x = ggplot2::element_text(face = "bold")
    )
  return(q)
}

#' aec_age_trend_rate
#'
#' Produce a bar graph showing the change in rate of infection over time, by age group.
#' This replaces \code{\link{aec_age_trend_pc}} in the Annual Epidemiological Commentary.
#' See \code{\link{age_trends_data}} for example of format for data to be used with this function.
#' @seealso \code{\link{aec_age_trend_pc}}
#' @seealso \code{\link{aec_age_trend_rate_pc_change}}
#'
#' @param collection One of CDI or Bacteraemia
#' @param data A data frame
#' @param x Variable giving financial year
#' @param y Variable giving rate for age group by sex and year
#' @param sex Variable giving the sex group
#' @param group Variable giving age group (using mandatory age groupings)
#' @param log_scale Logical for whether a log y scale should be used. Defaults to FALSE
#'
#' @return A ggplot2 object
#' @examples
#' data(age_trends_data)
#' q <- aec_age_trend_rate_bar(collection = "CDI", data = age_trends_data,
#'     x = "fyear6", y = "rate", sex = "sex", group = "age_group_new",
#'     log_scale = FALSE)
#' q
#' @export
aec_age_trend_rate_bar <- function(collection, data, x, y, sex, group,log_scale = FALSE){
  if(!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(!requireNamespace("scales", quietly = TRUE)) {
    stop("The package scales is needed for this function to work. Please install it.",
         call. = FALSE)
  }

  assertthat::assert_that("age_group_new" %in% names(data),
                          msg = "data must contain a variable named age_group_new")


  if(tolower(collection) == "cdi"){
    data$age_group_new <- as.character(data$age_group_new)
    data$age_group_new = factor(data$age_group_new,
                                levels = c("2-14", "15-44", "45-64", "65-74",
                                  "75-84", "ge85"),
                           labels = c("2-14", "15-44", "45-64", "65-74",
                                      "75-84", "\u2265 85"))
  }else if(tolower(collection) == "bacteraemia" ){
    data$age_group_new <- as.character(data$age_group_new)
    data$age_group_new = factor(data$age_group_new,
                                levels = c("<1", "1-14", "15-44", "45-64", "65-74",
                                           "75-84", "ge85"),
                                labels = c("<1", "1-14", "15-44", "45-64", "65-74",
                                           "75-84", "\u2265 85"))
  }else{
    stop("Please enter either CDI or Bacteraemia")
  }

  p <- ggplot2::ggplot(data = data,
                       ggplot2::aes(x = age_group_new, y = rate, group = fyear6)) +
    ggplot2::geom_bar(ggplot2::aes(fill = fyear6), stat = "identity",
                      position = ggplot2::position_dodge()) +
    ggplot2::facet_wrap( ~ sex) +
    # ggplot2::scale_fill_manual("Financial\nyear", values = my_palette) +
    ggplot2::scale_fill_continuous("Financial\nyear",
                                   breaks = unique(data$fyear6),
                                   labels = paste0(substr(unique(data$fyear6), 1,4),
                                                   "/",
                                                   substr(unique(data$fyear6), 5, 6)
                                                   )
                                   ) +
    ggplot2::scale_x_discrete("Age group") +
    ggplot2::scale_y_continuous("Rate, per 100,000 population",
                                label = scales::comma,
                                trans = ifelse(log_scale == TRUE, "log10",
                                               "identity")) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = FALSE)) +
    ggplot2::theme(#legend.position = c(0.9,1),
      legend.position = "bottom",
      #legend.justification = c(1,1),
      strip.background = ggplot2::element_rect(fill="white"),
      strip.text.x = ggplot2::element_text(face = "bold"),
      panel.spacing = ggplot2::unit(1.5, "lines")
    ) +
    NULL # this is added for tinkering, allows commenting out lines without breaking

  return(p)
}

#' aec_age_trend_rate_pc_change
#'
#' Plot the change in rate as a percentage from first year of surveillance.
#'
#' @param data a dataframe containing the following variables: fyear6, age_group_new, sex, rate
#' @param collection A string giving either "CDI" or "bacteraemia". This determines the age groups
#' @return A ggplot2 object
#' @seealso \code{\link{aec_age_trend_pc}}
#' @seealso \code{\link{aec_age_trend_rate}}
#' @importFrom magrittr %>%
#' @examples
#' data(age_trends_data)
#' p <- aec_age_trend_rate_pc_change(data = age_trends_data, collection = "CDI")
#' p
#' @export

aec_age_trend_rate_pc_change <- function(data, collection){

  if(!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(!requireNamespace("scales", quietly = TRUE)) {
    stop("The package scales is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(!requireNamespace("ggrepel", quietly = TRUE)) {
    stop("The package ggrepel is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The package dplyr is needed for this function to work. Please install it.",
         call. = FALSE)
  }

  assertthat::assert_that("age_group_new" %in% names(data),
                          msg = "data must contain a variable named age_group_new")
  assertthat::assert_that("sex" %in% names(data),
                          msg = "data must contain a variable named sex")
  assertthat::assert_that("fyear6" %in% names(data),
                          msg = "data must contain a variable named fyear6")
  assertthat::assert_that("rate" %in% names(data),
                          msg = "data must contain a variable named rate")

  if(tolower(collection) == "cdi"){
    age_group_values <- c('#034e7b','#0570b0','#3690c0','#74a9cf','#a6bddb','#d0d1e6')
    age_group_labels <- c("2-14", "15-44", "45-64", "65-74", "75-84",
                          expression(phantom(x) >=85))
  }else if(tolower(collection) == "bacteraemia" ){
    age_group_values <- c('#007EE5','#228BE5','#4599E5','#68A7E5','#8AB5E5',
                          '#ADC3E5', '#D0D1E6')
    age_group_labels <- c("<1", "1-14", "15-44", "45-64", "65-74", "75-84",
                          expression(phantom(x) >=85))
  }else{
    stop("Please enter either CDI or Bacteraemia")
  }

  plot_dat <- data %>%
    dplyr::select(fyear6, age_group_new, sex, rate) %>%
    dplyr::group_by(sex, age_group_new) %>%
    dplyr::arrange(sex, age_group_new,fyear6) %>%
    dplyr::mutate(start_value = rate[dplyr::row_number() == 1],
           change_from_start = (rate/start_value - 1) * 100) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(age_group_new = ifelse(age_group_new == "ge85",
                                         sprintf('\u226585'), age_group_new))


  # head(age_sex_change, n = 10)

  p <- ggplot2::ggplot(data = plot_dat, ggplot2::aes(x = fyear6, y = change_from_start,
                                         group = age_group_new)) +
    ggplot2::geom_line(ggplot2::aes(colour = age_group_new), size = 1.5) +
    ggplot2::scale_colour_manual("Age group", values = age_group_values,
                                 labels = age_group_labels) +
    ggplot2::facet_wrap( ~ sex) +
    ggplot2::scale_x_continuous("Financial year",
                       breaks = unique(plot_dat$fyear6),
                       labels = paste0(substr(unique(plot_dat$fyear6), 1,4), "/",
                                       substr(unique(plot_dat$fyear6), 5, 6)),
                       limits = c(min(plot_dat$fyear6), max(plot_dat$fyear6) + 101)) +
    ggplot2::scale_y_continuous("% change in rate over time") +
    ggrepel::geom_text_repel(data = subset(plot_dat, fyear6 == max(fyear6)),
                             ggplot2::aes(label = age_group_new),
                             nudge_x = 5.0,
                             point.padding = grid::unit(1.5, "lines") # point padding adds space around labelled point
    )+
    ggplot2::theme(legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
          strip.background = ggplot2::element_rect(fill="white"),
          strip.text.x = ggplot2::element_text(face = "bold"))

  return(p)
}

