#' Graph trend in percentage of cases by age group
#'
#' Prepares a graph for the trend in the proportion of cases that are from a particular age group.
#' Intended to be used in conjunction with \code{\link{aec_age_trend_rate}} to plot a side-by-side figure.
#' See \code{\link{trends_data}} for example of format for data to be used with this function.
#' @seealso \code{\link{aec_age_trend_rate}}
#' #' @seealso \code{\link{trends_data}}
#'
#' @param collection One of CDI or Bacteraemia
#' @param data A data frame
#' @param x Variable giving financial year
#' @param y Variable giving percentage of cases the age group makes for all cases for year
#' @param sex Variable giving the sex group
#' @param group Variable giving age group (using mandatory age groupings)
#' @examples
#' data(age_trends_data)
#' p <- aec_age_trend_pc(collection = "CDI", data = age_trends_data,
#'     x = "fyear6", y = "age_sex_pc", sex = "sex", group = "age_group_new")
#' p
#' @return A ggplot2 object
#' @export

aec_age_trend_pc <- function(collection, data, x, y, sex, group){
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
#' See \code{\link{trends_data}} for example of format for data to be used with this function.
#' @seealso \code{\link{aec_age_trend_pc}}
#'
#' @param collection One of CDI or Bacteraemia
#' @param data A data frame
#' @param x Variable giving financial year
#' @param y Variable giving rate for age group by sex and year
#' @param sex Variable giving the sex group
#' @param group Variable giving age group (using mandatory age groupings)
#' @return A ggplot2 object
#' @examples
#' data(age_trends_data)
#' q <- aec_age_trend_rate(collection = "CDI", data = age_trends_data,
#'     x = "fyear6", y = "rate", sex = "sex", group = "age_group_new")
#' q
#' @export

aec_age_trend_rate <- function(collection, data, x, y, sex, group){
  if(!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(!requireNamespace("scales", quietly = TRUE)) {
    stop("The package scales is needed for this function to work. Please install it.",
         call. = FALSE)
  }
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
  q <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y, group = group)) +
    ggplot2::geom_line(ggplot2::aes_string(colour = group), size = 1.5) +
    # facet_wrap(~ sex, ncol = 1) +
    ggplot2::facet_grid(sex ~ .) +
    ggplot2::scale_colour_manual("Age group", values = age_group_values, labels = age_group_labels) +
    ggplot2::scale_y_continuous("Rate, per 100,000 population", labels = scales::comma) +
#    ggplot2::scale_y_continuous("Rate, per 100,000 population") +
    # guides(colour = FALSE) +
    ggplot2::scale_x_continuous("Financial year",
                       breaks = unique(data$fyear6),
                       labels = paste0(substr(unique(data$fyear6), 1,4), "/",
                                       substr(unique(data$fyear6), 5, 6)) ) +
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

