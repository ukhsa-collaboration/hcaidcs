#' Monthly factsheet figure 1 function
#'
#' Function to produce figure 1 of the monthly factsheet.
#'
#' @param data A data frame giving the counts of cases for a month per row, with a date colummn which will be used as x variable and count of cases for a collection to be used as the y variable.
#' @param collection A string giving the name of the collection, which is the same as the name of the column in the data frame
#'
#' @return A ggplot showing the trend in counts of cases of the given collection
#' @examples
#'
#' data(mf_trend_data)
#' head(mf_trend_data)
#'
#' my_plot <- mf_fig1_fun(data = mf_trend_data, collection = "mrsa")
#' my_plot
#' my_plot <- mf_fig1_fun(data = mf_trend_data, collection = "mssa")
#' my_plot
#' my_plot <- mf_fig1_fun(data = mf_trend_data, collection = "ecoli")
#' my_plot
#' names(mf_trend_data)[5] <- "kleb"
#' my_plot <- mf_fig1_fun(data = mf_trend_data, collection = "kleb")
#' my_plot
#'
#' @export

mf_fig1_fun <- function(data, collection){
  my_minor_breaks <- subset(data, lubridate::day(t) == 1 & lubridate::month(t) == 6, select = t)
  my_minor_breaks <- as.data.frame(my_minor_breaks)
  my_minor_breaks <- my_minor_breaks[,1]
  my_major_breaks <- subset(data, lubridate::day(t) == 1 & lubridate::month(t) == 12, select = t)
  my_major_breaks <- as.data.frame(my_major_breaks)
  my_major_breaks <- my_major_breaks[,1]

  # title_collection <- dplyr::case_when(
  #   collection == "mssa" ~ "MSSA",
  #   collection == "mrsa" ~ "MRSA",
  #   collection == "cdi" ~ expression(italic("C. difficile")),
  #   collection == "ecoli" ~ expression(italic("C. difficile")),
  #   collection == "kleb" ~ expression(paste(italic("Klebsiella"), "\sspp.") ),
  #   collection == "paer" ~ expression(italic("P. aeruginosa"))
  # )

  collection_title <- switch(collection,
    mssa = expression("MSSA"),
    mrsa = expression("MRSA"),
    cdi = expression(italic("C. difficile")),
    ecoli = expression(italic("E. coli")),
    kleb = expression(paste(italic("Klebsiella"), " spp.") ),
    paer = expression(italic("P. aeruginosa")),
    ecoli_ta = expression(paste("Hospital-onset ", (italic("E. coli"))))
  )

  z <- ggplot2::ggplot(data, ggplot2::aes_string(x = "t", y = collection)) +
    ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::scale_x_date("",
                         breaks = my_major_breaks,
                         minor_breaks = my_minor_breaks,
                         date_labels = "%b %y") +
    ggplot2::scale_y_continuous("", labels = scales::comma,
                               expand = c(0, 0),
                       limits = c(0, max(data[, collection]) * 1.05)
    ) + # force y to zero and NA gives max y. expand = c(0,0) at J. Giltrow's request
    ggplot2::labs(title = collection_title) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0),
                   axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1, vjust = 1)) +
    cowplot::background_grid(major = "xy", minor = "x", colour.major = "grey82",
                    colour.minor = "grey92")
  return(z)
}
