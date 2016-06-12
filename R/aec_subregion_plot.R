#' Plot rates by NHS subregion
#'
#' Subregions (aka local offices) are subdivisions of the four NHS regions.
#' This plots rates by region in a choropleth map.
#' Although the function takes a single argument (the spatial data frame),
#' it relies on the presence of several variables to that data frame:
#' * \code{long} The longitude
#' * \code{lat} The latitude
#' * \code{rate} The rate to be plotted as a filled value
#' * \code{ODC_CD} The ODS code for the subregion for labelling
#' * \code{centroid_long} The longitude for the centroid of a polygon
#' * \code{centroid_lat} The latitude for the centroid of a polygon
#' Both of the last two are used for the plotting of the labels within the polygon.
#' @param x A spatial dataframe as described above.
#' @return A ggplot2 object
#' @examples
#' \dontrun{
#' dat <- structure(list(ODS_CD = c("Q70", "Q71", "Q72", "Q73", "Q74", "Q75",
#' "Q76", "Q77", "Q78", "Q79", "Q80", "Q81", "Q82"),
#' rate = c(0.541357619, 0.509643862, 0.976268126, 0.480329843, 0.704037304,
#' 0.962522235, 0.601202076, 0.582732042, 0.292755103, 0.460313728, 0.914434938,
#' 0.858357928, 0.959438418)),
#' .Names = c("ODS_CD", "rate"), class = "data.frame", row.names = c(NA, -13L))
#' data("subregions_sp_df")
#'
#' subregions_sp_df <- dplyr::left_join(subregions_sp_df, dat)
#' # Need to convert rate to factor and correctly label levels
#' # Can't do this before left_join as factors are coerced to characters.
#' subregions_sp_df$rate <- cut(subregions_sp_df$rate, c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
#' labels = c("0.0-0.2", "0.21-0.4", "0.41-0.6", "0.61-0.8", "0.81-1.0" ))
#' p <- aec_subregion_plot(subregions_sp_df)
#' p
#'
#' }
#' @export

aec_subregion_plot <- function(x){
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("viridis", quietly = TRUE)) {
    stop("The package *viridis* needed for this function to work. Please install it.",
         call. = FALSE)
  }
  ggplot2::ggplot(data = x,
                  ggplot2::aes(x = long, y = lat, group = group,
                       fill = rate,
                       label = ODS_CD)) +
    ggplot2::geom_polygon() +
    ggplot2::geom_path(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(x = centroid_long, y = centroid_lat), colour = "aquamarine2") +
    ggplot2::scale_x_continuous("", labels = NULL, breaks = NULL) +
    ggplot2::scale_y_continuous("", labels = NULL, breaks = NULL) +
    #viridis::scale_fill_viridis("Rate, per\n100,000\npopulation", low = "#56B1F7", high = "#132B43") +
    viridis::scale_fill_viridis("Rate, per\n100,000\npopulation", option = "plasma", discrete = TRUE,
                                begin = 0, end = 0.8) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE)) +
    ggplot2::coord_equal()
}
