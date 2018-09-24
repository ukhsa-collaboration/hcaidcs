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
#' data(subregion_test_data)
#'
#' # Join example data to subregions_sp_df based on ODS_CD variable
#' subregions_sp_df <- dplyr::left_join(subregions_sp_df, subregion_test_data)
#'
#' # Need to convert rand_val to factor and correctly label levels
#' # Can't do this before left_join as factors are coerced to characters.
#' subregions_sp_df$rand_val <- cut(subregions_sp_df$rand_val,
#'   c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
#'   labels = c("0.0-0.2", "0.21-0.4", "0.41-0.6", "0.61-0.8", "0.81-1.0" ))
#' # Rename for plotting
#' subregions_sp_df <- dplyr::rename(subregions_sp_df, rate = rand_val)
#' p <- aec_subregion_plot(subregions_sp_df)
#' p
#' @export

aec_subregion_plot <- function(x){
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  ggplot2::ggplot(data = x,
                  ggplot2::aes(x = long, y = lat, group = group,
                       fill = rate,
                       label = ODS_CD)) +
    ggplot2::geom_polygon() +
    ggplot2::geom_path(colour = "grey50") +
    ggplot2::geom_text(ggplot2::aes(x = centroid_long, y = centroid_lat), colour = "black") +
    ggplot2::scale_x_continuous("", labels = NULL, breaks = NULL) +
    ggplot2::scale_y_continuous("", labels = NULL, breaks = NULL) +
    ggplot2::scale_fill_brewer("Rate, per\n100,000\npopulation", palette = "Blues") +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE)) +
    ggplot2::theme(legend.position = c(0.1, 0.5), legend.justification = c(0.5,0.5) ) +
    ggplot2::coord_equal()
}
