#' Compare two data frames to identify cells which are different
#'
#' Used to identify cells which have changed between publications for later highlighting in Excel export.
#' With thanks to Amy Mikhail for writing this function for me.
#' Not currently production.
#'
#' @param currentdf A data frame in wide format giving the values for the current publication.
#' @param lastdf A data frame in wide format giving the values for the most recent publication prior to the one being prepared.
#' @return A matrix specifying cells that have changed.
#' @examples
#' \dontrun{
#' dontrun added while in development otherwise travis fails
#' last_pub <- data.frame(org = c("a", "b", "c"),
#' jan16 = sample(1:100, 3, replace=TRUE),
#' feb16 = sample(1:100, 3, replace=TRUE),
#' mar16 = sample(1:100, 3, replace=TRUE),
#' jan17 = sample(1:100, 3, replace=TRUE))
#'
#' # copy the last pub, but drop the oldest month
#' this_pub <- last_pub[c(1,3:5)]
#' # add new month
#' this_pub$feb17 <- sample(1:100, 3, replace=TRUE)
#' # change one value only
#' this_pub$mar16[2] <- this_pub$mar16[2] + 1
#' # Checki it out:
#' this_pub
#' highlight <- data_compare(this_pub, last_pub)
#' }
#' @export

data_compare <- function(currentdf, lastdf) {

  # Handy wrapper for 'not in':
  `%!in%` = Negate(`%in%`)

  # Create a data.frame showing results of value comparison for each cell
  results = as.data.frame(currentdf[, names(currentdf) %in% names(lastdf)] ==
                            lastdf[, names(lastdf) %in% names(currentdf)])

  # Use 'not in' to get column names not included in the comparison
  cols2add = names(currentdf)[names(currentdf) %!in% names(lastdf)]

  # Add the extra columns to the results and fill with NAs:
  for(i in cols2add)
    results[,i] <- NA

  # Make sure columns match the original order in currentdf:
  results = results[names(currentdf)]

  # Now get the array indices of the values that have changed:
  cells2flag = which(results == FALSE, arr.ind = TRUE)

  # Return the matching data.frame that can be used to colour cells:
  return(cells2flag)
}
