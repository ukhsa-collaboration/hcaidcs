#' graph_age_groups
#'
#' Produces nice age groups for plotting on graphs. Different output for
#' CDI and BSI
#'
#' @seealso \code{\link{age_sex_pyramid}} for its use in a graph.
#'
#' @param x A character vector of age groups
#' @param data_collection a string giving the data collection of the labels to
#'  be converted.
#'
#' @return A factor with labels that can be printed in ggplot2
#' @export
#'
#' @examples
#' data(age_trends_data)
#' head(age_trends_data)
#'
#' ## To get the right sorting and labels for the age groups
#'
#' age_trends_data$age_group2 <- graph_age_groups(age_trends_data$age_group_new,
#'   data_collection = "cdi")

graph_age_groups <- function(x, data_collection){
  if(tolower(data_collection) %in%
     c("cdi", "c. difficile", "clostridium difficile",
       "clostridioides difficile")){
    x <- factor(x,
                levels = c("2-14", "15-44", "45-64", "65-74", "75-84", "ge85"),
                labels = c("2-14", "15-44", "45-64", "65-74", "75-84",
                           "\u2265 85")
                )
  }else{
    x <- factor(x,
                levels = c("<1", "1-14", "15-44", "45-64", "65-74", "75-84", "ge85"),
                labels = c("<1", "1-14", "15-44", "45-64", "65-74", "75-84",
                           "\u2265 85")
    )
  }
  return(x)
}
