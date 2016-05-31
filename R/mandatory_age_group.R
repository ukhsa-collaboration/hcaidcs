#' Create age group from age
#'
#' Creates age group as ordered factor according to standard ages for HCAIDCS.
#' Values are rounded down using floor before grouping.
#'
#' @param x A numeric vector
#' @param collection An optional parameter giving the collection for CDI age grouping
#' @return A factor vector with levels <1 1-14 15-44 45-64 65-74 75-84 ge85
#' @seealso \code{\link{mandatory_age}}
#' @examples
#' mandatory_age_group(0)
#' mandatory_age_group(0, "cdi")
#' mandatory_age_group(1, "cdi")
#' mandatory_age_group(2, "cdi")
#' mandatory_age_group(0.038)
#' mandatory_age_group(1)
#' mandatory_age_group(14)
#' mandatory_age_group(15)
#' mandatory_age_group(seq(1,100, 1))
#' mandatory_age_group(seq(1,100, 1), "cdi")
#' @export

mandatory_age_group <- function(x, collection = NULL){
  stopifnot(is.numeric(x) == TRUE)
  ifelse(missing(collection) == TRUE | tolower(collection) %in% c("mssa", "mrsa", "ecoli"),
         z <- cut(floor(x), breaks = c(-Inf, 0, 14, 44, 64, 74, 84, Inf),
                  labels = c("<1", "1-14", "15-44", "45-64", "65-74", "75-84", "ge85")),
         ifelse(collection == "cdi",
                z <- factor(cut(floor(x), breaks = c(1, 14, 44, 64, 74, 84, Inf),
                         labels = c("2-14", "15-44", "45-64", "65-74", "75-84", "ge85"),
                         levels = c("2-14", "15-44", "45-64", "65-74", "75-84", "ge85"))),
                NA
                )
         )
  return(z)
}


