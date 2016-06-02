#' Create age group from age
#'
#' Creates age group as ordered factor according to standard ages for HCAIDCS.
#' Values are rounded down using floor before grouping.
#'
#' @param x A numeric vector
#' @param collection An optional parameter giving the collection for CDI age grouping.
#' If the collection is not given, the bacteraemia age grouping is applied.
#' @return A factor vector with levels <1 1-14 15-44 45-64 65-74 75-84 ge85 for bacteraemia or 2-14 15-44 45-64 65-74 75-84 ge85 for cdi.
#' @seealso \code{\link{mandatory_age}}
#' @examples
#' mandatory_age_group(0)
#' mandatory_age_group(x = 0)
#' mandatory_age_group(0, "MRSA")
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
  #ifelse(missing(collection) == TRUE | tolower(collection) %in% c("mssa", "mrsa", "ecoli"),
  ifelse(missing(collection) == TRUE,
         z <- mandatory_age_group_bacteraemia(x),
         ifelse(tolower(collection) %in% c("mssa", "mrsa", "ecoli"),
                z <- mandatory_age_group_bacteraemia(x),
                ifelse(collection == "cdi", z <- mandatory_age_group_cdi(x), z <- NA)))
  return(z)
}

test_fun <- function(x, y){
  z <- ifelse(x %in% c(1,2) | missing(y) == TRUE, 1, 0)
  return(z)
}
test_fun(1,1)
test_fun(x = 1)
test_fun(x = 0)
test_fun(x = 0, y = 0)
