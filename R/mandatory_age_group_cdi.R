#' Mandatory age group for CDI cases
#' Calculates the mandatory age grouping based on numeric age
#' @param x Age in years, calculated by \code{\link{mandatory_age}}
#' mandatory_age_group_cdi(0)
#' mandatory_age_group_cdi(2)

mandatory_age_group_cdi <- function(x){
  z <- factor(cut(floor(x), breaks = c(1, 14, 44, 64, 74, 84, Inf),
                labels = c("2-14", "15-44", "45-64", "65-74", "75-84", "ge85"),
                levels = c("2-14", "15-44", "45-64", "65-74", "75-84", "ge85")))
  return(z)
}
