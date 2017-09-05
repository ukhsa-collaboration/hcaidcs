#' mandatory_age_group_bacteraemia(0)
#'
#' Sub-function for creating mandatory age groups for bacteraemia cases
#'
#' @param x Age as a numeric value
#' @return An ordered factor giving the age group

mandatory_age_group_bacteraemia <- function(x){
  z <- cut(floor(x),
           breaks = c(-Inf, 0, 14, 44, 64, 74, 84, Inf),
           labels = c("<1", "1-14", "15-44", "45-64", "65-74", "75-84", "ge85"))
  return(z)
}
