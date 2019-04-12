#' factor_apportioned
#'
#' Converts apportioned column into a factor for production of long format tables.
#'
#' @seealso \code{\link{mo_tab_long}}, \code{\link{ann_tab_long}}
#'
#' @param x a column giving the apportioned status of a case. One of 1, 0, HO or CO
#' @return A factor with levels "Hospital-onset", "Community-onset" and "Total cases"
#' @export
#' @examples
#' factor_apportioned(1)
#' factor_apportioned("CO")

factor_apportioned <- function(x){
  assertthat::assert_that(x %in% c(1, 0, "HO", "CO"),
                          msg = "x must be one of 1, 0, HO or CO")
  x <- dplyr::case_when(
    x == 1 ~ "Hospital-onset",
    x == 0 ~ "Community-onset",
    x == "HO" ~ "Hospital-onset",
    x == "CO" ~ "Community-onset"
  )
  x <- factor(x, levels = c("Hospital-onset", "Community-onset", "Total cases"))
  return(x)
}

#' factor_prior_hc
#'
#' Converts data with prior healthcare exposure information into factor for
#' production of long format tables.
#'
#' @seealso \code{\link{mo_tab_long}}, \code{\link{ann_tab_long}}
#' @param x A character vector limited to hoha, coha, coia, coca, unknown_3_mo and all_blank
#' @return An ordered factor variable
#' @export
#' @examples
#' factor_prior_hc("hoha")

factor_prior_hc <- function(x){
  assertthat::assert_that(
    x %in% c("hoha", "coha", "coia", "coca", "unknown_3_mo", "all_blank"),
    msg = "x must be one of hoha, coha, coia, coca, unknown_3_mo, all_blank"
  )
  x <- dplyr::case_when(
    x == "hoha" ~ "Hospital-onset, healthcare associated",
    x == "coha" ~ "Community-onset, healthcare associated",
    x == "coia" ~ "Community-onset, indeterminate association",
    x == "coca" ~ "Community-onset, community associated",
    x == "unknown_3_mo" ~ "Unknown 3 months",
    x == "all_blank" ~    "No information"
  )

  x <- factor(x,
              levels = c("Hospital-onset, healthcare associated",
                         "Community-onset, healthcare associated",
                         "Community-onset, indeterminate association",
                         "Community-onset, community associated",
                         "Unknown 3 months", "No information", "Total cases"))
  return(x)
}
