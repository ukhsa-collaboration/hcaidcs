#' Provides grouped time to onset for bacteraemia cases
#'
#' @param time_diff The time to onset as calculated by \code{\link{time_to_onset}}
#' @return An ordered factor
#' @seealso \code{\link{time_to_onset}}
#' @seealso \code{\link{group_tto_bacteraemia}}
#' @examples
#' onset <- lubridate::dmy("05-01-2011")
#' admitted <- lubridate::dmy("01-01-2011")
#' patient_cat <- "In-patient"
#' patient_loc <- "NHS Acute Trust"
#' tto <- time_to_onset(admitted, onset, patient_cat, patient_loc)
#' group_tto_cdi(tto)
#' @export

group_tto_cdi <- function(time_diff){
    z <- ifelse(time_diff >= 0 & time_diff < 3, "< 3 days",
                ifelse(time_diff >= 2 & time_diff <= 6, "3 - 6 days",
                       ifelse(time_diff > 6, "\u2265 7", NA)))
    z <- factor(z, levels = c("< 3 days", "3 - 6 days", "\u2265 7"))
    return(z)
}
