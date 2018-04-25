#' Create factor variable from financial quarter
#'
#' Creates factor variable from financial quarter to preserve correct ordering
#' in wide reshape of data.
#'
#' @param data A dataframe
#' @param quarter_in_long_format A string variable giving long financial year
#' created using \code{\link{fy_long}}. Must be quoted.
#' @return A dataframe with a new variable 'fin_qtr_ftr'
#' @seealso \code{\link{fy_long}}
#' @seealso \code{\link{ordered_m_y}}
#' @examples
#' dat <- data.frame(sample_date = lubridate::dmy("01/01/2001"))
#' dat$sample_fy <- fy_long(dat$sample_date)
#' dat <- ordered_fin_qtr(dat, "sample_fy")
#' class(dat$fin_qtr_ftr)
#' levels(dat$fin_qtr_ftr)
#'
#' dat <- data.frame(sample_date = lubridate::dmy("01/01/2001"))
#' dat$sample_fq <- fq_long(dat$sample_date)
#' dat <- ordered_fin_qtr(dat, "sample_fq")
#' class(dat$fin_qtr_ftr)
#' levels(dat$fin_qtr_ftr)
#'
#' dat <- data.frame(sample_date = c(lubridate::dmy("01/01/2001"),
#'    lubridate::dmy("01/04/2001"),
#'    lubridate::dmy("01/07/2001"),
#'    lubridate::dmy("01/10/2001"),
#'    lubridate::dmy("01/01/2002")
#'    ))
#' dat$sample_fq <- fq_long(dat$sample_date)
#' dat <- ordered_fin_qtr(dat, "sample_fq")
#' class(dat$fin_qtr_ftr)
#' levels(dat$fin_qtr_ftr)
#'
#' @importFrom magrittr "%>%"
#' @export


ordered_fin_qtr <- function(data, quarter_in_long_format){
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("lubridate is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  z <- data %>% dplyr::mutate_(temp_year = lazyeval::interp(~substr(var, nchar(var)-4, nchar(var)),
                                                            var = as.name(quarter_in_long_format) ),
                               text_qtr = lazyeval::interp(~substr(var, 1, nchar(var)-5),
                                                           var = as.name(quarter_in_long_format) )) %>%
    dplyr::mutate(temp_year = as.numeric(temp_year),
                  temp_year = ifelse(text_qtr == "January to March", temp_year-1, temp_year)) %>%
    dplyr::mutate(text_qtr2 = factor(text_qtr, levels = c("April to June",
                                                          "July to September",
                                                          "October to December",
                                                          "January to March"))) %>%
    dplyr::arrange(temp_year, text_qtr2) %>%
    dplyr::mutate_(fin_qtr_ftr = lazyeval::interp(~factor(var, levels = as.ordered(unique(var))),
                                                  var = as.name(quarter_in_long_format))
    ) %>%
    dplyr::select(-temp_year, -text_qtr, -text_qtr2)
}

