#' @title Calculates inpatient bed-days from Actue Trust KH03 returns
#'
#' @description This package calculates NHS Acute Trusts's inpatient bed-days using NHSI's published mandatory KH03 return
#'
#'
#'
#' @param data A dataframe
#' @param kh03 Trust specific KH03 mandatory return
#' @param qtr Financial quarter. Example format; 20171 (Apr-Jun 2017), 20172 (Jul-Sep 2017), 20173 (Oct-Dec 2017) and 20172 (Jan-Mar 2018)
#'
#' @return Dataframe with the number of days in each quarter with corresponding inpatient bed-days for the same period
#'
#' @examples
#' average_kh03 <- data.frame(
#' fq = c(20171,20172,20173,20174,20151,20152,20153,20154),
#' trust_code = c(rep("RX1",4), rep("RYJ",4)),
#' trust_name = c(rep("Nottingham University Hospitals",4), rep("Imperial College Healthcare",4)),
#' kh03_avg = c(1425,1357,1392,1481,965,940,933,970)
#' )
#'
#' bed_days_fq(average_kh03,"kh03_avg","fq")
#'
#' @export

bed_days_fq <- function(data, kh03, qtr){

  dataset <- dplyr::select(data,"kh03"=kh03, "qtr" = qtr)
  dataset <- dataset %>%
    dplyr::mutate(
      cyear = ifelse(substr(qtr,5,5) == "4", as.numeric(substr(qtr,1,4))+1, as.numeric(substr(qtr,1,4)) ),
      days = dplyr::case_when(
        substr(qtr,5,5) == "1" ~ 91,
        substr(qtr,5,5) %in% c("2","3") ~ 92,
        substr(qtr,5,5) == "4" & cyear%% 4 == 0 ~ 90,
        substr(qtr,5,5) == "4" & cyear%% 4 != 0 ~ 91
        ),
      bed_days = kh03 * days
      ) %>%
    dplyr::select(days, bed_days)

  dataset
}
