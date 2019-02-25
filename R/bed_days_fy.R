#' @title Calculates inpatient bed-days from Actue Trust KH03 returns
#'
#' @description This package calculates NHS Acute Trusts's inpatient bed-days using NHSI's published mandatory KH03 return
#'
#'
#'
#' @param data A dataframe
#' @param kh03 Trust specific KH03 mandatory return
#' @param fy Financial year. Example format; 201718 (Apr 2017 to March 2018) and 201819 (Apr 2018 to March 2019)
#'
#' @return Dataframe with the number of days in each financial year with corresponding inpatient bed-days for the same period
#'
#' @examples
#' average_kh03 <- data.frame(
#' fy = c(200708,200809,200708,200809),
#' trust_code = c(rep("RX1",2), rep("RYJ",2)),
#' trust_name = c(rep("Nottingham University Hospitals",2), rep("Imperial College Healthcare",2)),
#' kh03_avg = c(1508,1537,1211,1250)
#' )
#'
#' bed_days_fy(average_kh03,"kh03_avg","fy")
#'
#' @export

bed_days_fy <- function(data, kh03, fyear){

  dataset <- dplyr::select(data,"kh03"=kh03, "fyear" = fyear)
  dataset <- dataset %>%
    dplyr::mutate(
      cyear = as.numeric(substr(fyear,1,4)),
      days = as.numeric((lubridate::dmy(paste("31/03/", cyear+1, sep="")) - lubridate::dmy(paste("01/04/", cyear, sep=""))) + 1) ,
      bed_days = kh03 * days
    ) %>%
    dplyr::select(days, bed_days)

  dataset
}
