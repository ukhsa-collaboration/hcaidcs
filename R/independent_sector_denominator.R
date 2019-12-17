#' @title Inpatient modified bed-days from Independent Sector Providers
#'
#' @description This function calculates inpatient modified bed-days as used in Public Health England's Independent Sector reports
#'
#'
#' @param data  A data frame
#' @param bd_start_dt A date vector. The first day of the period of interest
#' @param bd_end_dt A date vector. The last day of the period of interest
#' @param admitted_dt A date vector. Date of patient admission
#' @param discharge_dt A date vector. Date patient was discharged
#'
#' @return A dataframe containing the modified bed-days, number of days admitted and discharge for the period of interest
#'
#' @examples
#'
#' library(lubridate)
#' library(dplyr)
#'
#' data <- data.frame(bd_start_dt = dmy("01/04/2017"),
#'                     bd_end_dt = dmy("31/03/2018"),
#'                     admitted_dt = dmy("17/03/2017"),
#'                     discharge_dt = dmy("01/04/2017"))
#'
#' cbind(data, independent_sector_denominator(data, bd_start_dt, bd_end_dt, admitted_dt, discharge_dt))
#'
#' data <- data.frame(
#'  admission_dt = c("17/03/2017","01/04/2017","01/04/2017","17/03/2017","01/04/2017","31/03/2018","23/04/2018","01/03/2018"),
#'  discharge_dt = c("02/04/2017","01/04/2017","03/04/2017","01/04/2018","23/04/2018","23/04/2018","23/04/2018","19/06/2018")
#' )
#'
#' data$admission_dt <- dmy(data$admission_dt)
#' data$discharge_dt <- dmy(data$discharge_dt)
#' data$start_date <- dmy("01/04/2017")
#' data$end_date <- dmy("31/03/2018")
#'
#' modified_bds <- independent_sector_denominator(data, bd_start_dt = start_date,
#'                                           bd_end_dt = end_date,
#'                                           admitted_dt = admission_dt,
#'                                           discharge_dt = discharge_dt)
#' cbind(data, modified_bds)
#'
#' @export

independent_sector_denominator <- function(
  data,
  bd_start_dt,
  bd_end_dt,
  admitted_dt,
  discharge_dt
){

  bd_start_dt <- select(data, !!enquo(bd_start_dt))[[1]]
  bd_end_dt <- select(data, !!enquo(bd_end_dt))[[1]]
  admitted_dt <- select(data, !!enquo(admitted_dt))[[1]]
  discharge_dt <- select(data, !!enquo(discharge_dt))[[1]]

  #only those admitted on or before the last day of period of interest and discharged on or after the first day of period of interest
  z <- ifelse(admitted_dt <= bd_end_dt & discharge_dt >= bd_start_dt ,
              ifelse(discharge_dt <= bd_end_dt,
                     discharge_dt, bd_end_dt + 1
              ), 0)

  a <- ifelse(admitted_dt <= bd_end_dt & discharge_dt >= bd_start_dt ,
              ifelse(admitted_dt > bd_start_dt,
                     admitted_dt, bd_start_dt
              ), 0)

  #pid discharge; first day as day 1 not day 0
  pid_discharge <- ifelse(discharge_dt >= bd_start_dt & discharge_dt <= bd_end_dt,1,0)

  #discharge date before admission date
  pid_discharge <- ifelse((z-a) < 0 , 0, pid_discharge)
  pid_bd <- ifelse((z-a) < 0, 0, (z-a))

  mod_is_denom <- pid_bd + pid_discharge

  output <- data.frame(
    pid_bd = pid_bd,
    pid_discharge = pid_discharge,
    mod_is_denom = mod_is_denom
  )

  return(output)
}
