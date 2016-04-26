#' for ecoli apportioning

ec_apportion_step1 <- function(patient_location, date_entered){
  trust_locs <- c("NHS Acute Trust", "")
  z <- ifelse(patient_location %in% trust_locs | 
                (patient_location == "Unknown" & date_entered >= lubridate::dmy("26-10-2015")),
              1, 0
  )
  return(z)
}
