#' For e coli apportioning

ec_apportion_step3a <- function(specimen_date, date_admitted){
  diff <- specimen_date - date_admitted + 1
  z <- ifelse(diff < 2, "Non-Trust", 
              ifelse(diff > 2 & diff <= 7, "Trust, 48 hrs to 7 days", "Trust, >7 days"))
  return(z)
}