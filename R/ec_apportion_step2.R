#' for e coli apportioning 

ec_apportion_step2 <- function(patient_category){
  trust_cats <- c("In-patient", "Day patient", "Emergency Assessment", "Unknown", "")
  z <- ifelse(patient_category %in% trust_cats, 1, 0)
  return(z)
}