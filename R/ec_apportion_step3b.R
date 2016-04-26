#' For e coli apportioning
 
ec_apportion_step3b <- function(patient_location, patient_category){
  trust_locs <- c("NHS Acute Trust", "")
  trust_cats <- c("In-patient", "Day patient", "Emergency Assessment", "Unknown", "")
  z <- ifelse((patient_location %in% trust_locs | patient_location == "Unknown") & 
                patient_category %in% trust_cats, "Trust, no date of admission",
              "Non-Trust"
  )
  return(z)
}