#' Classify the source of bacteramia for MRSA
#'
#' Groups the source of bacteraemia according to AEC table.
#' @param x Character variable giving the source of bacteraemia
#' @return An ordered factor giving the grouped source
#' @examples
#' mrsa_source_bacteraemia("CVC associated")
#' mrsa_source_bacteraemia("Endocarditis")
#' mrsa_source_bacteraemia("")
#' @export

mrsa_source_bacteraemia <- function(x){
  .Deprecated("group_source_bacteraemia")
  lines <- c("CVC associated", "Dialysis line", "PVC associated", "Tunnelled IV line")
  other <- c("Endocarditis", "Osteomyelitis", "Other",
             "Prosthetic joint infection", "SSI", "Septic arthritis",
             "UTI", "Ventilator associated pneumonia")
  z <- ifelse(x %in% lines, "Catheters and lines",
              ifelse(x %in% other, "Other",
                     ifelse(x == "Skin/Soft tissue infection", "SSTI",
                            ifelse(x == "Other", "Other",
                                   ifelse(x == "Unknown", "Unknown",
                                          ifelse(x == "Pneumonia", "Pneumonia", NA))))))
  z <- factor(z, levels = c("Catheters and lines", "SSTI", "Pneumonia",
                            "Other", "Unknown"),
              ordered = TRUE)
  return(z)
}
