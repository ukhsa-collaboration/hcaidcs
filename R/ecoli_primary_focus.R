#' Classify the source of bactaeramia for E. coli
#'
#' Groups the most likely primary focus of E. coli bacteraemia according to AEC table.
#' @param x Character variable giving the most likely primary focus of bacteraemia
#' @return An ordered factor giving the grouped primary focus
#' @examples
#' ecoli_primary_focus("Urinary tract")
#' ecoli_primary_focus("Central nervous system")
#' ecoli_primary_focus("")
#' @export

ecoli_primary_focus <- function(x){
  # gi (not hepatobiliary), hepatobiliary, uti, other, unknown
  other <- c("Bone and joint", "Central nervous system", "Genital tract (inc. prostate)",
             "Indwelling intravascular device", "Other", "Respiratory tract",
             "Skin/soft tissue", "No clinical signs of bacteraemia")
  z <- ifelse(x %in% other, "Other",
                     ifelse(x == "Gastrointestinal (not hepatobiliary)", "Gastrointestinal (not hepatobiliary)",
                            ifelse(x == "Hepatobiliary", "Hepatobiliary",
                                   ifelse(x == "Unknown", "Unknown",
                                          ifelse(x == "Urinary tract", "UTI", NA)
                                          )
                                   )
                            )
                     )

  z <- factor(z, levels = c("Gastrointestinal (not hepatobiliary)", "Hepatobiliary", "UTI",
                            "Other", "Unknown"),
              ordered = TRUE)
  return(z)
}
