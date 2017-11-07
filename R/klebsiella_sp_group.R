#' Create group for Klebsiella species
#'
#' For reporting purposes, Klebsiella species are regrouped.
#' This function performs the regrouping.
#' Unrecognised species entries are given the value "This is not a known value".
#' Blank values are reclassified to 'Not speciated'
#'
#' @param x A string giving the species name
#' @return A string giving the reporting group
#' @examples
#' test <- ""
#' klebsiella_sp_group(test)

klebsiella_sp_group <- function(x){
  z <- ifelse(x == "" | x == "Klebsiella sp.", "Not speciated",
              ifelse(x == "K. pneumoniae (incl. subspecies pnemoniae and ozenae)", "K. pneumoniae",
                     ifelse(x == "K. oxytoca", "K. oxytoca",
                            ifelse(x == "Other Named" | x == "K. aerogenes", "Other named species", "This is not a known value")
                            )))
  return(z)
}
