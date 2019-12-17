#' Italicise species names for graphing

#' Producing mixed italics for species names in plots is tricky.
#' This function will do it for the species in the mandatory surveillance collections
#'
#' @param x a character vector giving the data collection
#' @return A factor variable with labels that will control italicisation
#'
#' @examples
#' graph_species_names("Klebsiella spp")
#' graph_species_names("MRSA")
#' graph_species_names(x = c("Klebsiella spp", "MRSA"))
#' graph_species_names <- function(x){
#'   assertthat::assert_that(is.character(x), msg = "x must be a character")
#'   x <- trimws(x)
#'   assertthat::assert_that(
#'     all(x %in% c("C. difficile", "E. coli","Klebsiella spp", "MRSA", "MSSA",
#'                   "P. aeruginosa", "Pseudomonas aeruginosa")),
#'                   msg = "Please make sure that x is one of C. difficile, E. coli,
#'                   Klebsiella spp, MRSA, MSSA, P. aeruginosa or Pseudomonas aeruginosa,
#'     S. aureus or Staphylococcus aureus"
#'     )
#'   x <- ifelse(x == "Pseudomonas aeruginosa", "P. aeruginosa", x)
#'   x <- factor(x)
#'   levels(x) = c("C. difficile" = expression(italic("C. difficile")),
#'                 "E. coli" = expression(italic("E. coli")),
#'                 "Klebsiella spp" = expression(paste(italic("Klebsiella "), " spp.")),
#'                 "MRSA" = "MRSA",
#'                 "MSSA" = "MSSA",
#'                 "P. aeruginosa" = expression(italic("P. aeruginosa")),
#'                 "S. aureus" = expression(italic("S. aureus"))
#'   )
#'   x <- droplevels(x)
#'   return(x)
#' }
#' @export

graph_species_names <- function(x){
  assertthat::assert_that(is.character(x), msg = "x must be a character")
  x <- trimws(x)
  assertthat::assert_that(
    all(x %in% c("C. difficile", "E. coli","Klebsiella spp", "MRSA", "MSSA",
                 "P. aeruginosa", "Pseudomonas aeruginosa")),
    msg = "Please make sure that x is one of C. difficile, E. coli,
                  Klebsiella spp, MRSA, MSSA, P. aeruginosa or Pseudomonas aeruginosa,
    S. aureus or Staphylococcus aureus"
  )
  x <- ifelse(x == "Pseudomonas aeruginosa", "P. aeruginosa", x)
  x <- factor(x,
              levels = c("C. difficile",
                         "E. coli",
                         "Klebsiella spp",
                         "MRSA",
                         "MSSA",
                         "P. aeruginosa",
                         "S. aureus"),
              labels = c(expression(italic("C. difficile")),
                         expression(italic("E. coli")),
                         expression(paste(italic("Klebsiella "), " spp.")),
                         "MRSA",
                         "MSSA",
                         expression(italic("P. aeruginosa")),
                         expression(italic("S. aureus"))))
  x <- droplevels(x)
  return(x)
}
