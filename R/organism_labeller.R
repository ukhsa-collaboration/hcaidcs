#' Create italicised organism names for plotting in facetted graphs
#'
#' Creating mixed normal font and italic font labels is tricky.
#' ggplot2 includes the \code{as_labeller} function and this function uses that to create labels for the organisms currently covered in the surveillance.
#' This function takes no arguments.
#'
#' @return A labeller object for use in a ggplot graph
#' @export
#' @examples
#'   \dontrun{
#'   # These are only set as don't run so that the package builds more rapidly.
#' dat <- data.frame( variable = c("MRSA", "MSSA", "E. coli", "C. difficile",
#'     "Klebsiella spp", "Pseudomonas aeruginosa"),
#'     value = c(1, 2, 3, 4, 5, 6),
#'   stringsAsFactors = FALSE)
#'   ggplot2::ggplot(data = dat, ggplot2::aes(x = 1, y = value)) +
#'     ggplot2::facet_wrap( ~ variable, labeller = organism_labeller)
#'   }

organism_labeller <- ggplot2::as_labeller(c(
    `MRSA` = "MRSA", `MSSA` = "MSSA",
    `C. difficile` = "italic(C.~difficile)",
    `E. coli` = "italic(E.~coli)",
    `Klebsiella spp` = paste("italic(Klebsiella)", "~spp."),
    `Pseudomonas aeruginosa` = "italic(Pseudomonas~aeruginosa)"
  ), ggplot2::label_parsed)
