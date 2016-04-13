#' Display manual for this package in browser
#'
#' This function is Windows only.
#' A convenience function to create the pdf version of the help pages in the
#' current working directory.
#' Requires pdflatex to be installed on your machine.
#' The function takes no arguments and may run a little slowly.
#' @export

show_hcaidcs_package_manual <- function(){
  # Inspiration from http://r.789695.n4.nabble.com/Opening-package-manual-from-within-R-td3763938.html#a3764572
  # and openPDF from https://github.com/Bioconductor-mirror/Biobase/blob/a1794a80262268138cbd9a45cc320e644aabe2fc/R/tools.R
  if(file.exists(paste0(getwd(), "/", "hcaidcs.pdf"))){
    shell.exec(paste0(getwd(), "/", "hcaidcs.pdf"))
  }else{
    path <- find.package("hcaidcs", lib.loc = NULL)
    system(paste(shQuote(file.path(R.home("bin"), "R")),
                 "CMD", "Rd2pdf",
                 shQuote(path)))
  }
}
