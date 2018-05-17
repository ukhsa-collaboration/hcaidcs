.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste0(
      "\n\r\n\rThanks for using the HCAIDCS package.",
      "\n\r\n\rPlease note that the apportion_prior_healthcare function is still experimental and has not been validated by a second team member or against the DCS.")
  )
}
