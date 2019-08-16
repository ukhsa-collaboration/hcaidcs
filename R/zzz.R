.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste0(
      "\n\r\n\rThanks for using the HCAIDCS package.",
      "\n\r\n\rIf you have any suggestions to improve the HCAIDCS package, please file an issue at https://github.com/PublicHealthEngland/hcaidcs/issues")
  )
}
