#' Convert financial year quarters to calendar year quarters
#'
#' Converts from five-character financial year quarter to five character calendar year quarter
#' @param x A financial quarter, e.g. 20154 is January-March 2016 (cyear 20161)
#' @return A numeric value giving calendar quarter in format `yyyyq`
#' @examples
#' fq_to_cq(20154)
#' fq_to_cq(20151)
#' @export

fq_to_cq <- function(x){
  year <- as.numeric(substr(as.character(x), 1, 4))
  q <- as.numeric(substr(as.character(x), 5, 5))
  cq <- ifelse(q == 4,
               paste0(year + 1, 1),
               paste0(year, q + 1))
  return(as.numeric(cq))
}
