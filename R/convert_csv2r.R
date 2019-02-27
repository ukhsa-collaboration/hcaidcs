#' Convert .csv file to global variable that can be used in merge_everything function.
#'
#' Creates ccg_list objects.
#'
#' @param ccg_file A file with CCG codes.
#' @return R object(list).
#' @export

convert_csv2RCCG <- function(ccg_file) {

  r_object <- unname(as.list(utils::read.csv(ccg_file, colClasses = "character", na.strings = NA, fill = F, stringsAsFactors = F)))
  r_object <- lapply(r_object, function(x) x[nzchar(x)])
  ccg_list <- NULL
  count = length(r_object)/2
  while (count != 0) {
    ccg_list <- append(ccg_list, list(c(r_object[1], r_object[2])))
    r_object <- r_object[-c(1,2)]
    count <- count - 1
  }
  ccg_list <<- ccg_list
}

#' Convert .csv file to global variables that can be used in merge_everything function.
#'
#' Creates trust_list objects.
#'
#' @param trust_file A file with CCG codes.
#' @return R object(list).
#' @export

convert_csv2RTrust <- function(trust_file) {

  r_object <- unname(as.list(utils::read.csv(trust_file, colClasses = "character", na.strings = NA, fill = F, stringsAsFactors = F)))
  r_object <- lapply(r_object, function(x) x[nzchar(x)])
  trust_list <- NULL
  count = length(r_object)/2
  while (count != 0) {
    trust_list <- append(trust_list, list(c(r_object[1], r_object[2])))
    r_object <- r_object[-c(1,2)]
    count <- count - 1
  }
  trust_list <<- trust_list
}

#' Convert .csv files to global variables that can be used in merge_everything function.
#'
#' Create trust_list and ccg_list objects.
#'
#' @param ccg_file A file with CCG codes.
#' @param trust_file A file with CCG codes.
#' @return R objects (lists of list) to be used in merge_everything function.
#' @export

convert_csv2R <- function(trust_file, ccg_file){
  convert_csv2RTrust(trust_file)
  convert_csv2RCCG(ccg_file)

}
