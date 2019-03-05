#' Convert .csv file to global variable that can be used in merge_everything function.
#'
#' Creates ccg_list object from ".csv" file. Csv files must contain only CCG codes to be merged.
#' This file needs to be formatted using following specification:
#'\itemize{
#' \item first row contains only column names, such as "old codes", "new code" etc.
#' \item "odd" columns (1, 3, 5...) contains old codes in a separate cells
#' \item "even" columns (2, 4, 6...) contains new code in 1 cell only
#' \item "odd" column 1 corresponds to "even" column 2, 3 to 4, 5 to 6 ...
#' \item there should be no empty spaces between the cells with data
#' \item there should be only data necessary for conversion, for example to convert 2 merge events with 3 and 4 codes
#' you will need to use csv with data in 4 columns, where first row is description only (no codes). First column
#' would have 4 rows (description + 3 codes), second column 2 rows (description + new code), third column 5 rows (description
#' + 4 codes) and final fourth column 2 rows(description + new code)
#'}
#' For example how properly format file please see  "test_convertcsv2rCCG.csv" file in the main directory
#' of HCAIDCS package.
#'
#' @param ccg_file A file with CCG codes.
#' @return R object with the structure: \cr
#' (list(list(old codes 1), new code 1), list((old codes 2), new code 2), ...) \cr
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
#' Creates trust_list object from ".csv" file. Csv files must contain only Trust codes to be merged.
#' This file needs to be formatted using following specification:
#'\itemize{
#' \item first row contains only column names, such as "old codes", "new code" etc.
#' \item "odd" columns (1, 3, 5...) contains old codes in a separate cells
#' \item "even" columns (2, 4, 6...) contains new code in 1 cell only
#' \item "odd" column 1 corresponds to "even" column 2, 3 to 4, 5 to 6 ...
#' \item there should be no empty spaces between the cells with data
#' \item there should be only data necessary for conversion, for example to convert 2 merge events with 3 and 4 codes
#' you will need to use csv with data in 4 columns, where first row is description only (no codes). First column
#' would have 4 rows (description + 3 codes), second column 2 rows (description + new code), third column 5 rows (description
#' + 4 codes) and final fourth column 2 rows(description + new code)
#'}
#' For example how properly format file please see  "test_convertcsv2rTrust.csv" file in the main directory of HCAIDCS package.
#'
#' @param trust_file A file with Trust codes.
#' @return R object with the structure: \cr
#' (list(list(old codes 1), new code 1), list((old codes 2), new code 2), ...) \cr
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
#' Creates trust_list and ccg_list objects from ".csv" files.
#' Csv files must contain CCG and Trust codes (separately).
#' These files need to be formatted using following specification:
#'\itemize{
#' \item first row contains only column names, such as "old codes", "new code" etc.
#' \item "odd" columns (1, 3, 5...) contains old codes in a separate cells
#' \item "even" columns (2, 4, 6...) contains new code in 1 cell only
#' \item "odd" column 1 corresponds to "even" column 2, 3 to 4, 5 to 6 ...
#' \item there should be no empty spaces between the cells with data
#' \item there should be only data necessary for conversion, for example to convert 2 merge events with 3 and 4 codes
#' you will need to use csv with data in 4 columns, where first row is description only (no codes). First column
#' would have 4 rows (description + 3 codes), second column 2 rows (description + new code), third column 5 rows (description
#' + 4 codes) and final fourth column 2 rows(description + new code)
#'}
#' Examples how properly format files can be found in the main directory of HCAIDCS package.
#'
#' @param ccg_file A file with CCG codes.
#' @param trust_file A file with Trust codes.
#' @return R objects with the structure: \cr
#' (list(list(old codes 1), new code 1), list((old codes 2), new code 2), ...) \cr
#' @export

convert_csv2R <- function(trust_file, ccg_file){
  convert_csv2RTrust(trust_file)
  convert_csv2RCCG(ccg_file)

}
