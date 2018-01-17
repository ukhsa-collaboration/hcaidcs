# update organisation details on the DCS

# A collection of functions that will take data from ODS and write zip files that can be used to update the DCS.


#' Update independent sector providers and independent sector sites.
#'
#' This function takes two csv files: ephp.csv and ephpsite.csv limits data to the specified organisation(s) then exports to zip files.
#' The returned .csv files must have characters for all variables otherwise upload to the DCS will fail.
#' The returned zip files can be used to upload to the HCAI DCS to update organisation mappings.
#'
#' @param organisation A character giving the organisation to be updated.
#' @param input_file_path A character string giving the path to the ods csv files, but without specifying the csv file itself.
#' @param output_file_path A character string giving the path where outputs will be saved.
#' @export
#' @return Four files, two csv and two zip for each ephp and ephpsite files.
#' @examples
#' \dontrun{
#'  update_independent("AXG")
#' }

update_independent <- function(
  organisation,
  input_file_path = "K:/Mandatory databases/maintaining_new_org_structure_new/ods_files/",
  output_file_path = "K:/Mandatory databases/maintaining_new_org_structure_new/ods_files/updates/"){
  # will require stringr
  assertthat::assert_that(
    substr(input_file_path, start = nchar(input_file_path),
           stop = nchar(input_file_path)) == "/",
    msg ="input_file_path must end in a forward slash")
  assertthat::assert_that(
    substr(output_file_path, start = nchar(output_file_path),
           stop = nchar(output_file_path)) == "/",
    msg ="output_file_path must end in a forward slash")
  assertthat::assert_that(file.exists(paste0(input_file_path, "ephp.csv")) == TRUE,
                          msg = paste0("ephp.csv does not exist in ", input_file_path))
  assertthat::assert_that(file.exists(paste0(input_file_path, "ephpsite.csv")) == TRUE,
                          msg = paste0("ephpsite.csv does not exist in ", input_file_path))
  assertthat::assert_that(is.character(organisation) == TRUE,
                          msg = "organisation must be a character string")
  this_wd <- getwd()

  # select just the org(s) that we want
  ephp <- read.csv(file = paste0(input_file_path, "ephp.csv"), header = FALSE)
  ephp <- ephp[ephp$V1 == organisation, ]
  ephp$V22 <- "1"
  ephpsite <- read.csv(file = paste0(input_file_path, "ephpsite.csv"), header = FALSE)
  ephpsite <- ephpsite[grepl(organisation, ephpsite$V1) == TRUE, ]
  ephpsite$V22 <- "1"

  # ensure that every var is a character
  ephp <- data.frame(lapply(ephp, as.character))
  ephpsite <- data.frame(lapply(ephpsite, as.character))

  # write csvs out
  if(dir.exists(output_file_path)){
    write.table(ephp, paste0(output_file_path, "ephp.csv"),
                row.names = FALSE, col.names = FALSE, sep = ",", na = "\"\"")
    write.table(ephpsite, paste0(output_file_path, "ephpsite.csv"),
                row.names = FALSE, col.names = FALSE, sep = ",", na = "\"\"")
  }else{
    dir.create(output_file_path)
    write.table(ephp, paste0(output_file_path, "ephp.csv"),
                row.names = FALSE, col.names = FALSE, sep = ",", na = "\"\"")
    write.table(ephpsite, paste0(output_file_path, "ephpsite.csv"),
                row.names = FALSE, col.names = FALSE, sep = ",", na = "\"\"")
  }
  setwd(output_file_path)
  zip(zipfile = "ephp.zip",
      files = "ephp.csv")
  zip(zipfile = "ephpsite.zip",
      "ephpsite.csv")
  setwd(this_wd)
}
