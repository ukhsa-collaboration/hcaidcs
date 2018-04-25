#' Set up empty folders for Annual publication
#'
#' Annual publication for mandatory surveillance should follow the same structure so that exports do not break.
#' This function creates the main folder on the K: drive and sets up empty subfolders.
#'
#' @param fyear Financial year for report in YY_YY, e.g. 17_18 for 2017/2018 report
#' @return New folder structure. Will print to console on success
#' @export
#' @examples
#' \dontrun{
#' aec_create_folder_structure("17_18")
#' }

aec_create_folder_structure <- function(fyear){
  assertthat::assert_that(nchar(fyear) == 5,
                          msg = "Please ensure that fyear is character in format YY_YY")
  root_dir <- "K:/Annual Table Publication/Annual_Publication_FY "
  if(dir.exists(paste0(root_dir, fyear))){
    print("Warning: Master folder already exists\nSubfolders will be created")
    dir.create(paste0(root_dir, fyear, "/data"))
    dir.create(paste0(root_dir, fyear, "/denominators"))
    dir.create(paste0(root_dir, fyear, "/drafts"))
    dir.create(paste0(root_dir, fyear, "/extracts"))
    dir.create(paste0(root_dir, fyear, "/figs"))
    dir.create(paste0(root_dir, fyear, "/prerelease"))
    dir.create(paste0(root_dir, fyear, "/scripts"))
    dir.create(paste0(root_dir, fyear, "/tables"))
    dir.create(paste0(root_dir, fyear, "/web_publishing"))
    print("Subfolders created")
  }else{
    dir.create(paste0(root_dir, fyear))
    print("Master folder created")
    dir.create(paste0(root_dir, fyear, "/data"))
    dir.create(paste0(root_dir, fyear, "/denominators"))
    dir.create(paste0(root_dir, fyear, "/drafts"))
    dir.create(paste0(root_dir, fyear, "/extracts"))
    dir.create(paste0(root_dir, fyear, "/figs"))
    dir.create(paste0(root_dir, fyear, "/prerelease"))
    dir.create(paste0(root_dir, fyear, "/scripts"))
    dir.create(paste0(root_dir, fyear, "/tables"))
    dir.create(paste0(root_dir, fyear, "/web_publishing"))
    print("Subfolders created")
  }
}
