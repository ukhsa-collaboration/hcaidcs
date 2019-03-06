#' @title Group specialties from the HCAI DCS
#'
#' @description Group specialties from the HCAI DCS
#'
#' @param df Character. Specialty name
#'
#' @examples
#'
#' dcs_speciality_grouping("Clinical oncology (radiotherapy)")
#'
#' dcs_speciality_grouping("PIC Gastroenterology")
#'
#' dcs_speciality_grouping("PIC Nephrology")
#'
#' dcs_speciality_grouping("")
#'
#' dcs_speciality_grouping("  ")
#'
#' dcs_speciality_grouping(NA)
#'
#' dcs_speciality_grouping("xds")
#'
#'
#' @export

dcs_speciality_grouping <- function(spec_nm){
    ward <- tolower(stringr::str_trim(spec_nm))
    ward <- ifelse(is.na(ward) | ward=="", "not reported", ward)
    ward <- ifelse(stringr::str_detect(tolower(ward), "^clinical oncology (prev. radiotherapy)|^clinical oncology (radiotherapy)|^medical oncology|^gynaecological oncology|^pic medical oncology") , "oncology", ward)
    ward <- ifelse(tolower(ward) %in% c("haematology","clinical haematology") , "haematology", ward)
    ward <- ifelse(tolower(ward) %in% c("neonatology") | stringr::str_detect(tolower(ward),"paediatric") , "paediatrics", ward)
    ward <- ifelse(stringr::str_detect(tolower(ward), "pic nephrology") , "nephrology", ward)
    ward <- ifelse(stringr::str_detect(tolower(ward), "pic gastroenterology") , "gastroenterology", ward)
    ward <- ifelse(stringr::str_detect(tolower(ward), "pic trauma and orthopaedics") , "trauma & orthopaedics", ward)
    ward <- ifelse(tolower(ward) %in% c("not applicable","unknown","not reported") , "not available", ward)
    ward <- ifelse( !tolower(ward) %in% c("not reported","oncology","haematology","paediatrics","nephrology",
                                         "gastroenterology", "trauma & orthopaedics", "not available", "general medicine",
                                         "urology", "general surgery", "geriatric medicine") , "others", ward)

  ward
}

