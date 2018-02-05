#' group_source_bacteraemia
#'
#' Group the sources of bacteraemia for reporting in QEC or elsewhere.
#' In 2017 the column which gave the primary focus for Gram-negative collections
#' changed with the introducion  new risk factor collection on the DCS.
#'
#' Previously, the field name was risk_factors_most_likely_primary_focus
#' the new field name is source_primary_focus_of_bacteraemia
#'
#'  The general approach when analysing the data is to combine the information
#' from both fields into one.
#' This may cause problems where there is contradictory information.
#' New information is copied into old, but only when new information is not
#' unknown or otherwise missing.
#'
#' @param collection A string giving the data collection, one of
#' "MRSA", "MSSA", "E. coli", "Klebsiella spp", "Pseudomonas aeruginosa"
#' @param source_col The string giving the source of bacteraemia to be grouped
#'
#' @return A vector of grouped sources
#' @examples
#' group_source_bacteraemia(collection = "E. coli", source_col = "Urinary tract")
#' \dontrun{
#' dat <- data.frame(stringsAsFactors=FALSE,
#'   org = c("E. coli", "MRSA"),
#'   source = c("Unassessed", "Skin/Soft tissue infection")
#' )
#' dat$grouped_source <- group_source_bacteraemia(collection = dat$org, source_col = dat$source)
#' }
#' @export


group_source_bacteraemia <- function(collection, source_col){
  assertthat::assert_that(all(collection != "CDI"),
                          msg = "CDI does not have a source and is not a bacteraemia")
  assertthat::assert_that(is.character(collection),
                          msg = "collection must be a string")
  assertthat::assert_that(is.character(source_col),
                          msg = "source_col must be a string")
  # z <- ifelse(collection %in% c("MRSA", "MSSA"), sa_source(source_col),
  #             ifelse(stringr::str_trim(collection, "both") %in%
  #                      c("E. coli", "Klebsiella spp", "Pseudomonas aeruginosa"),
  #                    gn_source(source_col), NA))
  collection <- stringr::str_trim(collection, side = "both")
  vectored_source <- Vectorize(function(collection, source_col){
    switch(collection,
              MRSA = sa_source(source_col),
              MSSA = sa_source(source_col),
              "E. coli" = gn_source(source_col),
              "Klebsiella spp" = gn_source(source_col),
              "Pseudomonas aeruginosa" = gn_source(source_col)
              )
  })
  z <- vectored_source(collection, source_col)

  return(z)
}

sa_source <- function(source_col){
  z <- dplyr::case_when(
    source_col %in% c("Unknown", "Unassessed") ~ "Unknown",
    source_col %in% c("CVC associated", "PVC associated", "Tunnelled IV line",
                      "Dialysis line") ~ "Catheters & lines",
    source_col == "Skin/Soft tissue infection" ~ "SSTI",
    source_col == "Pneumonia" ~ "Pneumonia",
    is.na(source_col) ~ NA_character_,
    TRUE ~ "Others"
  )
  z <- factor(z, levels = c("Catheters & lines", "SSTI", "Pneumonia", "Others",
                            "Unknown"))
  return(z)
}

gn_source <- function(source_col){
  z <- dplyr::case_when(
    source_col %in% c("Gastrointestinal (not hepatobiliary)",
                      "Gastrointestinal or Intraabdominal collection (excluding hepatobiliary)") ~ "Gastrointestinal (not hepatobiliary)",
    source_col == "Hepatobiliary" ~ "Hepatobiliary",
    source_col %in% c("Urinary tract", "Lower Urinary Tract",
                      "Upper Urinary Tract (pyelonephritis/ abscess)") ~ "UTI",
    source_col %in% c("Respiratory tract","Lower Respiratory Tract (pneumonia, VAP, bronciectasis, exac COPD etc)",
                      "Upper Respiraotry Tract and ENT") ~ "Respiratory Tract",
    source_col %in% c("Unknown","Unassessed") ~ "Unknown",
    source_col == "" ~ "Not reported",
    TRUE ~ "Others"
  )
  z <- factor(z, levels = c("Gastrointestinal (not hepatobiliary)", "Hepatobiliary", "UTI", "Respiratory Tract",
                            "Others", "Not reported", "Unknown"))
  return(z)
}


# ecoli_dcs <- readr::read_delim("K:/Quarterly analyses/2018/01_Mar_8_2018/extracts/ecoli_dcs.txt",
#                         "|", escape_double = FALSE, trim_ws = TRUE)
#
# levels(as.factor(ecoli_dcs$`Risk Factors-Most likely primary focus`))
# levels(as.factor(ecoli_dcs$`Source-Primary focus of bacteraemia`))
#
# writeClipboard(levels(as.factor(ecoli_dcs$`Risk Factors-Most likely primary focus`)))
# writeClipboard(levels(as.factor(ecoli_dcs$`Source-Primary focus of bacteraemia`)))
#
# names(ecoli_dcs) <- nicethings::nice_names(ecoli_dcs)
#
# ecoli_dcs %>% group_by(year_no) %>%
#   count(is.na(risk_factors_most_likely_primary_focus)) %>% View()
#
# ecoli_dcs %>% group_by(year_no) %>%
#   count(is.na(source_primary_focus_of_bacteraemia)) %>% View()
#
# # source_primary_focus started 2017
#
## Old
# Bone and joint
# Central nervous system
# Gastrointestinal (not hepatobiliary)
# Genital tract (inc. prostate)
# Hepatobiliary
# Indwelling intravascular device
# No clinical signs of bacteraemia
# Other
# Respiratory tract
# Skin/soft tissue
# Urinary tract
# Yes
#
## New
# Bone and Joint (no prosthetic material)
# Bone and Joint (with prosthetic material)
# Cardiovascular or Vascular (without prosthetic material, including fistula infection)
# Cardiovascular or Vascular (withprosthetic material e.g. EVAR, stent, valve, prsthetic fistula)
# Central Nervous System
# Gastrointestinal or Intraabdominal collection (excluding hepatobiliary)
# Genital system (including prostate if male)
# Hepatobiliary
# Intravascular device (including Pacemaker/ ICD or CVC)
# Lower Respiratory Tract (pneumonia, VAP, bronciectasis, exac COPD etc)
# Lower Urinary Tract
# No clinical signs of infection
# No underlying focus of infection
# Skin or Soft Tissue (including ulcers, cellulitis, diabetic foot infections without OM)
# Unknown
# Upper Respiraotry Tract and ENT
# Upper Urinary Tract (pyelonephritis/ abscess)
