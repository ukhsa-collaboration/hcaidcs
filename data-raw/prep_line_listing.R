# Example line list data

library(wakefield)
library(hcaidcs)
library(lubridate)
library(janitor)
library(dplyr)

set.seed(1)
line_listing <- r_data_frame(n = 500,
  id,
  collection = r_sample_factor(
    x = c("E. coli", "MRSA", "Klebsiella spp.", "MSSA", "C. difficile")),
  age, sex,
  specimen_date = date_stamp(start = dmy("01/04/2016")),
  onset_status = answer(x = c("HO", "CO")),
  reporting_organisation_code = answer(x = c("W1A", "E17")),
  ccg_code = answer(x = c("00A", "00B")),
  pir_status = r_sample_factor(x = c("Trust-assigned", "CCG-assigned",
                                     "Third-party")),
  prior_hc = r_sample_factor(x = c("hoha, ha", "coha, ha", "coia", "coca",
                                     "unknown_3_mo", "all_blank")),
  patient_location = r_sample_factor(
    x = c("GP", "Independent Sector Provider", "Mental Health Provider",
          "NHS Acute Trust", "Non-acute NHS Provider", "Nursing Home",
          "Other", "Penal Establishment", "Residential Home", "Unknown")),
  patient_category = r_sample_factor(
    x = c(c("A&E only", "Day patient", "Emergency Assessment", "In-patient",
            "Other", "Outpatient", "Regular Attender", "Unknown")
))
)

names(line_listing) <- tolower(names(line_listing))
head(line_listing)

line_listing <- line_listing %>%
  mutate_if(.predicate = is.factor, funs(as.character))

line_listing$pir_status <- ifelse(line_listing$collection != "MRSA", "",
                                  line_listing$pir_status)
line_listing$prior_hc <- ifelse(line_listing$collection != "C. difficile", "",
                                  line_listing$prior_hc)

line_listing %>% tabyl(onset_status)
line_listing %>% tabyl(collection)
line_listing %>% tabyl(collection, pir_status)
line_listing %>% tabyl(collection, prior_hc)

usethis::use_data(line_listing, overwrite = TRUE)
