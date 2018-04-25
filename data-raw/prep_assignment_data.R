assignment_data <- structure(list(pircasestatus = c("Final assignment", "Final assignment",
 "Final assignment", "Final assignment", "Provisional assignment",
 "Provisional assignment", "Provisional assignment", "Provisional assignment",
 "Provisional assignment", "Provisional assignment", "Provisional assignment",
 "Provisional assignment", "Provisional assignment", "Provisional assignment"),
 assignmentmethodcode = c(9L, 10L, 10L, 8L, 10L, 10L, 13L,15L, 15L, 11L, 11L, 11L, NA, NA),
 patientlocation = c("NHS Acute Trust", "NHS Acute Trust",
 "Independent Sector Provider", "Independent Sector Provider",
     "NHS Acute Trust", "Independent Sector Provider", "NHS Acute Trust",
     "NHS Acute Trust", "NHS Acute Trust", "NHS Acute Trust", "NHS Acute Trust",
     "NHS Acute Trust", "NHS Acute Trust", "NHS Acute Trust"),
 provisionalorganisationname = c("Cambridge University Hospitals NHS Foundation Trust",
                                                                "Cambridge University Hospitals NHS Foundation Trust", "Cambridge University Hospitals NHS Foundation Trust",
                                                                "Cambridge University Hospitals NHS Foundation Trust", "Cambridge University Hospitals NHS Foundation Trust",
                                                                "NHS Central London CCG", "Cambridge University Hospitals NHS Foundation Trust",
                                                                "Cambridge University Hospitals NHS Foundation Trust", "NHS Central London CCG",
                                                                "Cambridge University Hospitals NHS Foundation Trust", "NHS Central London CCG", "NHS Central London CCG",
                                                                "NHS Central London CCG", "NHS Central London CCG"
                             ), finalpirassignedorganisation = c("NHS Trust", "NHS Trust",
                                                                 "Clinical Commissioning Group", "Clinical Commissioning Group",
                                                                 "NHS Trust", "NHS Trust", "NHS Trust", "NHS Trust", "NHS Trust",
                                                                 "NHS Trust", "NHS Trust", "NHS Trust", "NHS Trust", "NHS Trust"),
                              patient_category = c("In-patient", "In-patient", "In-patient", "In-patient", "In-patient", "In-patient",
                              "In-patient", "In-patient", "In-patient", "In-patient", "A&E only", "In-patient", "In-patient", "In-patient"
                              )), .Names = c("pircasestatus", "assignmentmethodcode", "patientlocation",
                                                                                "provisionalorganisationname", "finalpirassignedorganisation",
                                                                                "patient_category"), row.names = c(NA, -14L), class = "data.frame")

assignment_data2 <- structure(list(pir_case_status = c("Provisional Assignment",
 "Provisional Assignment", "Provisional Assignment", "Provisional Assignment",
 "Provisional Assignment", "Provisional Assignment", "Provisional Assignment",
 "Provisional Assignment", "Provisional Assignment", "Provisional Assignment",
 "Provisional Assignment", "Provisional Assignment", "Provisional Assignment",
 "Provisional Assignment", "Provisional Assignment"), assignment_method_code = c(NA_real_,
                                                                                 NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
                                                                                 NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_
 ), patient_location = c("NHS Acute Trust", "NHS Acute Trust",
                         "NHS Acute Trust", "NHS Acute Trust", "NHS Acute Trust", "NHS Acute Trust",
                         "NHS Acute Trust", "NHS Acute Trust", "NHS Acute Trust", "NHS Acute Trust",
                         "NHS Acute Trust", "NHS Acute Trust", "NHS Acute Trust", "NHS Acute Trust",
                         "NHS Acute Trust"), patient_category = c("In-patient", "In-patient",
                                                                  "In-patient", "Emergency Assessment", "In-patient", "Emergency Assessment",
                                                                  "In-patient", "In-patient", "In-patient", "In-patient", "Emergency Assessment",
                                                                  "In-patient", "In-patient", "Emergency Assessment", "Day patient"
                         ), provisional_organisation_name = c("NHS TOWER HAMLETS CCG",
                                                              "NHS CHILTERN CCG", "NHS GLOUCESTERSHIRE CCG", "NHS BRISTOL CCG",
                                                              "NHS DARTFORD, GRAVESHAM AND SWANLEY CCG", "NHS CRAWLEY CCG",
                                                              "NHS BRACKNELL AND ASCOT CCG", "NHS SOMERSET CCG", "NHS SOUTH GLOUCESTERSHIRE CCG",
                                                              "NHS HIGH WEALD LEWES HAVENS CCG", "NHS BRISTOL CCG", "NHS TOWER HAMLETS CCG",
                                                              "NHS NORTHUMBERLAND CCG", "NHS BRISTOL CCG", "NHS WEST ESSEX CCG"
                         ), final_pir_assigned_organisation_type = c("", "", "", "", "",
                                                                     "", "", "", "", "", "", "", "", "", ""), id = 1:15), .Names = c("pir_case_status",
                                                                                                                                     "assignment_method_code", "patient_location", "patient_category",
                                                                                                                                     "provisional_organisation_name", "final_pir_assigned_organisation_type",
                                                                                                                                     "id"), row.names = c(NA, -15L), class = "data.frame")

devtools::use_data(assignment_data)
devtools::use_data(assignment_data2)
