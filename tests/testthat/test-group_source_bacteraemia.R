context("group_source_bacteraemia")

# Test that assertions are working

test_that("Assertions are working",{
  expect_error(group_source_bacteraemia("CDI", "Urinary Tract"))
  expect_error(group_source_bacteraemia(1, "Urinary Tract"))
  expect_error(group_source_bacteraemia("MRSA", 1))
})

# Test that outputs are as expected for S. aureus

test_that("S. aureus returns expected outputs", {
  expect_equal(as.character(group_source_bacteraemia("MRSA", "Unknown")), "Unknown")
  expect_equal(as.character(group_source_bacteraemia("MRSA", "Unassessed")), "Unknown")
  expect_equal(as.character(group_source_bacteraemia("MRSA", "CVC associated" )), "Catheters & lines")
  expect_equal(as.character(group_source_bacteraemia("MRSA", "PVC associated" )), "Catheters & lines")
  expect_equal(as.character(group_source_bacteraemia("MRSA", "Tunnelled IV line")), "Catheters & lines")
  expect_equal(as.character(group_source_bacteraemia("MRSA", "Dialysis line")), "Catheters & lines")
  expect_equal(as.character(group_source_bacteraemia("MRSA", "Skin/Soft tissue infection")), "SSTI")
  expect_equal(as.character(group_source_bacteraemia("MRSA", "Endocarditis")), "Others")
  expect_equal(as.character(group_source_bacteraemia("MRSA", "Osteomyelitis")), "Others")
  expect_equal(as.character(group_source_bacteraemia("MRSA", "Other")), "Others")
  expect_equal(as.character(group_source_bacteraemia("MRSA", "Pneumonia")), "Pneumonia")
  expect_equal(as.character(group_source_bacteraemia("MRSA", "Prosthetic joint infection")), "Others")
  expect_equal(as.character(group_source_bacteraemia("MRSA", "Septic arthritis")), "Others")
  expect_equal(as.character(group_source_bacteraemia("MRSA", "SSI")), "Others")
  expect_equal(as.character(group_source_bacteraemia("MRSA", "UTI")), "Others")
  expect_equal(as.character(group_source_bacteraemia("MRSA", "")), "Not reported")
  expect_equal(as.character(group_source_bacteraemia("MRSA", "Ventilator associated pneumonia")), "Others")
  expect_equal(as.character(group_source_bacteraemia("MRSA", NA_character_)), NA_character_)
})

# test outputs as expected for Gram negatives

test_that("E. coli returns expected values", {
  expect_equal(as.character(group_source_bacteraemia("E. coli", "Bone and Joint (no prosthetic material)")), "Others")
  expect_equal(as.character(group_source_bacteraemia("E. coli", "Bone and Joint (with prosthetic material)")), "Others")
  expect_equal(as.character(group_source_bacteraemia("E. coli", "Cardiovascular or Vascular (without prosthetic material, including fistula infection)")), "Others")
  expect_equal(as.character(group_source_bacteraemia("E. coli", "Cardiovascular or Vascular (withprosthetic material e.g. EVAR, stent, valve, prsthetic fistula)")), "Others")
  expect_equal(as.character(group_source_bacteraemia("E. coli", "Central Nervous System")), "Others")
  expect_equal(as.character(group_source_bacteraemia("E. coli", "Gastrointestinal or Intraabdominal collection (excluding hepatobiliary)")), "Gastrointestinal (not hepatobiliary)")
  expect_equal(as.character(group_source_bacteraemia("E. coli", "Genital system (including prostate if male)")), "Others")
  expect_equal(as.character(group_source_bacteraemia("E. coli", "Hepatobiliary")), "Hepatobiliary")
  expect_equal(as.character(group_source_bacteraemia("E. coli", "Intravascular device (including Pacemaker/ ICD or CVC)")), "Others")
  expect_equal(as.character(group_source_bacteraemia("E. coli", "Lower Respiratory Tract (pneumonia, VAP, bronciectasis, exac COPD etc)")), "Respiratory Tract")
  expect_equal(as.character(group_source_bacteraemia("E. coli", "Lower Urinary Tract")), "UTI")
  expect_equal(as.character(group_source_bacteraemia("E. coli", "No clinical signs of infection")), "Others")
  expect_equal(as.character(group_source_bacteraemia("E. coli", "No underlying focus of infection")), "Others")
  expect_equal(as.character(group_source_bacteraemia("E. coli", "Skin or Soft Tissue (including ulcers, cellulitis, diabetic foot infections without OM)")), "Others")
  expect_equal(as.character(group_source_bacteraemia("E. coli", "Unknown")), "Unknown")
  expect_equal(as.character(group_source_bacteraemia("E. coli", "Upper Respiraotry Tract and ENT")), "Respiratory Tract")
  expect_equal(as.character(group_source_bacteraemia("E. coli", "Upper Urinary Tract (pyelonephritis/ abscess)")), "UTI")
  expect_equal(as.character(group_source_bacteraemia("E. coli", NA_character_)), "Not reported")
  expect_equal(as.character(group_source_bacteraemia("E. coli", "")), "Not reported")
})
