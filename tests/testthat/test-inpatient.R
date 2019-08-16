context("Test inpatient")

test_that("Assertions work", {
  expect_error(inpatient(patient_location = 1, patient_category = "Day Patient"))
  expect_error(inpatient(patient_location = "NHS Acute Trust", patient_category = 1))
})

test_that("Function produces expected output", {
  expect_equal(inpatient(patient_location = "NHS Acute Trust", patient_category = "In-patient"), 1)
  expect_equal(inpatient(patient_location = "NHS Acute Trust", patient_category = "Day patient"), 1)
  expect_equal(inpatient(patient_location = "NHS Acute Trust", patient_category = "Emergency Assessment"), 1)
  expect_equal(inpatient(patient_location = "GP", patient_category = "Emergency assessment"), 0)
  expect_equal(inpatient(patient_location = "NHS Acute Trust", patient_category = "Outpatient"), 0)
})
