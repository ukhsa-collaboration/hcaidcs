context("Completing the PIR assignment for provisional cases")

test_that("Cases with 13 or 14 and provisional assignment are Third Party", {
  expect_equal(
    assignment_algorithm(pircasestatus = "provisional assignment",
                         assignmentmethodcode = 13,
                         patientlocation = "In-patient",
                         patientcategory = "",
                         provisionalorganisationname = "Some trust",
                         finalpirassignedorganisationtype = NA) ,
    "Third Party")

  expect_equal(
    assignment_algorithm(pircasestatus = "provisional assignment",
                         assignmentmethodcode = 14,
                         patientlocation = "In-patient",
                         patientcategory = "",
                         provisionalorganisationname = "Some trust",
                         finalpirassignedorganisationtype = NA) ,
    "Third Party")

  expect_equal(
    assignment_algorithm(pircasestatus = "provisional assignment",
                         assignmentmethodcode = 14,
                         patientlocation = "In-patient",
                         patientcategory = "",
                         provisionalorganisationname = "Some CCG",
                         finalpirassignedorganisationtype = NA) ,
    "Third Party")
})

test_that("Cases with assignmentmethodcode < 10 give final pir status as new status", {
  expect_equal(
    assignment_algorithm(pircasestatus = "final assignment",
                         assignmentmethodcode = 9,
                         patientlocation = "NHS Acute Trust",
                         patientcategory = "In-patient",
                         provisionalorganisationname = "some trust",
                         finalpirassignedorganisationtype = "NHS Trust"),
    "NHS Trust"
  )

    expect_equal(
    assignment_algorithm(pircasestatus = "final assignment",
                         assignmentmethodcode = 9,
                         patientlocation = "NHS Acute Trust",
                         patientcategory = "In-patient",
                         provisionalorganisationname = "some trust",
                         finalpirassignedorganisationtype = "Clinical Commissioning Group"),
    "Clinical Commissioning Group"
  )
    # Added test to account for Commissioning Hubs, based on case id 613241
    expect_equal(
      assignment_algorithm(pircasestatus = "provisional assignment",
                           assignmentmethodcode = NA_real_,
                           patientlocation = "NHS Acute Trust",
                           patientcategory = "A&E only",
                           provisionalorganisationname = "NATIONAL COMMISSIONING HUB",
                           finalpirassignedorganisationtype = "Clinical Commissioning Group"),
      "Clinical Commissioning Group"
    )
})
