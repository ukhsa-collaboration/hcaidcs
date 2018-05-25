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

test_that("Cases with 13 or 14 and final assignment are Third Party", {
  expect_equal(
    assignment_algorithm(pircasestatus = "Final Assignment",
                         assignmentmethodcode = 13,
                         patientlocation = "NHS Acute Trust",
                         patientcategory = "In-patient",
                         provisionalorganisationname = "Some trust",
                         finalpirassignedorganisationtype = NA) ,
    "Third Party")

  expect_equal(
    assignment_algorithm(pircasestatus = "Final Assignment",
                         assignmentmethodcode = 14,
                         patientlocation = "NHS Acute Trust",
                         patientcategory = "In-patient",
                         provisionalorganisationname = "Some trust",
                         finalpirassignedorganisationtype = NA) ,
    "Third Party")

  expect_equal(
    assignment_algorithm(pircasestatus = "Final Assignment",
                         assignmentmethodcode = 14,
                         patientlocation = "NHS Acute Trust",
                         patientcategory = "In-patient",
                         provisionalorganisationname = "Some CCG",
                         finalpirassignedorganisationtype = NA) ,
    "Third Party")
})

test_that("Cases with non-numeric assignmentmethodcode do work", {
  expect_equal(
    assignment_algorithm(pircasestatus = "final assignment",
                         assignmentmethodcode = "9",
                         patientlocation = "NHS Acute Trust",
                         patientcategory = "In-patient",
                         provisionalorganisationname = "some trust",
                         finalpirassignedorganisationtype = "NHS Trust"),
    "NHS Trust"
  )
  expect_equal(
    assignment_algorithm(pircasestatus = "final assignment",
                         assignmentmethodcode = "09",
                         patientlocation = "NHS Acute Trust",
                         patientcategory = "In-patient",
                         provisionalorganisationname = "some trust",
                         finalpirassignedorganisationtype = "NHS Trust"),
    "NHS Trust"
  )
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

test_that("On Hold cases are assigned to provisional organisations", {
  expect_equal(
    assignment_algorithm(pircasestatus = "On-Hold",
                         assignmentmethodcode = NA_real_,
                         patientlocation = "NHS Acute Trust",
                         patientcategory = "In-patient",
                         provisionalorganisationname = "NHS SOUTH GLOUCESTERSHIRE CCG",
                         finalpirassignedorganisationtype = NA_character_),
    "Clinical Commissioning Group")
  expect_equal(
    assignment_algorithm(pircasestatus = "On-Hold",
                         assignmentmethodcode = NA_real_,
                         patientlocation = "NHS Acute Trust",
                         patientcategory = "In-patient",
                         provisionalorganisationname = "Some Random Trust",
                         finalpirassignedorganisationtype = NA_character_),
    "NHS Trust" )
})
