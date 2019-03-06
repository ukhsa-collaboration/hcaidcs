context("Testing function for grouping specialties from the HCAI DCS")

library(stringr)

test_that("All PICs go to the 'major' specialty" , {
  expect_equal(dcs_speciality_grouping("PIC medical oncology"), "oncology")
  expect_equal(dcs_speciality_grouping("PIC Gastroenterology"), "gastroenterology")
  expect_equal(dcs_speciality_grouping("PIC Nephrology"), "nephrology")
  expect_equal(dcs_speciality_grouping("PIC trauma and orthopaedics"), "trauma & orthopaedics")
  expect_equal(dcs_speciality_grouping("PIC Medical Oncology "), "oncology")
})

test_that("Any speciality name conataining \"paediatric\" is grouped under paediatrics"  , {
  expect_equal(dcs_speciality_grouping("A paediatric"), "paediatrics")
  expect_equal(dcs_speciality_grouping("Paediatric C"), "paediatrics")

})

test_that("Any other name not blank or NA goes to other"  , {
  expect_equal(dcs_speciality_grouping("xxx"), "others")
  expect_equal(dcs_speciality_grouping("ccc"), "others")
})

test_that("Blanks or NAs are grouped under not reported"  , {
  expect_equal(dcs_speciality_grouping(NA), "not available")
  expect_equal(dcs_speciality_grouping(""), "not available")
  expect_equal(dcs_speciality_grouping("   "), "not available")

})


