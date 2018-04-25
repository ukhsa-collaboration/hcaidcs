# test-mo_cdi_phc_table

data(cdi_prior_hc_data)

test_that("mo_prior_hc_table returns data frame with expected columns", {
  expect_match(class(mo_prior_hc_table(cdi_prior_hc_data, collection = "C. difficile"))[3], "data\\.frame")
  expect_match(names(mo_prior_hc_table(cdi_prior_hc_data, collection = "C. difficile"))[1], "org_code")
  expect_match(names(mo_prior_hc_table(cdi_prior_hc_data, collection = "C. difficile"))[2], "year_no")
  expect_match(names(mo_prior_hc_table(cdi_prior_hc_data, collection = "C. difficile"))[3], "month_no")
  expect_match(names(mo_prior_hc_table(cdi_prior_hc_data, collection = "C. difficile"))[4], "month_string")
  expect_match(names(mo_prior_hc_table(cdi_prior_hc_data, collection = "C. difficile"))[5], "Measure")
  expect_match(names(mo_prior_hc_table(cdi_prior_hc_data, collection = "C. difficile"))[6], "Count of cases")
})
