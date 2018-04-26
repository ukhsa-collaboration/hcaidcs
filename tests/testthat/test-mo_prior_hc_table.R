# test-mo_cdi_phc_table

data(cdi_prior_hc_data)

test_that("mo_prior_hc_table returns data frame", {
  expect_match(class(mo_prior_hc_table(cdi_prior_hc_data, collection = "C. difficile"))[3], "data\\.frame")
})

test_that("First month forms first column of output", {

  data(cdi_prior_hc_data)
  expect_match(names(mo_prior_hc_table(cdi_prior_hc_data, collection = "C. difficile"))[2],
               "March_2017_total_cases")
})
