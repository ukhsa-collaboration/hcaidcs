# Test mandatory age groups

test_that("Mandatory age group function works for new collections", {
  expect_equal(mandatory_age_group(0, collection = "KLEB"),
               factor("<1", levels = c("<1", "1-14", "15-44", "45-64", "65-74", "75-84", "ge85")))
})

test_that("Mandatory age group function returns NA for out-of-range ages",{
  expect_equal(mandatory_age_group(0, collection = "cdi"), factor(NA))
  expect_equal(mandatory_age_group(1, collection = "cdi"), factor(NA))
  expect_equal(mandatory_age_group(2, collection = "cdi"),
               factor("2-14", levels = c("2-14")))
})
