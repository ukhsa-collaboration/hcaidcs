context("Converting numeric months to string months")

test_that("cmonth_to_string returns expected results", {
  expect_match(cmonth_to_string(1), "January")
  expect_match(cmonth_to_string(12), "December")
  expect_equal(cmonth_to_string(13), as.character(NA))
})

test_that("fmonth_to_string returns expected results", {
  expect_match(fmonth_to_string(1), "April")
  expect_match(fmonth_to_string(12), "January")
  expect_equal(fmonth_to_string(13), as.character(NA))
})
