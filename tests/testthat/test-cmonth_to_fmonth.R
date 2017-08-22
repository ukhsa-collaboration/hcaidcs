context("Converting calendar months to financial year months")

test_that("Function returns expected results", {
  expect_equal(cmonth_to_fmonth(1), 12)
  expect_equal(cmonth_to_fmonth(2), 11)
  expect_equal(cmonth_to_fmonth(4), 1)
  expect_equal(cmonth_to_fmonth(12), 9)
})
