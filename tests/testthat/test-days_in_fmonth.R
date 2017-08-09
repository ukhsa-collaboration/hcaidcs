context("days in a financial month")

test_that("days_in_fmonth gives 29 days for February in leap year", {
  expect_equal(days_in_fmonth(11, 2012), 29)
})

test_that("days_in_fmonth gives 28 days for February in a normal year", {
  expect_equal(days_in_fmonth(11, 2013), 28)
})

test_that("days_in_fmonth gives 31 days for January", {
  expect_equal(days_in_fmonth(10, 2012), 31)
})
