context("days in a financial month")

test_that("days_in_fmonth gives 29 days for February in leap year", {
  expect_equal(days_in_fmonth(11, 2012), 28) # FY 2012/13 has Feb 2013, not Feb 2012
  expect_equal(days_in_fmonth(11, 2011), 29) # this is a leap year (has Feb 2012 in it)
})

test_that("days_in_fmonth gives 28 days for February in a normal year", {
  expect_equal(days_in_fmonth(11, 2013), 28)
})

test_that("days_in_fmonth gives 31 days for January", {
  expect_equal(days_in_fmonth(10, 2012), 31)
})
