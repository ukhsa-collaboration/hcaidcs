context( "test date_to_fmonth")

test_that("date_to_fmonth returns expected values", {
  expect_equal(date_to_fmonth(lubridate::dmy("01-01-2018")), 10)
  expect_equal(date_to_fmonth(lubridate::dmy("01-02-2018")), 11)
  expect_equal(date_to_fmonth(lubridate::dmy("01-03-2018")), 12)
  expect_equal(date_to_fmonth(lubridate::dmy("01-04-2018")), 1)
  expect_equal(date_to_fmonth(lubridate::dmy("01-12-2018")), 9)
})

test_that("assertions function for date_to_fmonth", {
  expect_error(date_to_fmonth("Aardvark"))
})
