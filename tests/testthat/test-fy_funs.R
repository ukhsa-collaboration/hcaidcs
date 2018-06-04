context("Testing fy_long")

test_that("fy_long assertions work", {
  expect_error(fy_long("Aardvark"))
})

test_that("fy_long is vectorised",{
  expect_equal(fy_long(lubridate::dmy(c("01/01/2001", "01/01/2002"))),
               c("April 2000 to March 2001", "April 2001 to March 2002"))
})
