context("Check that date_to_fq works")

test_that("Function returns preceeding cal year for first three months",{
  x <- as.Date("01/01/2017", format = "%d/%m/%Y")
  y <- as.Date("01/02/2017", format = "%d/%m/%Y")
  z <- as.Date("01/03/2017", format = "%d/%m/%Y")
  expect_equal(date_to_fq(x), 20164)
  expect_equal(date_to_fq(y), 20164)
  expect_equal(date_to_fq(z), 20164)
})

test_that("Function returns correct quarter", {
  x <- as.Date("01/01/2017", format = "%d/%m/%Y")
  y <- as.Date("01/04/2017", format = "%d/%m/%Y")
  z <- as.Date("01/07/2017", format = "%d/%m/%Y")
  a <- as.Date("01/10/2017", format = "%d/%m/%Y")
  expect_equal(date_to_fq(x), 20164)
  expect_equal(date_to_fq(y), 20171)
  expect_equal(date_to_fq(z), 20172)
  expect_equal(date_to_fq(a), 20173)
})
