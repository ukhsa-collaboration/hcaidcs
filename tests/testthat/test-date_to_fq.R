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

context("Check that fq_short_to_date works")

test_that("Function returns a date", {
  expect_true(lubridate::is.Date(fq_short_to_date(20091)))
})

# test_that("Assertions work", {
#   expect_error(fq_short_to_date(200912))
# })

test_that("Function returns correct date", {
  expect_equal(fq_short_to_date(20091),
               as.Date("01/04/2009", format = "%d/%m/%Y"))
  expect_equal(fq_short_to_date(20092),
               as.Date("01/07/2009", format = "%d/%m/%Y"))
  expect_equal(fq_short_to_date(20093),
               as.Date("01/10/2009", format = "%d/%m/%Y"))
  expect_equal(fq_short_to_date(20094),
               as.Date("01/01/2010", format = "%d/%m/%Y"))
})
