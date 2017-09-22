context("Check that date_to_fyear works")

test_that("Function returns preceeding cal year for first three months",{
  x <- as.Date("01/01/2017", format = "%d/%m/%Y")
  y <- as.Date("01/02/2017", format = "%d/%m/%Y")
  z <- as.Date("01/03/2017", format = "%d/%m/%Y")
  expect_equal(date_to_fyear(x), 2016)
  expect_equal(date_to_fyear(y), 2016)
  expect_equal(date_to_fyear(z), 2016)
})
