context("Calculating date of first Wednesday of a month for a monthly output")

test_that("first_wednesday returns a date", {
  x <- as.Date("01-01-1970", format = "%d-%m-%Y")
  expect_is(first_wednesday(x), "Date")
  })

test_that("first_wednesday returns a Wednesday", {
  x <- as.Date("01-01-1970", format = "%d-%m-%Y")
  y <- first_wednesday(x)
  expect_equal(weekdays(first_wednesday(y)), "Wednesday")
})

rm(x, y)
