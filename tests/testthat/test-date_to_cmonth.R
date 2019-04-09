context("Test date_to_cmonth")

test_that("date_to_cmonth assertions work", {
  expect_error(date_to_cmonth("01/01/2019"))
  expect_error(date_to_cmonth(as.Date("01/01/2019", format = "%d/%m/%Y"),
                              as_factor = "Aardvark"))
})

test_that("date_to_cmonth works", {
  expect_equal(date_to_cmonth(as.Date("01/01/2019", format = "%d/%m/%Y")),
               1)
  expect_equal(date_to_cmonth(as.Date("01/01/2019", format = "%d/%m/%Y"), as_factor = TRUE),
               factor("January", levels = month.name))
})
