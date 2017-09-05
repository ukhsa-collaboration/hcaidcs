context("Creating a KH03 year")

test_that("Not providing a valid time format returns an error", {
  expect_error(kh03_year("2000-01", "March", "aardvark"))
})

test_that("The function works", {
  expect_equal(kh03_year("2010-11", "March", "fyear"), 20104)
})

# test_that("Creating a KH03 year is vectorised", {
#   dat <- data.frame(my_year = c("2010-11", "2007-08"), my_q = c("March", "June"))
#   dat$kh03_year <- kh03_year(dat$my_year, dat$my_q, "fyear")
#   expect_equal(length(unique(dat$kh03_year)), 2)
# })
