context("Test that mf_create_text_values works as expected")
data("mf_trend_data")

# test assertions ####
test_that("assertion for collection works", {
  expect_error(data = mf_trend_data, collection = "Aardvark",
               output_reqd = "12_month")
})

test_that("assertion for output works", {
  expect_error(data = mf_trend_data, collection = "cdi",
               output_reqd = "Aardvark")
})

# manipulate trend data here
library(dplyr)
mf_trend_data <- mf_trend_data %>% mf_lag_trend()

# test output ####
test_that("mf_create_text_values outputs expected data for 12_month", {
  expect_equal(mf_create_text_values(data = mf_trend_data, collection = "cdi",
                                     output_reqd = "12_month"),
               ((1092 - 1058) / 1058) * 100
               )
})

test_that("mf_create_text_values outputs expected data for 3_month", {
  expect_equal(mf_create_text_values(data = mf_trend_data, collection = "cdi",
                                     output_reqd = "3_month"),
               ((3534 - 3366) / 3366) * 100
  )
})

test_that("mf_create_text_values outputs expected data for 12_avg", {

})
