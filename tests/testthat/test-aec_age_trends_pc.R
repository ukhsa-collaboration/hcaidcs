context("Check that aec_age_trends works")
# tips for testing ggplot2 functions: https://stackoverflow.com/questions/31038709/how-to-write-a-test-for-a-ggplot-plot

test_that("aec_age_trends_pc returns a ggplot object", {
  data(age_trends_data)
  p <- aec_age_trend_pc(collection = "CDI", data = age_trends_data,
                        x = "fyear6", y = "age_sex_pc", sex = "sex",
                        group = "age_group_new")
  expect_is(p, c("gg", "ggplot"))
})

test_that("aec_age_trends_pc assertions work", {
  data(age_trends_data)
  expect_error(p <- aec_age_trend_pc(collection = "aardvark",
                                     data = age_trends_data,
                        x = "fyear6", y = "age_sex_pc", sex = "sex",
                        group = "age_group_new"))
})

test_that("aec_age_trend_rate  returns a ggplot object", {
  data(age_trends_data)
  p <- aec_age_trend_rate(collection = "CDI", data = age_trends_data,
                          x = "fyear6", y = "rate", sex = "sex",
                          group = "age_group_new",
                          log_scale = TRUE)
  expect_is(p, c("gg", "ggplot"))
})

test_that("aec_age_trend_rate assertions work", {
  data(age_trends_data)
  expect_error(p <- aec_age_trend_rate(collection = "aardvark",
                                     data = age_trends_data,
                                     x = "fyear6", y = "rate", sex = "sex",
                                     group = "age_group_new"))
})

test_that("aec_age_trend_rate assertions work", {
  data(age_trends_data)
  names(age_trends_data)[2] <- "age_group_old"
  expect_error(p <- aec_age_trend_rate(collection = "CDI",
                                     data = age_trends_data,
                                     x = "fyear6", y = "rate", sex = "sex",
                                     group = "age_group_old"))
})

# age_trends_rate_bar ####
test_that("aec_age_trend_rate_bar  returns a ggplot object", {
  data(age_trends_data)
  p <- aec_age_trend_rate_bar(collection = "CDI", data = age_trends_data,
                              x = "fyear6", y = "rate", sex = "sex",
                              group = "age_group_new",
                              log_scale = FALSE)
  expect_is(p, c("gg", "ggplot"))
})

test_that("aec_age_trend_rate_bar assertions work", {
  data(age_trends_data)
  expect_error(p <- aec_age_trend_rate_bar(collection = "Aardvark",
                                           data = age_trends_data,
                                           x = "fyear6", y = "rate",
                                           sex = "sex", group = "age_group_new",
                                           log_scale = FALSE))
})

test_that("aec_age_trend_rate_bar assertions work", {
  data(age_trends_data)
  names(age_trends_data)[2] <- "age_group_old"
  expect_error(p <- aec_age_trend_rate_bar(collection = "CDI",
                                           data = age_trends_data,
                                           x = "fyear6", y = "rate",
                                           sex = "sex", group = "age_group_old",
                                           log_scale = FALSE))
})

# aec_age_trend_rate_pc_change  ####

test_that("aec_age_trend_rate_pc_change  returns a ggplot object", {
  data(age_trends_data)
  p <- aec_age_trend_rate_pc_change(data = age_trends_data, collection = "CDI")
  expect_is(p, c("gg", "ggplot"))
})

test_that("aec_age_trend_rate_pc_change assertions work", {
  data(age_trends_data)
  expect_error(p <- aec_age_trend_rate_pc_change(data = age_trends_data,
                                                 collection = "Aardvark"))
})

test_that("aec_age_trend_rate_pc_change assertions work", {
  data(age_trends_data)
  names(age_trends_data)[2] <- "age_group_old"
  expect_error(p <- aec_age_trend_rate_pc_change(data = age_trends_data,
                                                 collection = "CDI"))

  data(age_trends_data)
  names(age_trends_data)[3] <- "not_sex"
  expect_error(p <- aec_age_trend_rate_pc_change(data = age_trends_data,
                                                 collection = "CDI"))

  data(age_trends_data)
  names(age_trends_data)[1] <- "not_fyear6"
  expect_error(p <- aec_age_trend_rate_pc_change(data = age_trends_data,
                                                 collection = "CDI"))

  data(age_trends_data)
  names(age_trends_data)[6] <- "not_rate"
  expect_error(p <- aec_age_trend_rate_pc_change(data = age_trends_data,
                                                 collection = "CDI"))
})
