context("Testing that functions associated with producing age-sex pyramids work")

test_that("graph_age_groups works",{
  data(age_trends_data)
  t <- graph_age_groups(age_trends_data$age_group_new,
                        data_collection = "cdi")

  expect_equal(
    length(levels(t)), 6
  )
  rm(t)

  t <- graph_age_groups(age_trends_data$age_group_new,
                        data_collection = "bsi")
  expect_equal(
    length(levels(t)), 7
  )
})

test_that("abs_comma works", {
  expect_equal(hcaidcs:::abs_comma(-1000.43235), "1,000.432")
  expect_equal(hcaidcs:::abs_comma(1000.43235), "1,000.432")
  expect_equal(hcaidcs:::abs_comma(-100.43235), "100.4323")
})

test_that("age_sex_pyramid returns a ggplot object", {
  data(age_trends_data)
  p <- age_sex_pyramid(dat = subset(age_trends_data, fyear6 == 200708),
                       age_group = age_group2, sex_var = sex, y_var = rate,
                       y_var_label = "Rate, per 100,000 population")
    expect_is(p, c("gg", "ggplot"))
})
