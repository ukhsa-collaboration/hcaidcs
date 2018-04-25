context("Producing figure 1 for the monthly HCAI factsheet")

test_that("mf_fig1_fun produces a ggplot graph", {
  data(mf_trend_data)
  sum(mf_trend_data$mrsa)
  expect_match(class(my_plot <- mf_fig1_fun(data = mf_trend_data, collection = "mrsa"))[1], "gg")
})
