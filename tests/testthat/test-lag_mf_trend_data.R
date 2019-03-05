context("Tests for lag_mf_trend")

# data("mf_trend_data")

test_that("lag 12 works",{
  expect_equal(mf_lag_trend(mf_trend_data)$cdi_lag_12[nrow(mf_trend_data)],
               mf_trend_data$cdi[nrow(mf_trend_data)-12])
})

test_that("sum 3 works",{
  expect_equal(mf_lag_trend(mf_trend_data)$cdi_sum_3[nrow(mf_trend_data)],
               sum(mf_trend_data$cdi[c((nrow(mf_trend_data)-2):nrow(mf_trend_data))], na.rm = TRUE )
               )
})

# lag 3 creates variable comparing sum of 3 months for current month to sum of
# 3 months, twelve months prior
test_that("lag 3 works",{
  expect_equal(mf_lag_trend(mf_trend_data)$cdi_sum_3_lag_3[nrow(mf_trend_data)],
               # not going to calculate in R
               3366
  )
  # mf_lag_trend(mf_trend_data)
})

test_that("sum 12 works",{
  expect_equal(mf_lag_trend(mf_trend_data)$cdi_sum_12[nrow(mf_trend_data)],
               sum(mf_trend_data$cdi[c((nrow(mf_trend_data)-11):nrow(mf_trend_data))], na.rm = TRUE )
  )
})

test_that("sum 12 lag 12 works",{
  expect_equal(mf_lag_trend(mf_trend_data)$cdi_sum_12_lag_12[nrow(mf_trend_data)],
               13800)
})
