context("Test that qec_age_table works")

test_that("qec_age_table works", {
  dat <- data.frame(age_group = c(1, 1, 1, 2, 2, 2, 2),
                    denominator = c(500000,500000,500000, 600000, 600000, 600000, 600000))
  t <- qec_age_table(df = dat, age_var = age_group, denom = denominator)
  expect_equal(t[[2,4]], 0.667, tolerance = 1)
})
