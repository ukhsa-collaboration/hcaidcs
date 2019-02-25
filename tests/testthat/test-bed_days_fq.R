context("Testing function for calculating NHS Trusts inpatient bed-days from quarterly KH03 returns")

average_kh03 <- data.frame(
  fq = c(20171,20172,20173,20174,20151,20152,20153,20154),
  trust_code = c(rep("RX1",4), rep("RYJ",4)),
  trust_name = c(rep("Nottingham University Hospitals",4), rep("Imperial College Healthcare",4)),
  kh03_avg = c(1425,1357,1392,1481,965,940,933,970)
)

bds <- dplyr::bind_cols(average_kh03, bed_days_fq(average_kh03,"kh03_avg","fq"))

test_that("Leap year is accounted for", {
  expect_equal(bds[8,]$days, 90)
  expect_equal(bds[4,]$days, 91)
})

test_that("Inpatient bed-days is accurate", {
  expect_equal(bds[8,]$bed_days, 87300)
  expect_equal(bds[4,]$bed_days, 134771)
})
