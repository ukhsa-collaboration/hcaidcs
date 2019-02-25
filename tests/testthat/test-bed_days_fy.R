context("Testing function for calculating NHS Trusts inpatient bed-days from annual KH03 returns")

average_kh03 <- data.frame(
  fy = c(200708,200809,200708,200809),
  trust_code = c(rep("RX1",2), rep("RYJ",2)),
  trust_name = c(rep("Nottingham University Hospitals",2), rep("Imperial College Healthcare",2)),
  kh03_avg = c(1508,1537,1211,1250)
)

bds <- dplyr::bind_cols(average_kh03, bed_days_fy(average_kh03,"kh03_avg","fy"))

test_that("Leap year is accounted for", {
  expect_equal(bds[1,]$days, 366)
  expect_equal(bds[2,]$days, 365)
})

test_that("Inpatient bed-days is accurate", {
  expect_equal(bds[1,]$bed_days, 551928)
  expect_equal(bds[4,]$bed_days, 456250)
})

