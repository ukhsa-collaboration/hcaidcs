context("Testing function for calculating Independent provider's modified inpatient bed-days")

library(lubridate)

test_that("Value is 1 (discharge) when admitted and discharged on the same day", {
  expect_equal(IS_modified_bed_days(bd_start_dt = dmy("01/04/2017"),
                                    bd_end_dt = dmy("31/03/2018"),
                                    admitted_dt = dmy("17/04/2017"),
                                    discharge_dt = dmy("17/04/2017"))$mod_is_denom, 1)
})

test_that("Value is 1 (discharge) when discharged on the first day of interest", {
  expect_equal(IS_modified_bed_days(bd_start_dt = dmy("01/04/2017"),
                                    bd_end_dt = dmy("31/03/2018"),
                                    admitted_dt = dmy("17/03/2017"),
                                    discharge_dt = dmy("01/04/2017"))$mod_is_denom, 1)
})

test_that("Value is 1 (bed-day) when admitted on the last day of interest", {
  expect_equal(IS_modified_bed_days(bd_start_dt = dmy("01/04/2017"),
                                    bd_end_dt = dmy("31/03/2018"),
                                    admitted_dt = dmy("31/03/2018"),
                                    discharge_dt = dmy("01/04/2018"))$mod_is_denom, 1)
})

test_that("Value is 365 when period of interest is a year, patient is admitted all through it and is not discharged within the period", {
  expect_equal(IS_modified_bed_days(bd_start_dt = dmy("01/04/2017"),
                                    bd_end_dt = dmy("31/03/2018"),
                                    admitted_dt = dmy("01/04/2017"),
                                    discharge_dt = dmy("23/04/2018"))$mod_is_denom, 365)
})

test_that("Value is 366 when period of interest is a leap year, patient is admitted all through it and is not discharged within the period", {
  expect_equal(IS_modified_bed_days(bd_start_dt = dmy("01/04/2015"),
                                    bd_end_dt = dmy("31/03/2016"),
                                    admitted_dt = dmy("01/04/2015"),
                                    discharge_dt = dmy("23/04/2016"))$mod_is_denom, 366)
})

test_that("Value is 0 when period of interest is after the admission period", {
  expect_equal(IS_modified_bed_days(bd_start_dt = dmy("01/04/2017"),
                                    bd_end_dt = dmy("31/03/2018"),
                                    admitted_dt = dmy("01/04/2013"),
                                    discharge_dt = dmy("30/03/2017"))$mod_is_denom, 0)
})


test_that("Value is 0 when period of interest is before the admission period", {
  expect_equal(IS_modified_bed_days(bd_start_dt = dmy("01/04/2011"),
                                    bd_end_dt = dmy("31/03/2012"),
                                    admitted_dt = dmy("01/04/2013"),
                                    discharge_dt = dmy("30/03/2017"))$mod_is_denom, 0)
})

test_that("Value is 0 when discharge date is before admission date", {
  expect_equal(IS_modified_bed_days(bd_start_dt = dmy("01/04/2017"),
                                    bd_end_dt = dmy("31/03/2018"),
                                    admitted_dt = dmy("31/03/2018"),
                                    discharge_dt = dmy("01/04/2017"))$mod_is_denom, 0)
})
