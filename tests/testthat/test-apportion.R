context("Testing apportioning")

test_that("NA values in admission date are correctly handled", {
  expect_equal(
    apportion("mssa", patient_location = "Unknown", patient_category = NA,
              date_admitted = NA, specimen_date = as.Date("14/08/2017", "%d/%m/%Y"),
              date_entered = as.Date("14/08/2017", "%d/%m/%Y")
              ),
    1)
})

test_that("apportion.R does apportion cases that should be apportioned", {
  expect_equal(apportion("mssa", "NHS Acute Trust", "In-patient",
                         date_admitted = lubridate::dmy("01-01-2015"), specimen_date =
                         lubridate::dmy("05-01-2015"), date_entered = lubridate::dmy("26-10-2015")), 1)
  expect_equal(apportion("mssa", "Unknown", "",
                         date_admitted = lubridate::dmy(NA), specimen_date =
                           lubridate::ymd("2012-10-16"), date_entered = lubridate::ymd("2012-10-23")), 0)
})

test_that("apportion.R does not trust apportion records less than two days after admission", {
  expect_equal(apportion("mssa", "NHS Acute Trust", "In-patient", date_admitted =
                           lubridate::dmy("01-01-2015"),  specimen_date = lubridate::dmy("01-01-2015"),
                         date_entered = lubridate::dmy("26-10-2015")), 0)
})

# Test apportion_prior_healthcare

test_that("apportion_prior_healthcare returns correct result depending on difference between date of admission and date of specimen",
          {
            # specimen on same day as admission
            testdat <- data.frame(
              date_admitted = as.Date("01/01/2017", format = "%d/%m/%Y"),
              date_specimen = as.Date("01/01/2017", format = "%d/%m/%Y"),
              adm_4_wks = "no",
              adm_12_wks = "no",
              date_entered = as.Date("01/04/2017", format = "%d/%m/%Y"),
              stringsAsFactors = FALSE)
            expect_equal(
              apportion_prior_healthcare(
                testdat$date_admitted, testdat$date_specimen, testdat$adm_4_wks,
                testdat$adm_12_wks, testdat$date_entered), "coca")

            # 1 day difference
            testdat <- data.frame(
              date_admitted = as.Date("01/01/2017", format = "%d/%m/%Y"),
              date_specimen = as.Date("02/01/2017", format = "%d/%m/%Y"),
              adm_4_wks = "no",
              adm_12_wks = "no",
              date_entered = as.Date("01/04/2017", format = "%d/%m/%Y"),
              stringsAsFactors = FALSE)

            expect_equal(
              apportion_prior_healthcare(
                testdat$date_admitted, testdat$date_specimen, testdat$adm_4_wks,
                testdat$adm_12_wks, testdat$date_entered), "coca")

            # 2 days difference
            testdat <- data.frame(
              date_admitted = as.Date("01/01/2017", format = "%d/%m/%Y"),
              date_specimen = as.Date("03/01/2017", format = "%d/%m/%Y"),
              adm_4_wks = "no",
              adm_12_wks = "no",
              date_entered = as.Date("01/04/2017", format = "%d/%m/%Y"),
              stringsAsFactors = FALSE)

            expect_equal(
              apportion_prior_healthcare(
                testdat$date_admitted, testdat$date_specimen, testdat$adm_4_wks,
                testdat$adm_12_wks, testdat$date_entered), "hoha")
          })

test_that("Apportioning to coha works", {
  testdat <- data.frame(
    date_admitted = as.Date("01/01/2017", format = "%d/%m/%Y"),
    date_specimen = as.Date("01/01/2017", format = "%d/%m/%Y"),
    adm_4_wks = "yes",
    adm_12_wks = "no",
    date_entered = as.Date("01/04/2017", format = "%d/%m/%Y"),
    stringsAsFactors = FALSE)

  expect_equal(
    apportion_prior_healthcare(
      testdat$date_admitted, testdat$date_specimen, testdat$adm_4_wks,
      testdat$adm_12_wks, testdat$date_entered), "coha")
})

test_that("Apportioning to coia works", {
  testdat <- data.frame(
    date_admitted = as.Date("01/01/2017", format = "%d/%m/%Y"),
    date_specimen = as.Date("01/01/2017", format = "%d/%m/%Y"),
    adm_4_wks = "no",
    adm_12_wks = "yes",
    date_entered = as.Date("01/04/2017", format = "%d/%m/%Y"),
    stringsAsFactors = FALSE)

  expect_equal(
    apportion_prior_healthcare(
      testdat$date_admitted, testdat$date_specimen, testdat$adm_4_wks,
      testdat$adm_12_wks, testdat$date_entered), "coia")
})

test_that("Apportioning to coca works", {
  testdat <- data.frame(
    date_admitted = as.Date("01/01/2017", format = "%d/%m/%Y"),
    date_specimen = as.Date("01/01/2017", format = "%d/%m/%Y"),
    adm_4_wks = "no",
    adm_12_wks = "no",
    date_entered = as.Date("01/04/2017", format = "%d/%m/%Y"),
    stringsAsFactors = FALSE)

  expect_equal(
    apportion_prior_healthcare(
      testdat$date_admitted, testdat$date_specimen, testdat$adm_4_wks,
      testdat$adm_12_wks, testdat$date_entered), "coca")
})

test_that("Apportioning by prior health care is vectorised", {
  testdat <- data.frame(
    date_admitted = rep(as.Date("01/01/2017", format = "%d/%m/%Y"), 2),
    date_specimen = as.Date(c("01/01/2017", "05/01/2017"), format = "%d/%m/%Y"),
    adm_4_wks = rep("no", 2),
    adm_12_wks = rep("no", 2),
    date_entered = rep(as.Date("01/04/2017", format = "%d/%m/%Y"), 2),
    stringsAsFactors = FALSE)

  expect_equal(
    length(unique(apportion_prior_healthcare(
      testdat$date_admitted, testdat$date_specimen, testdat$adm_4_wks,
      testdat$adm_12_wks, testdat$date_entered))), 2)
})

test_that("Apportioning by prior health care returns NA when date record created earlier than 01/04/2017", {
  testdat <- data.frame(
    date_admitted = as.Date("01/01/2017", format = "%d/%m/%Y"),
    date_specimen = as.Date("01/01/2017", format = "%d/%m/%Y"),
    adm_4_wks = "no",
    adm_12_wks = "no",
    date_entered = as.Date("31/03/2017", format = "%d/%m/%Y"),
    stringsAsFactors = FALSE)

  expect_equal(apportion_prior_healthcare(
    testdat$date_admitted, testdat$date_specimen, testdat$adm_4_wks,
    testdat$adm_12_wks, testdat$date_entered), NA)
})

test_that("Apportioning by prior health care works inside dplyr", {
  testdat <- data.frame(
    date_admitted = rep(as.Date("01/01/2017", format = "%d/%m/%Y"), 2),
    date_specimen = as.Date(c("01/01/2017", "05/01/2017"), format = "%d/%m/%Y"),
    adm_4_wks = rep("no", 2),
    adm_12_wks = rep("no", 2),
    date_entered = rep(as.Date("01/04/2017", format = "%d/%m/%Y"), 2),
    stringsAsFactors = FALSE)

  expect_equal(ncol(dplyr::mutate(testdat,
                               new_apportioned = apportion_prior_healthcare(
                                 date_admitted, date_specimen, adm_4_wks,
                                 adm_12_wks, date_entered))), 6
                 )
})
