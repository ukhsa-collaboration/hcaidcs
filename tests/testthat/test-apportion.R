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

