context("Testing apportioning on prior healthcare exposure, by date")

# Does hoha apportion? ####

test_that("Cases which are hoha are appropriately apportioned", {
  dat <- data.frame(organism = "CDI",
    pat_loc = "NHS Acute Trust", pat_cat = "In-patient",
                    dat_admit = as.Date(NA_real_, origin = "01-01-1970"),
                    spec_date = as.Date("14/08/2017", "%d/%m/%Y"),
                    dat_ent = as.Date("14/08/2017", "%d/%m/%Y"),
                    dat_dis = as.Date(NA_real_, origin = "01-01-1970"),
                    admitted_at_3_mo = "no",
                    stringsAsFactors = FALSE
                    )
  expect_equal(
    apportion_phc_date(
      data_collection = dat$organism,
      patient_location = dat$pat_loc,
                       patient_category = dat$pat_cat,
              adm_date = dat$dat_admit, spec_date = dat$spec_date,
              adm_3_mo = dat$admitted_at_3_mo,
              date_discharge = dat$dat_dis,
              date_record_created = dat$dat_ent),
    "hoha")

  # test works with date admission at two days
  dat <- data.frame(organism = "CDI",
                    pat_loc = "NHS Acute Trust", pat_cat = "In-patient",
                    dat_admit = as.Date("12/08/2017", "%d/%m/%Y"),
                    spec_date = as.Date("14/08/2017", "%d/%m/%Y"),
                    dat_ent = as.Date("14/08/2017", "%d/%m/%Y"),
                    dat_dis = as.Date(NA_real_, origin = "01-01-1970"),
                    admitted_at_3_mo = "no",
                    stringsAsFactors = FALSE
  )
  expect_equal(
    apportion_phc_date(data_collection = dat$organism,
                       patient_location = dat$pat_loc,
                       patient_category = dat$pat_cat,
                       adm_date = dat$dat_admit, spec_date = dat$spec_date,
                       adm_3_mo = dat$admitted_at_3_mo,
                       date_discharge = dat$dat_dis,
                       date_record_created = dat$dat_ent),
    "hoha")

  # testing situation where user has supplied prior admissions, but should still be hoha

  dat <- data.frame(organism = "CDI",
                    pat_loc = "NHS Acute Trust", pat_cat = "In-patient",
                    dat_admit = as.Date("12/08/2017", "%d/%m/%Y"),
                    spec_date = as.Date("14/08/2017", "%d/%m/%Y"),
                    dat_ent = as.Date("14/08/2017", "%d/%m/%Y"),
                    # COHA is <28 days
                    dat_dis = as.Date("18/07/2017", "%d/%m/%Y"),
                    admitted_at_3_mo = "yes",
                    stringsAsFactors = FALSE
  )
  expect_equal(
    apportion_phc_date(data_collection = dat$organism,
                       patient_location = dat$pat_loc,
                       patient_category = dat$pat_cat,
                       adm_date = dat$dat_admit, spec_date = dat$spec_date,
                       adm_3_mo = dat$admitted_at_3_mo,
                       date_discharge = dat$dat_dis,
                       date_record_created = dat$dat_ent),
    "hoha")
})

# COHAs ####

test_that("Let's test the COHAs", {
  dat <- data.frame(organism = "CDI",
                    pat_loc = "NHS Acute Trust", pat_cat = "In-patient",
                    dat_admit = as.Date("14/08/2017", "%d/%m/%Y"),
                    spec_date = as.Date("14/08/2017", "%d/%m/%Y"),
                    dat_ent = as.Date("14/08/2017", "%d/%m/%Y"),
                    # COHA is <28 days
                    dat_dis = as.Date("12/08/2017", "%d/%m/%Y"),
                    admitted_at_3_mo = "yes",
                    stringsAsFactors = FALSE
  )
  expect_equal(
    apportion_phc_date(data_collection = dat$organism,
                       patient_location = dat$pat_loc,
                       patient_category = dat$pat_cat,
                       adm_date = dat$dat_admit, spec_date = dat$spec_date,
                       adm_3_mo = dat$admitted_at_3_mo,
                       date_discharge = dat$dat_dis,
                       date_record_created = dat$dat_ent),
    "coha")

  # let's try a date difference of 27 days
  dat <- data.frame(organism = "CDI",
                    pat_loc = "NHS Acute Trust", pat_cat = "In-patient",
                    dat_admit = as.Date("14/08/2017", "%d/%m/%Y"),
                    spec_date = as.Date("14/08/2017", "%d/%m/%Y"),
                    dat_ent = as.Date("14/08/2017", "%d/%m/%Y"),
                    # COHA is <28 days
                    dat_dis = as.Date("18/07/2017", "%d/%m/%Y"),
                    admitted_at_3_mo = "yes",
                    stringsAsFactors = FALSE
  )
  expect_equal(
    apportion_phc_date(data_collection = dat$organism,
                       patient_location = dat$pat_loc,
                       patient_category = dat$pat_cat,
                       adm_date = dat$dat_admit, spec_date = dat$spec_date,
                       adm_3_mo = dat$admitted_at_3_mo,
                       date_discharge = dat$dat_dis,
                       date_record_created = dat$dat_ent),
    "coha")

  # let's try a date difference of 28 days
  # This is not true, which is good.
  # dat <- data.frame(pat_loc = "NHS Acute Trust", pat_cat = "In-patient",
  #                   dat_admit = as.Date("14/08/2017", "%d/%m/%Y"),
  #                   spec_date = as.Date("14/08/2017", "%d/%m/%Y"),
  #                   dat_ent = as.Date("14/08/2017", "%d/%m/%Y"),
  #                   # COHA is <28 days
  #                   dat_dis = as.Date("17/07/2017", "%d/%m/%Y"),
  #                   admitted_at_3_mo = "yes",
  #                   stringsAsFactors = FALSE
  # )
  # expect_false(
  #   apportion_phc_date(patient_location = dat$pat_loc,
  #                      patient_category = dat$pat_cat,
  #                      adm_date = dat$dat_admit, spec_date = dat$spec_date,
  #                      adm_3_mo = dat$admitted_at_3_mo,
  #                      date_discharge = dat$dat_dis,
  #                      date_record_created = dat$dat_ent),
  #   "coha")

})

# COIAs ####

test_that("COIAs work", {

  dat <- data.frame(organism = "CDI",
                    pat_loc = "NHS Acute Trust", pat_cat = "In-patient",
                    dat_admit = as.Date("14/08/2017", "%d/%m/%Y"),
                    spec_date = as.Date("14/08/2017", "%d/%m/%Y"),
                    dat_ent = as.Date("14/08/2017", "%d/%m/%Y"),
                    # COHA is <28 days
                    dat_dis = as.Date("18/07/2017", "%d/%m/%Y"),
                    admitted_at_3_mo = "yes",
                    stringsAsFactors = FALSE
  )
  expect_equal(
    apportion_phc_date(data_collection = dat$organism,
                       patient_location = dat$pat_loc,
                       patient_category = dat$pat_cat,
                       adm_date = dat$dat_admit, spec_date = dat$spec_date,
                       adm_3_mo = dat$admitted_at_3_mo,
                       date_discharge = dat$dat_dis,
                       date_record_created = dat$dat_ent),
    "coha")

  # let's try a date difference of 28 days

  dat <- data.frame(organism = "CDI",
                    pat_loc = "NHS Acute Trust", pat_cat = "In-patient",
                    dat_admit = as.Date("14/08/2017", "%d/%m/%Y"),
                    spec_date = as.Date("14/08/2017", "%d/%m/%Y"),
                    dat_ent = as.Date("14/08/2017", "%d/%m/%Y"),
                    # COHA is <28 days
                    dat_dis = as.Date("17/07/2017", "%d/%m/%Y"),
                    admitted_at_3_mo = "yes",
                    stringsAsFactors = FALSE
  )
  expect_equal(
    apportion_phc_date(data_collection = dat$organism,
                       patient_location = dat$pat_loc,
                       patient_category = dat$pat_cat,
                       adm_date = dat$dat_admit, spec_date = dat$spec_date,
                       adm_3_mo = dat$admitted_at_3_mo,
                       date_discharge = dat$dat_dis,
                       date_record_created = dat$dat_ent),
    "coia")

  # 83 days
  dat <- data.frame(organism = "CDI",
                    pat_loc = "NHS Acute Trust", pat_cat = "In-patient",
                    dat_admit = as.Date("14/08/2017", "%d/%m/%Y"),
                    spec_date = as.Date("14/08/2017", "%d/%m/%Y"),
                    dat_ent = as.Date("14/08/2017", "%d/%m/%Y"),
                    # COHA is <28 days
                    dat_dis = as.Date("23/05/2017", "%d/%m/%Y"),
                    admitted_at_3_mo = "yes",
                    stringsAsFactors = FALSE
  )
  expect_equal(
    apportion_phc_date(data_collection = dat$organism,
                       patient_location = dat$pat_loc,
                       patient_category = dat$pat_cat,
                       adm_date = dat$dat_admit, spec_date = dat$spec_date,
                       adm_3_mo = dat$admitted_at_3_mo,
                       date_discharge = dat$dat_dis,
                       date_record_created = dat$dat_ent),
    "coia")
})

# COCAs ####

test_that("COCAs work", {
  # 84 days
  dat <- data.frame(organism = "CDI",
                    pat_loc = "NHS Acute Trust", pat_cat = "In-patient",
                    dat_admit = as.Date("14/08/2017", "%d/%m/%Y"),
                    spec_date = as.Date("14/08/2017", "%d/%m/%Y"),
                    dat_ent = as.Date("14/08/2017", "%d/%m/%Y"),
                    # COHA is <28 days
                    dat_dis = as.Date("22/05/2017", "%d/%m/%Y"),
                    admitted_at_3_mo = "yes",
                    stringsAsFactors = FALSE
  )
  expect_equal(
    apportion_phc_date(data_collection = dat$organism,
                       patient_location = dat$pat_loc,
                       patient_category = dat$pat_cat,
                       adm_date = dat$dat_admit, spec_date = dat$spec_date,
                       adm_3_mo = dat$admitted_at_3_mo,
                       date_discharge = dat$dat_dis,
                       date_record_created = dat$dat_ent),
    "coca")
})

# Awkward cases ####

test_that("Let's test those awkward cases", {
  # adm_3_mo == "no" & hoha != 1 should be coca
  dat <- data.frame(organism = "CDI",
                    pat_loc = "NHS Acute Trust", pat_cat = "In-patient",
                    dat_admit = as.Date("14/08/2017", "%d/%m/%Y"),
                    spec_date = as.Date("14/08/2017", "%d/%m/%Y"),
                    dat_ent = as.Date("14/08/2017", "%d/%m/%Y"),
                    # COHA is <28 days
                    dat_dis = as.Date("12/08/2017", "%d/%m/%Y"),
                    admitted_at_3_mo = "no",
                    stringsAsFactors = FALSE
  )
  expect_equal(
    apportion_phc_date(data_collection = dat$organism,
                       patient_location = dat$pat_loc,
                       patient_category = dat$pat_cat,
                       adm_date = dat$dat_admit, spec_date = dat$spec_date,
                       adm_3_mo = dat$admitted_at_3_mo,
                       date_discharge = dat$dat_dis,
                       date_record_created = dat$dat_ent),
    "coca")
  # same but for hoha case which takes precedent
  dat <- data.frame(organism = "CDI",
                    pat_loc = "NHS Acute Trust", pat_cat = "In-patient",
                    dat_admit = as.Date("12/08/2017", "%d/%m/%Y"),
                    spec_date = as.Date("14/08/2017", "%d/%m/%Y"),
                    dat_ent = as.Date("14/08/2017", "%d/%m/%Y"),
                    # COHA is <28 days
                    dat_dis = as.Date("10/08/2017", "%d/%m/%Y"),
                    admitted_at_3_mo = "no",
                    stringsAsFactors = FALSE
  )
  expect_equal(
    apportion_phc_date(data_collection = dat$organism,
                       patient_location = dat$pat_loc,
                       patient_category = dat$pat_cat,
                       adm_date = dat$dat_admit, spec_date = dat$spec_date,
                       adm_3_mo = dat$admitted_at_3_mo,
                       date_discharge = dat$dat_dis,
                       date_record_created = dat$dat_ent),
    "hoha")

  #adm_3_mo == "don't know" & !is.na(adm_3_mo) & z != "hoha"
  dat <- data.frame(organism = "CDI",
                    pat_loc = "NHS Acute Trust", pat_cat = "In-patient",
                    dat_admit = as.Date("14/08/2017", "%d/%m/%Y"),
                    spec_date = as.Date("14/08/2017", "%d/%m/%Y"),
                    dat_ent = as.Date("14/08/2017", "%d/%m/%Y"),
                    # COHA is <28 days
                    dat_dis = as.Date("12/08/2017", "%d/%m/%Y"),
                    admitted_at_3_mo = "don't know",
                    stringsAsFactors = FALSE
  )
  expect_equal(
    apportion_phc_date(data_collection = dat$organism,
                       patient_location = dat$pat_loc,
                       patient_category = dat$pat_cat,
                       adm_date = dat$dat_admit, spec_date = dat$spec_date,
                       adm_3_mo = dat$admitted_at_3_mo,
                       date_discharge = dat$dat_dis,
                       date_record_created = dat$dat_ent),
    "unknown_3_mo")
  # same but for hoha case which takes precedent
  dat <- data.frame(organism = "CDI",
                    pat_loc = "NHS Acute Trust", pat_cat = "In-patient",
                    dat_admit = as.Date("12/08/2017", "%d/%m/%Y"),
                    spec_date = as.Date("14/08/2017", "%d/%m/%Y"),
                    dat_ent = as.Date("14/08/2017", "%d/%m/%Y"),
                    # COHA is <28 days
                    dat_dis = as.Date("10/08/2017", "%d/%m/%Y"),
                    admitted_at_3_mo = "don't know",
                    stringsAsFactors = FALSE
  )
  expect_equal(
    apportion_phc_date(data_collection = dat$organism,
                       patient_location = dat$pat_loc,
                       patient_category = dat$pat_cat,
                       adm_date = dat$dat_admit, spec_date = dat$spec_date,
                       adm_3_mo = dat$admitted_at_3_mo,
                       date_discharge = dat$dat_dis,
                       date_record_created = dat$dat_ent),
    "hoha")

  # what about the weird case when yes for prior admission, but >83 days
  dat <- data.frame(organism = "CDI",
                    pat_loc = "NHS Acute Trust", pat_cat = "In-patient",
                    dat_admit = as.Date("12/08/2017", "%d/%m/%Y"),
                    spec_date = as.Date("12/08/2017", "%d/%m/%Y"),
                    dat_ent = as.Date("14/08/2017", "%d/%m/%Y"),

                    dat_dis = as.Date("19/05/2017", "%d/%m/%Y"),
                    admitted_at_3_mo = "yes",
                    stringsAsFactors = FALSE
  )
  expect_equal(
    apportion_phc_date(data_collection = dat$organism,
                       patient_location = dat$pat_loc,
                       patient_category = dat$pat_cat,
                       adm_date = dat$dat_admit, spec_date = dat$spec_date,
                       adm_3_mo = dat$admitted_at_3_mo,
                       date_discharge = dat$dat_dis,
                       date_record_created = dat$dat_ent),
    "coca")

  # What about when adm_3_mo = yes, but date discharge > date admission
  dat <- data.frame(organism = "E. coli",
                    pat_loc = "NHS Acute Trust", pat_cat = "In-patient",
                    dat_admit = as.Date("12/08/2017", "%d/%m/%Y"),
                    spec_date = as.Date("12/08/2017", "%d/%m/%Y"),
                    dat_ent = as.Date("14/08/2017", "%d/%m/%Y"),

                    dat_dis = as.Date("19/09/2017", "%d/%m/%Y"),
                    admitted_at_3_mo = "yes",
                    stringsAsFactors = FALSE
  )
  expect_equal(
    apportion_phc_date(data_collection = dat$organism,
                       patient_location = dat$pat_loc,
                       patient_category = dat$pat_cat,
                       adm_date = dat$dat_admit, spec_date = dat$spec_date,
                       adm_3_mo = dat$admitted_at_3_mo,
                       date_discharge = dat$dat_dis,
                       date_record_created = dat$dat_ent),
    "unknown_1_mo")

  # What about when adm_3_mo = yes, but date discharge > date admission - CDI
  dat <- data.frame(organism = "CDI",
                    pat_loc = "NHS Acute Trust", pat_cat = "In-patient",
                    dat_admit = as.Date("12/08/2017", "%d/%m/%Y"),
                    spec_date = as.Date("12/08/2017", "%d/%m/%Y"),
                    dat_ent = as.Date("14/08/2017", "%d/%m/%Y"),

                    dat_dis = as.Date("19/09/2017", "%d/%m/%Y"),
                    admitted_at_3_mo = "yes",
                    stringsAsFactors = FALSE
  )
  expect_equal(
    apportion_phc_date(data_collection = dat$organism,
                       patient_location = dat$pat_loc,
                       patient_category = dat$pat_cat,
                       adm_date = dat$dat_admit, spec_date = dat$spec_date,
                       adm_3_mo = dat$admitted_at_3_mo,
                       date_discharge = dat$dat_dis,
                       date_record_created = dat$dat_ent),
    "unknown_3_mo")

})

test_that("all_blank works", {
  dat <- data.frame(organism = "CDI",
                    pat_loc = "NHS Acute Trust", pat_cat = "In-patient",
                    dat_admit = as.Date("14/08/2017", "%d/%m/%Y"),
                    spec_date = as.Date("14/08/2017", "%d/%m/%Y"),
                    dat_ent = as.Date("14/08/2017", "%d/%m/%Y"),
                    # missing date of discharge
                    dat_dis = as.Date(NA_real_, origin = "01-01-1970"),
                    admitted_at_3_mo = "",
                    stringsAsFactors = FALSE
  )
  expect_equal(
    apportion_phc_date(data_collection = dat$organism,
                       patient_location = dat$pat_loc,
                       patient_category = dat$pat_cat,
                       adm_date = dat$dat_admit, spec_date = dat$spec_date,
                       adm_3_mo = dat$admitted_at_3_mo,
                       date_discharge = dat$dat_dis,
                       date_record_created = dat$dat_ent),
    "all_blank")
})

# Check that function works for bacteraemia cases ####
test_that("Cases which are hoha are appropriately apportioned", {
  dat <- data.frame(organism = "E. coli",
                    pat_loc = "NHS Acute Trust", pat_cat = "In-patient",
                    dat_admit = as.Date(NA_real_, origin = "01-01-1970"),
                    spec_date = as.Date("14/08/2017", "%d/%m/%Y"),
                    dat_ent = as.Date("14/08/2017", "%d/%m/%Y"),
                    dat_dis = as.Date(NA_real_, origin = "01-01-1970"),
                    admitted_at_3_mo = "no",
                    stringsAsFactors = FALSE
  )
  expect_equal(
    apportion_phc_date(
      data_collection = dat$organism,
      patient_location = dat$pat_loc,
      patient_category = dat$pat_cat,
      adm_date = dat$dat_admit, spec_date = dat$spec_date,
      adm_3_mo = dat$admitted_at_3_mo,
      date_discharge = dat$dat_dis,
      date_record_created = dat$dat_ent),
    "hoha")
  })

# COIAs should not occur

test_that("COIAs aren't returned", {
  # let's try a date difference of 27 days

  dat <- data.frame(organism = "E. coli",
                    pat_loc = "NHS Acute Trust", pat_cat = "In-patient",
                    dat_admit = as.Date("14/08/2017", "%d/%m/%Y"),
                    spec_date = as.Date("14/08/2017", "%d/%m/%Y"),
                    dat_ent = as.Date("14/08/2017", "%d/%m/%Y"),
                    # COHA is <28 days
                    dat_dis = as.Date("18/07/2017", "%d/%m/%Y"),
                    admitted_at_3_mo = "yes",
                    stringsAsFactors = FALSE
  )
  expect_equal(
    apportion_phc_date(data_collection = dat$organism,
                       patient_location = dat$pat_loc,
                       patient_category = dat$pat_cat,
                       adm_date = dat$dat_admit, spec_date = dat$spec_date,
                       adm_3_mo = dat$admitted_at_3_mo,
                       date_discharge = dat$dat_dis,
                       date_record_created = dat$dat_ent),
    "coha")

  # let's try a date difference of 28 days

  dat <- data.frame(organism = "E. coli",
                    pat_loc = "NHS Acute Trust", pat_cat = "In-patient",
                    dat_admit = as.Date("14/08/2017", "%d/%m/%Y"),
                    spec_date = as.Date("14/08/2017", "%d/%m/%Y"),
                    dat_ent = as.Date("14/08/2017", "%d/%m/%Y"),
                    # COHA is <28 days
                    dat_dis = as.Date("17/07/2017", "%d/%m/%Y"),
                    admitted_at_3_mo = "yes",
                    stringsAsFactors = FALSE
  )
  expect_equal(
    apportion_phc_date(data_collection = dat$organism,
                       patient_location = dat$pat_loc,
                       patient_category = dat$pat_cat,
                       adm_date = dat$dat_admit, spec_date = dat$spec_date,
                       adm_3_mo = dat$admitted_at_3_mo,
                       date_discharge = dat$dat_dis,
                       date_record_created = dat$dat_ent),
    "coca")

  # 83 days
  dat <- data.frame(organism = "E. coli",
                    pat_loc = "NHS Acute Trust", pat_cat = "In-patient",
                    dat_admit = as.Date("14/08/2017", "%d/%m/%Y"),
                    spec_date = as.Date("14/08/2017", "%d/%m/%Y"),
                    dat_ent = as.Date("14/08/2017", "%d/%m/%Y"),
                    # COHA is <28 days
                    dat_dis = as.Date("23/05/2017", "%d/%m/%Y"),
                    admitted_at_3_mo = "yes",
                    stringsAsFactors = FALSE
  )
  expect_equal(
    apportion_phc_date(data_collection = dat$organism,
                       patient_location = dat$pat_loc,
                       patient_category = dat$pat_cat,
                       adm_date = dat$dat_admit, spec_date = dat$spec_date,
                       adm_3_mo = dat$admitted_at_3_mo,
                       date_discharge = dat$dat_dis,
                       date_record_created = dat$dat_ent),
    "coca")
})

test_that("COCA works if not previously admitted", {
  dat <- data.frame(organism = "E. coli",
                    pat_loc = "NHS Acute Trust", pat_cat = "In-patient",
                    dat_admit = as.Date("14/08/2017", "%d/%m/%Y"),
                    spec_date = as.Date("14/08/2017", "%d/%m/%Y"),
                    dat_ent = as.Date("14/08/2017", "%d/%m/%Y"),
                    # COHA is <28 days
                    dat_dis = as.Date("NA", "%d/%m/%Y"),
                    admitted_at_3_mo = "No",
                    stringsAsFactors = FALSE
  )

  expect_equal(
    apportion_phc_date(data_collection = dat$organism,
                       patient_location = dat$pat_loc,
                       patient_category = dat$pat_cat,
                       adm_date = dat$dat_admit, spec_date = dat$spec_date,
                       adm_3_mo = dat$admitted_at_3_mo,
                       date_discharge = dat$dat_dis,
                       date_record_created = dat$dat_ent),
    "coca")
})
