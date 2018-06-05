context("Annual onset tables")

test_that("Assertions work",{
  test_dat <- "x"
  expect_error(ann_tab_onset_annual(dat = test_dat, timeperiod = "time_period",
                                    org_code = "org_code",
                                    denominator = "denominator",
                                    total= "this_is_the_total", ho = "ho",
                                    co = "co", org_type = "trust"))
})

test_that("Get expected output for trusts", {
  ann_onset_testdat <- data.frame(stringsAsFactors=FALSE,
                time_period = c(201415L, 201516L, 201617L, 201718L, 201415L, 201516L,
                                201617L, 201718L),
                   org_code = c("W1A", "W1A", "W1A", "W1A", "E17", "E17", "E17",
                                "E17"),
                denominator = c(100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L),
                      this_is_the_total = c(10L, 20L, 30L, 40L, 50L, 0L, 40L, 30L),
                         ho = c(8L, 16L, 22L, 31L, 40L, 0L, 35L, 15L),
                         co = c(2L, 4L, 8L, 9L, 10L, 0L, 5L, 15L),
         pir_trust_assigned = c(4L, 10L, 5L, 30L, 10L, 0L, 5L, 5L),
           pir_ccg_assigned = c(3L, 5L, 20L, 5L, 40L, 0L, 30L, 20L),
                third_party = c(3L, 5L, 5L, 5L, 0L, 0L, 5L, 5L)
      )
  expect_equal(
    names(ann_tab_onset_annual(ann_onset_testdat, timeperiod = time_period,
                         org_code = org_code, denominator = denominator,
                         total= this_is_the_total, ho = ho,
                         co = co, org_type = "trust"))[4],
    "April 2014 to March 2015_HO cases**"
  )
  expect_equal(
    names(ann_tab_onset_annual(ann_onset_testdat, timeperiod = time_period,
                               org_code = org_code, denominator = denominator,
                               total= this_is_the_total, ho = ho,
                               co = co, org_type = "trust"))[5],
    "April 2014 to March 2015_HO rate"
  )
})

test_that("Get expected output for CCGs", {
  ann_onset_testdat <- data.frame(stringsAsFactors=FALSE,
                                  time_period = c(201415L, 201516L, 201617L, 201718L, 201415L, 201516L,
                                                  201617L, 201718L),
                                  org_code = c("W1A", "W1A", "W1A", "W1A", "E17", "E17", "E17",
                                               "E17"),
                                  denominator = c(100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L),
                                  this_is_the_total = c(10L, 20L, 30L, 40L, 50L, 0L, 40L, 30L),
                                  ho = c(8L, 16L, 22L, 31L, 40L, 0L, 35L, 15L),
                                  co = c(2L, 4L, 8L, 9L, 10L, 0L, 5L, 15L),
                                  pir_trust_assigned = c(4L, 10L, 5L, 30L, 10L, 0L, 5L, 5L),
                                  pir_ccg_assigned = c(3L, 5L, 20L, 5L, 40L, 0L, 30L, 20L),
                                  third_party = c(3L, 5L, 5L, 5L, 0L, 0L, 5L, 5L)
  )
  expect_equal(
    names(ann_tab_onset_annual(ann_onset_testdat, timeperiod = time_period,
                               org_code = org_code, denominator = denominator,
                               total= this_is_the_total, ho = ho,
                               co = co, org_type = "CCG"))[4],
    "April 2014 to March 2015_CO cases**"
  )
  expect_equal(
    names(ann_tab_onset_annual(ann_onset_testdat, timeperiod = time_period,
                               org_code = org_code, denominator = denominator,
                               total= this_is_the_total, ho = ho,
                               co = co, org_type = "CCG"))[5],
    "April 2014 to March 2015_CO rate"
  )
})

test_that("Rate calculations are correct", {
  ann_onset_testdat <- data.frame(stringsAsFactors=FALSE,
                                  time_period = c(201415L, 201516L, 201617L, 201718L, 201415L, 201516L,
                                                  201617L, 201718L),
                                  org_code = c("W1A", "W1A", "W1A", "W1A", "E17", "E17", "E17",
                                               "E17"),
                                  denominator = c(100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L),
                                  this_is_the_total = c(10L, 20L, 30L, 40L, 50L, 0L, 40L, 30L),
                                  ho = c(8L, 16L, 22L, 31L, 40L, 0L, 35L, 15L),
                                  co = c(2L, 4L, 8L, 9L, 10L, 0L, 5L, 15L),
                                  pir_trust_assigned = c(4L, 10L, 5L, 30L, 10L, 0L, 5L, 5L),
                                  pir_ccg_assigned = c(3L, 5L, 20L, 5L, 40L, 0L, 30L, 20L),
                                  third_party = c(3L, 5L, 5L, 5L, 0L, 0L, 5L, 5L)
  )
  expect_equal(
    # E17, 201415, ho rate = (40 / 100) * 100000 = 40000
    ann_tab_onset_annual(ann_onset_testdat, timeperiod = time_period,
                         org_code = org_code, denominator = denominator,
                         total= this_is_the_total, ho = ho,
                         co = co, org_type = "trust")[1,5],
    40000
  )
  expect_equal(
    # E17, 201415, total rate = (50 / 100) * 100000 = 50000
    ann_tab_onset_annual(ann_onset_testdat, timeperiod = time_period,
                         org_code = org_code, denominator = denominator,
                         total= this_is_the_total, ho = ho,
                         co = co, org_type = "trust")[1,3],
    50000
  )
  expect_equal(
    # E17, 201415, total rate = (10 / 100) * 100000 = 10000
    ann_tab_onset_annual(ann_onset_testdat, timeperiod = time_period,
                         org_code = org_code, denominator = denominator,
                         total= this_is_the_total, ho = ho,
                         co = co, org_type = "ccg")[1,5],
    10000
  )
})
