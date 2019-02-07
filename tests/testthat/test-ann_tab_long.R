context("Tests for ann_tab_long")

test_that("Assertions work", {
  data("line_listing")
  expect_error(ann_tab_long(dat = line_listing, collection_var = collection,
                            collection_string = "C. difficile", org_code = reporting_organisation_code,
                            metric_type = "aardvark", metric_var = prior_hc, output = "quarterly",
                            spec_date = specimen_date))

  expect_error(ann_tab_long(dat = line_listing, collection_var = collection,
                            collection_string = "C. difficile", org_code = reporting_organisation_code,
                            metric_type = "prior_hc", metric_var = prior_hc, output = "aardvark",
                            spec_date = specimen_date))
})

# Annual data ####

test_that("Expansion works", {
  out <- ann_tab_long(dat = line_listing, collection_var = collection,
                      collection_string = "C. difficile", org_code = reporting_organisation_code,
                      metric_type = "prior_hc", metric_var = prior_hc, output = "annual",
                      spec_date = specimen_date)
  expect_equal(sum(is.na(out$count_cases)), 0)

  # want all metric levels, organisations and time periods.
  expect_equal(out %>% dplyr::group_by(metric) %>%
                 dplyr::filter(dplyr::row_number() == 1) %>% nrow(), 7 )

  # does onset status work?
  out <- ann_tab_long(dat = line_listing, collection_var = collection,
                      collection_string = "C. difficile", org_code = reporting_organisation_code,
                      metric_type = "onset", metric_var = onset_status, output = "annual",
                      spec_date = specimen_date)
  expect_equal(sum(is.na(out$count_cases)), 0)

  # want all metric levels, organisations and time periods.
  expect_equal(out %>% dplyr::group_by(metric) %>%
                 dplyr::filter(dplyr::row_number() == 1) %>% nrow(), 3 )


})

test_that("Summation works as expected", {
  out <- ann_tab_long(dat = line_listing, collection_var = collection,
                      collection_string = "C. difficile", org_code = reporting_organisation_code,
                      metric_type = "prior_hc", metric_var = prior_hc, output = "annual",
                      spec_date = specimen_date)
  expect_equal(
    out %>% dplyr::filter(metric == "Total cases" & as.character(period) == "201516" &
                            organisation == "E17") %>% dplyr::pull(count_cases),
    nrow(line_listing[line_listing$reporting_organisation_code == "E17" &
                        line_listing$collection == "C. difficile" &
                        line_listing$specimen_date >= lubridate::dmy("01-04-2015") &
                        line_listing$specimen_date <= lubridate::dmy("31-03-2016") ,])
               )

})

# Quarterly data ####
test_that("Expansion works", {
  out <- ann_tab_long(dat = line_listing, collection_var = collection,
                      collection_string = "C. difficile", org_code = reporting_organisation_code,
                      metric_type = "prior_hc", metric_var = prior_hc, output = "quarterly",
                      spec_date = specimen_date)
  expect_equal(sum(is.na(out$count_cases)), 0)

  # want all metric levels, organisations and time periods.
  expect_equal(out %>% dplyr::group_by(metric) %>%
                 dplyr::filter(dplyr::row_number() == 1) %>% nrow(), 7 )

  # does onset status work?
  out <- ann_tab_long(dat = line_listing, collection_var = collection,
                      collection_string = "C. difficile", org_code = reporting_organisation_code,
                      metric_type = "onset", metric_var = onset_status, output = "quarterly",
                      spec_date = specimen_date)
  expect_equal(sum(is.na(out$count_cases)), 0)

  # want all metric levels, organisations and time periods.
  expect_equal(out %>% dplyr::group_by(metric) %>%
                 dplyr::filter(dplyr::row_number() == 1) %>% nrow(), 3 )


})

test_that("Summation works as expected", {
  out <- ann_tab_long(dat = line_listing, collection_var = collection,
                      collection_string = "C. difficile", org_code = reporting_organisation_code,
                      metric_type = "prior_hc", metric_var = prior_hc, output = "quarterly",
                      spec_date = specimen_date)
  expect_equal(
    out %>% dplyr::filter(metric == "Total cases" &
                            as.character(period) == "April to June 2015" &
                            organisation == "E17") %>% dplyr::pull(count_cases),
    nrow(line_listing[line_listing$reporting_organisation_code == "E17" &
                        line_listing$collection == "C. difficile" &
                        line_listing$specimen_date >= lubridate::dmy("01-04-2015") &
                        line_listing$specimen_date <= lubridate::dmy("30-06-2015") ,])
  )

})

# test bug with line listings where E. coli has a trailing space

test_that("Trailing white space problem has been addressed",{
          line_listing$collection[line_listing$collection == "E. coli"] <- "E. coli "

          out <- ann_tab_long(dat = line_listing, collection_var = collection,
                              collection_string = "E. coli",
                              org_code = reporting_organisation_code,
                              metric_type = "prior_hc", metric_var = prior_hc,
                              output = "quarterly",
                              spec_date = specimen_date)
          expect_equal(
            out %>% dplyr::filter(metric == "Total cases" &
                                    as.character(period) == "April to June 2015" &
                                    organisation == "E17") %>% dplyr::pull(count_cases),
            nrow(line_listing[line_listing$reporting_organisation_code == "E17" &
                                line_listing$collection == "E. coli " &
                                line_listing$specimen_date >= lubridate::dmy("01-04-2015") &
                                line_listing$specimen_date <= lubridate::dmy("30-06-2015") ,]))
          }
)
