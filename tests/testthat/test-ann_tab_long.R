context("Tests for ann_tab_long")

# test_that("Assertions work", {
#   data("line_listing")
#   expect_error(ann_tab_long(dat = line_listing, collection_var = collection,
#                             collection_string = "C. difficile", org_code = reporting_organisation_code,
#                             metric_type = "aardvark", metric_var = prior_hc, output = "quarterly",
#                             spec_date = specimen_date))
#
#   expect_error(ann_tab_long(dat = line_listing, collection_var = collection,
#                             collection_string = "C. difficile", org_code = reporting_organisation_code,
#                             metric_type = "prior_hc", metric_var = prior_hc, output = "aardvark",
#                             spec_date = specimen_date))
# })

# Annual data ####
data("line_listing")
line_listing$fy_long <- hcaidcs::fy_long(line_listing$specimen_date)

test_that("Expansion works", {
  out <- line_listing %>% dplyr::filter(collection == "C. difficile") %>%
    ann_tab_long(dat = ., collection_var = collection, org_var = reporting_organisation_code,
                      metric_var = prior_hc, period_var = fy_long)
  expect_equal(sum(is.na(out$count_cases)), 0)

  # want all metric levels, organisations and time periods.
  # This no longer applies as no longer generate metric
  # expect_equal(out %>% dplyr::group_by(metric) %>%
  #                dplyr::filter(dplyr::row_number() == 1) %>% nrow(), 7 )
  #
  # # does onset status work?
  # out <- ann_tab_long(dat = line_listing, collection_var = collection,
  #                     collection_string = "C. difficile", org_code = reporting_organisation_code,
  #                     metric_type = "onset", metric_var = onset_status, output = "annual",
  #                     spec_date = specimen_date)
  # expect_equal(sum(is.na(out$count_cases)), 0)
  #
  # # want all metric levels, organisations and time periods.
  # expect_equal(out %>% dplyr::group_by(metric) %>%
  #                dplyr::filter(dplyr::row_number() == 1) %>% nrow(), 3 )


})

test_that("Summation works as expected", {
  line_listing$fy_six <- fy_six(line_listing$specimen_date)
  out <- line_listing %>% dplyr::filter(collection == "C. difficile") %>%
    ann_tab_long(dat = ., collection_var = collection, org_var = reporting_organisation_code,
                 metric_var = prior_hc, period_var = fy_six)

  expect_equal(
    out %>% dplyr::filter(metric == "Total cases" & as.character(period) == "201516" &
                            organisation == "E17") %>% dplyr::pull(count_cases),
    nrow(line_listing[line_listing$reporting_organisation_code == "E17" &
                        line_listing$collection == "C. difficile" &
                        line_listing$specimen_date >= lubridate::dmy("01-04-2015") &
                        line_listing$specimen_date <= lubridate::dmy("31-03-2016") ,])
               )

})
