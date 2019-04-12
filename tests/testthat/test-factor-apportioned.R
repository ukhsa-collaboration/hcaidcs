context("Test creation of factors")

test_that("factor_apportioned works", {
  expect_equal(factor_apportioned(1),
               factor("Hospital-onset",
                      levels = c("Hospital-onset", "Community-onset", "Total cases")))
  expect_equal(factor_apportioned("HO"),
               factor("Hospital-onset",
                      levels = c("Hospital-onset", "Community-onset", "Total cases")))
  expect_equal(factor_apportioned(0),
               factor("Community-onset",
                      levels = c("Hospital-onset", "Community-onset", "Total cases")))
  expect_equal(factor_apportioned("CO"),
               factor("Community-onset",
                      levels = c("Hospital-onset", "Community-onset", "Total cases")))

})

test_that("assertions work", {
  expect_error(factor_apportioned("Aardvark"))
})

test_that("factor_prior_hc works", {
  expect_equal(factor_prior_hc("hoha"),
               factor("Hospital-onset, healthcare associated",
                      levels = c("Hospital-onset, healthcare associated",
                                          "Community-onset, healthcare associated",
                                          "Community-onset, indeterminate association",
                                          "Community-onset, community associated",
                                          "Unknown 3 months", "No information", "Total cases")
                      )
               )

  expect_equal(factor_prior_hc("coha"),
               factor("Community-onset, healthcare associated",
                      levels = c("Hospital-onset, healthcare associated",
                                          "Community-onset, healthcare associated",
                                          "Community-onset, indeterminate association",
                                          "Community-onset, community associated",
                                          "Unknown 3 months", "No information", "Total cases")
               )
  )

  expect_equal(factor_prior_hc("coia"),
               factor("Community-onset, indeterminate association",
                      levels = c("Hospital-onset, healthcare associated",
                                          "Community-onset, healthcare associated",
                                          "Community-onset, indeterminate association",
                                          "Community-onset, community associated",
                                          "Unknown 3 months", "No information", "Total cases")
               )
  )

  expect_equal(factor_prior_hc("coca"),
               factor("Community-onset, community associated",
                      levels = c("Hospital-onset, healthcare associated",
                                          "Community-onset, healthcare associated",
                                          "Community-onset, indeterminate association",
                                          "Community-onset, community associated",
                                          "Unknown 3 months", "No information", "Total cases")
               )
  )

  expect_equal(factor_prior_hc("unknown_3_mo"),
               factor("Unknown 3 months",
                      levels = c("Hospital-onset, healthcare associated",
                                          "Community-onset, healthcare associated",
                                          "Community-onset, indeterminate association",
                                          "Community-onset, community associated",
                                          "Unknown 3 months", "No information", "Total cases")
               )
  )

  expect_equal(factor_prior_hc("all_blank"),
               factor("No information",
                      levels = c("Hospital-onset, healthcare associated",
                                          "Community-onset, healthcare associated",
                                          "Community-onset, indeterminate association",
                                          "Community-onset, community associated",
                                          "Unknown 3 months", "No information", "Total cases")
               )
  )
})

test_that("assertions work", {
  expect_error(factor_prior_hc("Aardvark"))
})
