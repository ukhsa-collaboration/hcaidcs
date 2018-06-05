context("pir_org_type")

test_that("pir_org_type is vectorised", {
  expect_equal(pir_org_type(c("", "NHS Trust", "Clinical Commissioning Group",
                              "Total Reported Cases", NA)),
               factor(c(NA_character_, "Trust Assigned Cases",
                        "CCG Assigned Cases", "Total Reported Cases",
                        NA_character_),
                      levels = c("Total Reported Cases", "Trust Assigned Cases",
                                 "CCG Assigned Cases", "Third Party Cases")) )
})
