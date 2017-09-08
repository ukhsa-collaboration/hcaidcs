context("Producing monthly gnabsi tables")

test_that("Function throws an error if wrong org type specified", {
  expect_error(mo_gnabsi_table(ccg_dat_all, "E. coli", org_type = "Aardvark"))
})
