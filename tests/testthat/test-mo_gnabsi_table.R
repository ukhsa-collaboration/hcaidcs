context("Producing monthly gnabsi tables")

test_that("Function throws an error if wrong org type specified", {
  expect_error(mo_gnabsi_table(ccg_dat_all, "E. coli", org_type = "Aardvark"))
})

test_that("The first month forms the first column in the output", {
  library(dplyr)
  data(monthly_ccg_data_raw)
  ccg_dat_all <- monthly_ccg_data_raw %>%
      rename(org_code = ccg_code, ho = apportioned) %>%
      mutate(data_collection = "E. coli", co = total_cases - ho)

  my_out_dat <- mo_gnabsi_table(data_fm = ccg_dat_all, collection = "E. coli",
      org_type = "ccg")
  expect_match(names(my_out_dat)[2], "July_2016_total_cases")
})
