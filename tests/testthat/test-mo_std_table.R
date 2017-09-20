context("Producing monthly tables")

test_that("The first month forms the first column in the output"{
  library(dplyr)
  data(monthly_ccg_data_raw)
  ccg_dat_all <- monthly_ccg_data_raw %>%
      rename(org_code = ccg_code)

  my_out_dat <- mo_std_table(data_fm = ccg_dat_all, collection = "MRSA",
                                column = "total_cases")
  expect_match(names(my_out_dat)[2], "July_2016")
})
