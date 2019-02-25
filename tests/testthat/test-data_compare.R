context("Testing data_compare")

#Basic setup
save.image(file <- "temp_ge.RData")
original_dir <- getwd()
current_path <- system.file(package = "hcaidcs")
temp_dir <- tempdir()
file.copy(file.path(current_path, "test-merge_everything_NON_MRSA_new.xlsx"), temp_dir, overwrite = T)
file.copy(file.path(current_path, "test-merge_everything_NON_MRSA.xlsx"), temp_dir, overwrite = T)
setwd(temp_dir )

#Data_compare tests
df1 <- data.frame(replicate(3, 1:3))
df2 <- data.frame(replicate(2, 1:3), 2:4)
colnames(df2) <- c("X1", "X2", "X3")
df3 <- data.frame(replicate(4,sample(0:1,4,rep=TRUE)))

testthat::test_that("Data_compare output is matrix", {
  testthat::expect_true(is.matrix(data_compare(df1, df2)))
})

testthat::test_that("Data_compare output has correct dimension", {
  testthat::expect_equal(dim(data_compare(df1, df2)), c(3,2))
})

testthat::test_that("Data_compare doesn't work when data frames have different dimensions", {
  testthat::expect_error(data_compare(df1, df3))
})

testthat::test_that("Data_compare highlight correct cell", {
  testthat::expect_equal(unname(data_compare(df1, df2)[2,]), c(2:3))

})


context ("Testing highlight")

highlight(old_file = "test-merge_everything_NON_MRSA.xlsx", new_file = "test-merge_everything_NON_MRSA_new.xlsx", result_file = "results.xlsx")

testthat::test_that("Hihglight creates output file", {
  testthat::expect_true(file.exists("results.xlsx"))
})

results = openxlsx::loadWorkbook("results.xlsx")

testthat::test_that("Output file for highlight function has the correct number of styles", {
  testthat::expect_equal(length(openxlsx::getStyles(results)), 46)
})

#Clean and revert to original environment
setwd(original_dir)
remove(list = ls())
invisible(gc())
load("temp_ge.RData")
file.remove("temp_ge.RData")

