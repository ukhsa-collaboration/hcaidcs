context("merge_everything")

#Basic setup
save.image(file <- "temp_ge.RData")
original_dir <- getwd()
current_path <- system.file(package = "hcaidcs")
temp_dir <- tempdir()
file.copy(file.path(current_path, "test-merge_everything_MRSA.xlsx"), temp_dir, overwrite = T)
file.copy(file.path(current_path, "test-merge_everything_NON_MRSA.xlsx"), temp_dir, overwrite = T)
setwd(temp_dir )

#Temp variables & files
MRSA_file <- "test-merge_everything_MRSA.xlsx"
NON_MRSA_file <- "test-merge_everything_NON-MRSA.xlsx"
trust_list <- list(list(c("RCF", "RFF", "RAE"), "RCF"), list(c("RF4", "R1H"), "R1H"), list(c("RWY", "RJF", "RXQ", "RXH"), "RXH"))
ccg_list <- list(list(c("07L", "07M", "07N"), "07L"), list(c("00T", "00R", "00Q"), "00Q"), list(c("02N", "02P"), "02P"))
merge_everything("test-merge_everything_NON_MRSA.xlsx", 1, trust_list, ccg_list, result_file = "NON_MRSA.xlsx")
merge_everything("test-merge_everything_NON_MRSA.xlsx", 1, result_file = "Error.xlsx")
merge_everything("test-merge_everything_MRSA.xlsx", 2, trust_list, ccg_list, result_file = "MRSA.xlsx")
non_mrsa_w2 <- openxlsx::read.xlsx("NON_MRSA.xlsx", sheet = 2, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)
non_mrsa_w3 <- openxlsx::read.xlsx("NON_MRSA.xlsx", sheet = 3, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)
non_mrsa_w4 <- openxlsx::read.xlsx("NON_MRSA.xlsx", sheet = 4, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)
non_mrsa_w5 <- openxlsx::read.xlsx("NON_MRSA.xlsx", sheet = 5, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)
mrsa_w3 <- openxlsx::read.xlsx("MRSA.xlsx", sheet = 3, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)
mrsa_w4 <- openxlsx::read.xlsx("MRSA.xlsx", sheet = 4, startRow = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)

#Tests - output files
test_that("Function creates files", {
  expect_true (file.exists("NON_MRSA.xlsx"), file.exists("MRSA.xlsx"))
})

test_that("Function does nothing when codes are null", {
  expect_true(!file.exists("Error.xlsx"))
})

test_that("Number of worksheets in workbook is correct", {
  expect_equal(length(openxlsx::getSheetNames("NON_MRSA.xlsx")), 5)
})

test_that("Correct number of rows/columns in output file", {
  expect_equal(dim(non_mrsa_w2), c(17, 6))
})
#Tests - processed information
test_that("Cell is correctly calculated for non-MRSA type w2", {
  expect_equal(as.numeric(non_mrsa_w2[6,3]), 29982)
})

test_that("Cell is correctly calculated for non-MRSA type w3", {
  expect_equal(as.numeric(non_mrsa_w3[5,5]), 100000)
})

test_that("Cell is correctly calculated for non-MRSA type w4", {
  expect_equal(as.numeric(non_mrsa_w4[9,5]), 10)
})

test_that("Cell is correctly calculated for non-MRSA type w5", {
  expect_equal(as.numeric(non_mrsa_w5[9,7]), 18.863275896583101)
})

test_that("Cell is correctly calculated for MRSA type w3", {
  expect_equal(as.numeric(mrsa_w3[7,7]), 58)
})

test_that("Cell is correctly calculated for MRSA type w4", {
  expect_equal(as.numeric(mrsa_w4[6,6]), 11.5)
})

#Clean and revert to original environment
setwd(original_dir)
remove(list = ls())
invisible(gc())
load("temp_ge.RData")
file.remove("temp_ge.RData")
