context("Convert_csv2r")

#Basic setup
save.image(file <- "temp_ge.RData")
original_dir <- getwd()
current_path <- system.file(package = "hcaidcs")
temp_dir <- tempdir()
file.copy(file.path(current_path, "test_convertcsv2rCCG.csv"), temp_dir, overwrite = T)
file.copy(file.path(current_path, "test_convertcsv2rTrust.csv"), temp_dir, overwrite = T)
setwd(temp_dir )

#Temp variables & files
Trust_file <- "test_convertcsv2rTrust.csv"
CCG_file <- "test_convertcsv2rCCG.csv"
compare_trust <- list(list(c("RCF", "RFF", "RAE"), "RCF"), list(c("RF4", "R1H"), "R1H"), list(c("RWY", "RJF", "RXQ", "RXH"), "RXH"))
compare_ccg <- list(list(c("07L", "07M", "07N"), "07L"), list(c("00T", "00R", "00Q"), "00Q"), list(c("02N", "02P"), "02P"))
hcaidcs::convert_csv2R(trust_file = Trust_file, ccg_file = CCG_file)

#Tests
testthat::test_that("Function works", {
  testthat::expect_equal(ccg_list, compare_ccg)
  testthat::expect_equal(trust_list, compare_trust)

})

#Clean and revert to original environment
setwd(original_dir)
remove(list = ls())
invisible(gc())
load("temp_ge.RData")
file.remove("temp_ge.RData")
