context("Klebsiella species reporting")

test_that("Function handles non-recognised input as desired",{
  expect_equal(klebsiella_sp_group("An aardvark"), "This is not a known value")
})

test_that("Function handles expected input as desired", {
  expect_equal(klebsiella_sp_group(""), "Not speciated")
  expect_equal(klebsiella_sp_group("Klebsiella sp."), "Not speciated")
  expect_equal(klebsiella_sp_group("K. oxytoca"), "K. oxytoca")
  expect_equal(klebsiella_sp_group("Other Named"), "Other named species")
  expect_equal(klebsiella_sp_group("K. aerogenes"), "Other named species")
  expect_equal(klebsiella_sp_group("K. pneumoniae (incl. subspecies pnemoniae and ozenae)"),
                "K. pneumoniae")
})
