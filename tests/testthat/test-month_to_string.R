context("Converting numeric months to string months")

test_that("cmonth_to_string returns expected results", {
  expect_match(cmonth_to_string(1), "January")
  expect_match(cmonth_to_string(12), "December")
  expect_equal(cmonth_to_string(13), as.character(NA))
})

test_that("fmonth_to_string returns expected results", {
  expect_match(fmonth_to_string(1), "April")
  # expect_match(fmonth_to_string(12), "January") # Why did I want 12 to return January? It should be March right?
  expect_match(fmonth_to_string(12), "March")#' fmonth_to_string(12)
  expect_equal(fmonth_to_string(13), as.character(NA))
})

test_that("fmonth_to_string works in a mutate chain", {
  out <- tibble::as_tibble(data.frame(x = c("April", "March"), stringsAsFactors = FALSE))
  in_df <- tibble::as_tibble(data.frame(x = c(1.0, 12.0)))
  expect_equal(in_df %>% dplyr::mutate(x = fmonth_to_string(x)),
               out)
})
