context("Check that aec_ec_primary_focus works")
# tips for testing ggplot2 functions: https://stackoverflow.com/questions/31038709/how-to-write-a-test-for-a-ggplot-plot


test_that("aec_ec_primary_focus returns a ggplot object", {
  test_dat <- structure(list(
    tto_group = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L,
                            3L, 3L, 3L, 3L, 3L),
                          .Label = c("<2", "2-6", "ge7"), class = "factor"),
    primary_focus = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 1L,
                                2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 6L),
                              .Label = c("gastro", "hepatobil", "other",
                                         "respiratory", "unknown", "uti"),
                              class = "factor"),
    pc = c(20, 20, 20, 20, 10, 10,
           30, 20, 10, 10, 10, 20, 50, 10, 10, 10, 10,
           10)), .Names = c("tto_group", "primary_focus", "pc"),
    class = "data.frame", row.names = c(NA, -18L))

  p <- aec_ec_source_plot(test_dat)
  expect_is(p, c("gg", "ggplot"))
})
