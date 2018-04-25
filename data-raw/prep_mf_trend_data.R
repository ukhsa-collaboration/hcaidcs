mf_trend_data <-
  structure(
    list(
      t = structure(
        c(16436, 16467, 16495, 16526,
          16556, 16587,  16617, 16648,
          16679, 16709,  16740, 16770
        ), class = "Date"),
      year = c(2015L, 2015L, 2015L,
        2015L, 2015L, 2015L, 2015L,
        2015L, 2015L, 2015L, 2015L,
        2015L
      ),
      month = 1:12,
      cdi = c( 1129L, 1051L,  1218L,
        1164L, 1253L, 1235L, 1349L,
        1270L, 1392L, 1269L, 1173L,
        1092L),
      ecoli = c( 2865L, 2702L,
        2941L, 2924L, 3052L, 3193L,
        3373L, 3351L, 3363L, 3308L,
        3166L, 3077L),
      mrsa = c(79L, 65L, 78L, 76L, 73L, 60L, 55L, 86L, 61L, 70L, 65L, 68L),
      mssa = c( 874L, 743L,
        909L, 850L, 851L, 866L,
        891L, 856L, 876L, 887L,
        855L, 930L)),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA,-12L),
    .Names = c("t", "year", "month", "cdi", "ecoli", "mrsa",
               "mssa"))
devtools::use_data(mf_trend_data)
