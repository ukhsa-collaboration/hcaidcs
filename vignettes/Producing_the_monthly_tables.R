## ----setup, echo=FALSE---------------------------------------------------
library(hcaidcs)

## ---- message=FALSE------------------------------------------------------
library(lubridate)
mo_curr_months <- function(x){
  x <- x - years(1)
  mo_df <- data.frame(mo_seq = seq(1, 13, 1), 
                      mo_now = lubridate::rollback(x, roll_to_first = TRUE))
  mo_df$first_day_text <- mo_df$mo_now %m+% months(mo_df$mo_seq - 1)
  mo_df$the_month <- paste(as.character(month(mo_df$first_day_text, label = TRUE, abbr = FALSE)), 
                           year(mo_df$first_day_text))
  mo_df$the_month <- factor(mo_df$the_month, levels = as.ordered(mo_df$the_month))
  the_months <- mo_df$the_month
  return(the_months)
}

the_months <- mo_curr_months(today())

## ------------------------------------------------------------------------
mo_curr_months(dmy("01/02/2015"))

## ------------------------------------------------------------------------
mo_pub_date()

