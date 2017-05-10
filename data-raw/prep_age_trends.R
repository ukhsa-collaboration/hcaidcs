library(dplyr)
age_trends_data <- read.csv("K:/Annual Table Publication/Annual_Publication_FY 15_16/data/cdi_age_trends.csv",
                        stringsAsFactors = FALSE)
age_trends_data <- age_trends_data %>%
  select(fyear6, age_group_new, sex, n, popn, rate, age_sex_pc)

devtools::use_data(age_trends_data)
