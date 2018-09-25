library(tidyverse)
library(lubridate)



# GENERATE TEST DATA ------------------------------------------------------

dat <-

  tibble(
    test_date = x <- ymd(c("2012-03-26", "2012-05-04", "2012-09-23", "2012-12-31", "2013-09-30"))
  )




# CALCULATE FISCAL YEAR  --------------------------------------------------

dat_00 <-

  dat %>%

    mutate(
      fyear = ifelse(
        # If the month of the date is after march
        month(test_date) > 3,
        # Use the year after the year of the test_date
        year(test_date) + 1,
        # Otherwise, if the month is in march or before,
        # use the year of the test_date
        year(test_date)
      )
    )
