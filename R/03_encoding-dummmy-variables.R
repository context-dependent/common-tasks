library(tidyverse)


# GENERATE TEST DATA ------------------------------------------------------

dat <-

  tribble(

    ~ id, ~ gender, ~ disabled, ~ immigrant,
       1,   "MALE",      "Yes",          NA,
       2,   "MALE",       "No",          NA,
       3, "FEMALE",         NA,       "Yes",
       4,       NA,      "Yes",        "No",
       5,   "MALE",      "Yes",          NA,
       6, "FEMALE",         NA,       "Yes",
       7,       NA,       "No",        "No"

  )


# ENCODE DUMMY VARIABLES INDIVIDUALLY (USE THIS ONE) ---------------------

dat_00 <-

  dat %>%

    mutate(
      gender_dummy    = gender %in% "MALE",
      disabled_dummy  = disabled %in% "Yes",
      immigrant_dummy = immigrant %in% "Yes"
    )

# A tibble: 7 x 7
# id    gender disabled immigrant gender_dummy disabled_dummy immigrant_dummy
# <dbl> <chr>  <chr>    <chr>     <lgl>        <lgl>          <lgl>
# 1     MALE   Yes      NA        TRUE         TRUE           FALSE
# 2     MALE   No       NA        TRUE         FALSE          FALSE
# 3     FEMALE NA       Yes       FALSE        FALSE          TRUE
# 4     NA     Yes      No        FALSE        TRUE           FALSE
# 5     MALE   Yes      NA        TRUE         TRUE           FALSE
# 6     FEMALE NA       Yes       FALSE        FALSE          TRUE
# 7     NA     No       No        FALSE        FALSE          FALSE


# TRUE = 1, FALSE = 0, NUMERIC COMPARISONS AND STATS HOLD -----------------

tab_00 <-

  dat_00 %>%

    summarize(
      gender_dummy_max = max(gender_dummy),
      gender_dummy_mean = mean(gender_dummy),
      gender_dummy_total = sum(gender_dummy)
    )

# A tibble: 1 x 3
# gender_dummy_max gender_dummy_mean gender_dummy_total
# <int>            <dbl>             <int>
# 1                0.429             3


