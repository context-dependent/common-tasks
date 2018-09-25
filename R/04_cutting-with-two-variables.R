library(tidyverse)



# GENERATE TEST DATA ------------------------------------------------------

dat <-

  tribble(

    ~ id, ~ immigrant, ~ yrsinland,
       1,        TRUE,           3,
       2,       FALSE,          NA,
       3,       FALSE,          NA,
       4,        TRUE,          10,
       5,        TRUE,           5,
       6,       FALSE,          NA,
       7,       FALSE,          NA

  )



# CUT VALUES AND FILL NA -------------------------------------------------------------

dat_00 <-

  dat %>%

    mutate(
      yrsinlandcat_00 = cut(
        yrsinland,
        c(0, 1, 2, 4, 9, 100),
        c("<1yr", "1-3yr", "3-4yr", "5-9yr", "10yr+")
      ) %>%
      # set a factor level for missing vlaues in a factor
      fct_explicit_na(
        "Not immigrant"
      )
    )


# TREAT THE NUMERIC VARIABLE FIRST TO WORK WITH THE CUT -------------------------

dat_01 <-

  dat %>%

    mutate(
      yrsinlandcat_01 = cut(
        # IF
        ifelse(
          # immigrant evaluates to TRUE
          immigrant,
          # cut the given value from yrsinland
          yrsinland,
          # otherwise, use 999
          999
        ),
        # additional break and corresponding category which uses the 999 values
        # to label a person a non immigrant.
        c(0, 1, 2, 4, 9, 100, 999),
        c("<1yr", "1-3yr", "3-4yr", "5-9yr", "10yr+", "Not immigrant")
      )
    )



# USE CASE WHEN TO MAKE A CHARACTER VARIABLE, THEN CONVERT TO FCT ---------
# this is the most typing intensive, but also the most flexible approach
# you can incorporate any number of variables from the data frame into
# logical assersions (left hand side) which correspond to values
# (right hand side)

dat_02 <-

  dat %>%

    mutate(
      # case_when tests assertions (eg. !immigrant) sequentially, assigning
      # the value corresponding to the first test passed
      yrsinlandcat_02 = case_when(
        # A: IF NOT immigrant THEN "Not Immigrant"
        !immigrant      ~ "Not immigrant",
        # IF immigrant (NOT A) AND
        # B: yrsinland < 1 THEN yrsinlandcat = "<1yr"
        yrsinland < 1   ~ "<1yr",
        # IF NOT A AND NOT B AND
        # C: yrsinland < 3 THEN yrsinlandcat  = "1-2yr"
        yrsinland < 3   ~ "1-2yr",
        # IF NOT A AND NOT B AND NOT C AND
        # D: yrsinland < 5 THEN yrsinlandcat = "3-4yr"
        yrsinland < 5   ~ "3-4yr",
        # IF NOT A AND NOT B AND NOT C AND NOT D AND
        # E: yrsinland < 10 THEN yrsinlandcat = "5-9yr"
        yrsinland < 10  ~ "5-9yr",
        # IF NOT A AND NOT B AND NOT C AND NOT D AND NOT E AND
        # F: yrsinland < 100 THEN yrsinlandcat = "10yr+"
        yrsinland < 100 ~ "10yr+",
        # TRUE ~ <y> assigns the value <y> if all of the above tests
        # have returned false.
        # It baseically means, "Otherwise..."
        TRUE ~ "Missing"
      ) %>%

      # coerce a character ariable to a factor, and set the order of the levels
      fct_relevel(
        "<1yr", "1-3yr", "3-4yr", "5-9yr", "10yr+",
        "Not immigrant"
      )
    )


# THESE ALL PRODUCE THE SAME RESULTS (DON'T WORRY ABOUT THIS CODE RIGHT NOW) ---

dat_03_comp <-

  ls(pattern = "dat_\\d\\d") %>%
  map(
  ~ get(.x) %>%
    select(-immigrant, -yrsinland)
  ) %>%
  reduce(
  ~ left_join(.x, .y, by = "id")
  )

# A tibble: 7 x 4
# id    yrsinlandcat_00 yrsinlandcat_01 yrsinlandcat_02
# <dbl> <fct>           <fct>           <fct>
# 1     3-4yr           3-4yr           3-4yr
# 2     Not immigrant   Not immigrant   Not immigrant
# 3     Not immigrant   Not immigrant   Not immigrant
# 4     10yr+           10yr+           10yr+
# 5     5-9yr           5-9yr           5-9yr
# 6     Not immigrant   Not immigrant   Not immigrant
# 7     Not immigrant   Not immigrant   Not immigrant

