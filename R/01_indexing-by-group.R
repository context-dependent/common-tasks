library(tidyverse)


# INITIALIZE TEST DATA ----------------------------------------------------

dat <- tribble(
  ~id, ~month, 
    1,	  456,
    1,	  500,
    1,	  600,
    2,	  123,
    2,	  444,
    2,	  555,
    3,	  125,
    3,	  130
)


# GENERATE J --------------------------------------------------------------

dat_j <- 
  
  dat %>% 
    group_by(id) %>% 
    arrange(month) %>% 
    mutate(
      j = row_number()
    ) %>% 
    ungroup()


# GENERATE N --------------------------------------------------------------

dat_N <- 
  
  dat %>% 
    group_by(id) %>% 
    mutate(
      N = n()
    ) %>% 
    ungroup()


