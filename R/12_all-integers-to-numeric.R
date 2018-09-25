library(tidyverse)


# GENERATE TEST DATA ------------------------------------------------------

dat_int <- 
  
  tibble(
    x_int = 1L:5L,
    y_int = 15L:19L
  )


dat_num <- 
  
  dat_int %>% 
  
    mutate_if(
      is.integer, 
      as.numeric
    )
