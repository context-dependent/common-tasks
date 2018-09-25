library(tidyverse)


# INITIALIZE TEST DATA ----------------------------------------------------

dat_long <- tribble(
  ~id, ~year, ~sex, ~inc, ~ue,
    1,    80,    0, 5000,   0,
    1,    81,    0, 5500,   1,
    1,    82,    0, 6000,   0,
    2,    80,    1, 2000,   1,
    2,    81,    1, 2200,   0,
    2,    82,    1, 3300,   0,
    3,    80,    0, 3000,   0,
    3,    81,    0, 2000,   0,
    3,    82,    0, 1000,   1 
)


# GATHER, UNITE, SPREAD ---------------------------------------------------

## go from long to wide for multiple value variables using unite

dat_wide_00 <- 
  
  dat_long %>% 
    gather(var, val, -id, -sex, -year) %>% 
    unite(var_year, var, year) %>% 
    spread(var_year, val)

## go back long data using separate

dat_reverse_00 <- 
  
  dat_wide_00 %>% 
    gather(var_year, val, -id, -sex) %>% 
    separate(var_year, into = c("var", "year")) %>% 
    spread(var, val)



# RESHAPE (BASE R) --------------------------------------------------------

## go from long to wide using reshape
dat_wide_01 <-
  
  reshape(
    as.data.frame(dat_long), 
    idvar = c("id", "sex"), 
    direction = "wide", 
    timevar = "year"
  )

## go back to long data using reshape
dat_reverse_01 <- 
  
  reshape(
    dat_wide_01,
    direction = "long", 
    timevar = "year"
  ) %>% 
  
  as_tibble()



# DCAST WITH DATA.TABLE ---------------------------------------------------

it's in this snippet, and also i added it to the 00_reshaping-in-R.R file in your project
basically you've got three arguments: data, formula, and value.var
in data, you pass the data you want to reshape, converting to a data.table on the fly if necessary, as in this example
in formula, specify the equivalent of stata reshape's j()
on the right hand side 
value.var takes a character vector of columns you want to reshape

## ... matches all columns that aren't otherwise mentioned
dat_wide_02 <- 
  
  data.table::dcast(
    data.table::setDT(dat_long), 
    formula = ... ~ year, 
    value.var = c("inc", "ue")
  )

## reverse using melt
dat_reverse_02 <- 
  
  data.table::melt(
    dat_wide_02, 
    measure = patterns("inc_", "ue_"),
    value.names = c("inc", "ue"), 
    variable.name = "year"
  )

