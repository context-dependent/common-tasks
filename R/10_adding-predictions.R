
library(tidyverse)

# GET TEST DATA -----------------------------------------------------------


dat <- mpg



# FIT THE MODEL -----------------------------------------------------------

fit_mpg <- lm(cty ~ cyl + displ + year, data = dat)



# ADD PREDICTIONS ---------------------------------------------------------

dat_fit <- 
  
  dat %>% 
    
    mutate(
      predictions = predict(fit_mpg, dat)
    )
