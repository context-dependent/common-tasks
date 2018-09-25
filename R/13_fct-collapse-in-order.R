
library(tidyverse)

# TEST DATA ---------------------------------------------------------------

dat <- mpg



# COLLAPSE THEN RELEVEL ---------------------------------------------------

dat00 <-

  dat %>%

    mutate(
      drv_not_f = fct_collapse(
          drv,
          "f" = "f",
          "not_f" = c("4", "r")
        ) %>%

        fct_relevel("f", "not_f")


    )



# FUNCTIONALIZE -----------------------------------------------------------

fct_collapse_inorder <- function(x, ...) {

  g <- quos(...)

  x %>% fct_collapse(...) %>% fct_relevel(!!!names(g))


}


# IMPLEMENT ---------------------------------------------------------------

dat01 <-

  dat %>%

    mutate(
      drv_not_f = fct_collapse_inorder(
          drv,
          "f" = "f",
          "not_f" = c("4", "r")
      )
    )
