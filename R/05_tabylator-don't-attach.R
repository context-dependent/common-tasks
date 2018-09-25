
# YOUR FIRST FUNCTION FACTORY ---------------------------------------------

tabylator <- function(x) {
  
  q <- enquo(x)
  n <- quo_name(q)
  
  print(q)
  print(n)
  
  function(...) { 
    
    n %>% 
      
      get(envir = .GlobalEnv) %>% 
      janitor::tabyl(...)
  }
  
}


# USE IT TO MAKE A FUNCTION FOR TABULATING IN MPG -------------------------

tmpg <- tabylator(mpg)



# USE THE FUNCTION YOU JUST MADE TO TABULATE WITHOUT THE DATAFRAME --------

mpg %>% tabyl(cyl, trans)
tmpg(cyl, trans)
tmpg(year, cyl)

# pulling the dataframe from the global environment means 
# that tabyls will reflect changes made to the object 

mpg <- 
  
  mpg %>% 
  
    mutate(
      cyl_over_4 = cyl > 4
    )

tmpg(cyl, cyl_over_4)
