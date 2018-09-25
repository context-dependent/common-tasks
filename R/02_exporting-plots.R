library(tidyverse)


dat <- mpg


p <- 
  
  dat %>% 
    
    ggplot(
      aes(
        hwy, 
        cty
      )
    ) + 
  
    geom_point() + 
    
    facet_wrap(
      ~ year, 
      ncol = 1
    )


ggsave(
  # replace the ling below with the path and file name where you want to save 
  # the plot
  "output/fig/mpg-test-plot.png",
  
  # plot will default to the last plot generated, but it's best to specify 
  plot = p,
  
  # you can export without specifying dimensions, but it's always best to do so
  width = 7.5, 
  height = 10, 
  units = "in",
  
  # by default, R for windows makes non-anti-aliased pngs. Use type = "cairo-png"
  # to ensure that your plots look smooth 
  type = "cairo-png"
)
