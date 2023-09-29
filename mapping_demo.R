###########################################################
#######             Mapping with R:                 #######
#######             practice/ example               #######
#######                2023-10-05                   #######
#######               Micah & Emma                  #######                       
###########################################################

## loading & installing packages ----

  # list of packages required
  libs <- c("sf", "raster","terra",
             "sp","mapview", "ggmap",
             "tidyverse","ggplot2",
               "here")
  
  # installing and loading
  for (i in libs){
    if( !is.element(i, .packages(all.available = TRUE)) ) {
      install.packages(i)
    }
    library(i,character.only = TRUE)
  }
  
## loading data ----
  
  v_data <- read.csv(here::here("name of file.csv"))
  r_data <- raster(here::here("name of file.tif"))
    
## sf data handling ----
  
  
  
## raster data handling ----
  
  
  
  