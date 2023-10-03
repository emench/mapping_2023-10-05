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
              "here", "rgdal","rgeos",
              "elevatr")
  
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
  
  ## loading one raster at a time 
    # loading
    bio1_ssp126 <- raster::raster(here::here("ssp126_bioclim_1_.tif"))
    # defining datum
    proj4string(bio1_ssp126) <- CRS("+proj=longlat")
    # plotting
    plot(bio1_ssp126)
  
  ## loading multiple rasters as a stack 
    # to stack rasters they must have same extent
    bio <- raster::stack(list.files(here::here(), pattern = ".tif"))
    proj4string(bio) <- CRS("+proj=longlat")
    plot(bio)
    
  ## cropping to smaller study area / area of interest 
    # setting max and min coordinates
    max.lat <- 59
    min.lat <-45
    max.lon <- -113
    min.lon <- -130
    
  ## creating geographic extent object from these points
    geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, 
                                      max.lat))
  
  ## cropping rasters to study area 
    bio <- raster::crop(bio, geographic.extent)
    
  ## plotting cropped rasters
    plot(bio)
    
  ## slightly nicer plotting 
    
    names(bio) <- c("SSP126", "SSP585") # defining layer names 
    spplot(bio,
           main = "Annual Mean Temp (C)", 
           xlab="Longitude",ylab="Latitude", 
           scales = list(draw = TRUE))
    
  ## histograms of cell values
    hist(bio[[1]]) # [[1]] refers to the first layer in stack 
    hist(bio[[2]])
    
  ## Raster math 
    
    ## calculating difference between high
    # and low emissions climate change scenarios
    
      difference <- raster::overlay(bio[[1]],
                               bio[[2]], 
                               fun = function(r1, r2){r2-r1})
  
    ## plotting
    spplot(difference, 
           main = "Difference in annual Mean Temp (C) (SSP585-SSP126)", 
           xlab="Longitude",ylab="Latitude", 
           scales = list(draw = TRUE))
    
    ## Finding mean value for all pixels in 
    ## study area 
    mean_ann_temp_126 <- raster::cellStats(bio[[1]], "max")
    mean_ann_temp_585 <- raster::cellStats(bio[[2]], "max")
  
## BONUS: downloading DEM raster (digital elevation model) ----
  
  ## 1) Create an account with Open Topography
  # Request API key
  # input API key as character 
  API = ""
  elevatr::set_opentopo_key(API)
  
  ## 2) restart R session as requested
  
  ## 3) setting max and min coordinates
      # in a real example, you could find the max and min coordinates
      # in a dataset of occurrence points 
    max.lat <- 49.5
    min.lat <-48.2
    max.lon <- -122.1
    min.lon <- -124.5
    
  ## 4) creating geographic extent from these points
      # you could also add a buffer around these points 
      # by adding and subtracting a parameter e.g. 0.5 degrees 
      # from each coordinate
    
     geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, 
                                      max.lat))
    
  ## 5) converting to spatial polygons vector 
      area <- as(geographic.extent, 'SpatialPolygons')
      crs(area) <- CRS("+proj=longlat") # defining the projection
    
    
    ## this can be converted to a 
    ## shapefile (vector) (can be used in ArcGIS)
      # StudyExtent <- shapefile(area, 'StudyExtent.shp',overwrite=T)
      # zip("StudyExtent.zip",
      #     files = c("StudyExtent.dbf","StudyExtent.shx","StudyExtent.shp",
      #               "StudyExtent.prj"))
      
  ## 6) creating function to retrieve 30m DEM
      # parameters of the function can be changed to obtain different
      # resolution DEMs 
      
      demRetrieve <- function(extentShape){
      
      # using shapefile created before to retieve DEM from Open Topography
      # where extentShape = shapefile of geographic extent 
      
      DEM<- get_elev_raster(locations= extentShape,src="gl1",clip="bbox")
      NAvalue(DEM) <- -9999 # reassining NA value in format for ClimateNA
      plot(DEM) # plotting to check
      # saving for use in ClimateNA
      writeRaster(DEM, here::here("dem.asc"),
                  overwrite=T) 
      return(DEM)
      
    } 
    
    ## 7) Calling function to retrieve 30 m DEM
      # for study area
    dem <- demRetrieve(area)
    
    ## 8) basic plotting to check if it worked
    plot(dem) 
    
    ## loading back in 
    
    dem <- raster::raster(here::here("dem.asc"))
    crs(dem) <- CRS("+proj=longlat") 
    
    ## rasters are far easier to plot 
    # and define colours in ArcGIS
    
    library(colorRamps)
    
    # on the full scale, difficult to see low-lying areas
    spplot(dem, 
           at = seq(0,2000,5),
           maxpixels = 250000,
           col.regions = matlab.like2(n=2000))
    
    # only going up to 800 m improves this slightly 
    # but still difficult to see
    spplot(dem, 
           at = seq(0,800,5),
           maxpixels = 250000,
           col.regions = matlab.like2(n=2000))
  
    # in arcGIS, you can scale things differently so 
    # the low-lying areas would stand out
    
    # could also try to cut out the coastlines 
    # but you would have to find a high resolution 
    # vector file of coastlines to crop with 
    