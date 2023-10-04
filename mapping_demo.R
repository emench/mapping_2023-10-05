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
              "elevatr","rnaturalearth","ggmap")
  
  # installing and loading
  for (i in libs){
    if( !is.element(i, .packages(all.available = TRUE)) ) {
      install.packages(i)
    }
    library(i,character.only = TRUE)
  }

#################################################################################### 
   
## Working with VECTOR data ----
  
  albicans <- read.csv(file = './Asclepias_albicans.csv')
    
  head(albicans) #note: this file consists of coordinates for A. albicans, downloaded from GBIF and cleaned using the CoordinateCleaner package and manual removal of some anomalous records
  
## First we need to import our data layers
  
  world_map = ne_countries(scale = "medium", returnclass = "sf", type = "countries") #background map for first pass: include all of North America
  
  lakes50 <- ne_download(scale = 50, type = "lakes", category = "physical", returnclass = 'sf') #include a layer for major lakes
  
  rivers50 <- ne_download(scale = 50, type = "rivers_lake_centerlines", category = "physical", returnclass = 'sf') #include a layer for major rivers
  
  glaciated_areas50 <- ne_download(scale = 50, type = "glaciated_areas", category = "physical", returnclass = 'sf') #include a layer for glaciated areas
  
  states <- ne_states(returnclass = 'sf') #include a layer for state boundaries
  
#lets create a basic map just showing some of the above layers, cropping to focus on North America
  
  ggplot() + 
    geom_sf(data = world_map)+
    geom_sf(data = lakes50, fill = "lightcyan")+
    geom_sf(data = states, fill = 'ivory')+
    geom_sf(data = glaciated_areas50, fill = 'hotpink')+
    theme(panel.background = element_rect(fill='lightcyan'))+
    coord_sf(xlim = c(-170, -50), ylim = c(0, 70))
  
#now let's add the coordinates for our species of interest
  
  coords <- albicans[,c("decimalLongitude","decimalLatitude")] #pick out the columns with coordinates
  
  #creating a static map with data layers
  
  (albicans_base_map <- ggplot() + 
    geom_sf(data = world_map)+
    geom_sf(data = states, col = 'black')+
    geom_sf(data = lakes50, fill = "lightcyan")+
    geom_sf(data = rivers50, col = 'darkblue')+
    theme(panel.background = element_rect(fill='lightcyan'))+
    geom_point(data = as.data.frame(coords), aes(x = coords[, 1], y = coords[, 2]), color = "darkgreen")+
    xlim(c(range(coords$decimalLongitude)[1] - 2, range(coords$decimalLongitude)[2] + 2))+
    ylim(c(range(coords$decimalLatitude)[1] - 2, range(coords$decimalLatitude)[2] + 2))+
    ylab('Latitude') + xlab('Longitude'))
  
#next, let's create a basic convex hull polygon defining the approximate boundaries of the species range
  
  convex_hull <- chull(coords) #creates a vector defining the vertices of the polygon
  
  hull_coords <- data.frame(coords[c(convex_hull, convex_hull[1]), ]) #assigns coordinates to each vertex
  
  albicans_base_map +
    geom_polygon(data = hull_coords, aes(x = decimalLongitude, y = decimalLatitude), 
                 fill = "lightgreen", color = "darkgrey", alpha = 0.3)
  
  ## This looks fine, but what if we want to omit the areas where we know this species does not occur (e.g. open ocean and lakes)
  
  ocean <- ne_download(scale = 50, type = 'ocean', category = 'physical', returnclass = 'sf')
  
  ocean #note that when we examine this object, it is of class 'multipolygon' and has the WGS 84 coordinate reference system
  
  points_sf <- st_as_sf(coords, coords = c("decimalLongitude","decimalLatitude")) #this time, use a different approach for defining the convex hull. First, create an SF object based on coordinates (geometry type = point)
  
  convex_hull <- st_convex_hull(st_union(points_sf)) #convert the points object into a polygon object
  
  hull <- convex_hull %>% st_set_crs(st_crs(ocean)) #set the coordinate reference system to be the same between each layer; otherwise any spatial transform operation will throw an error
  
  pared_range <- st_intersection(ocean, hull) #create a new multipolygon object defined by the area where the ocean and hull layers do not overlap
  
  ggplot() + 
    geom_sf(data = world_map)+
    geom_sf(data = states, col = 'black')+
    geom_sf(data = lakes50, fill = "lightcyan")+
    theme(panel.background = element_rect(fill='lightcyan'))+
    xlim(c(range(coords$decimalLongitude)[1] - 2, range(coords$decimalLongitude)[2] + 2))+
    ylim(c(range(coords$decimalLatitude)[1] - 2, range(coords$decimalLatitude)[2] + 2))+
    geom_sf(data = pared_range, fill = 'lightgreen', color = 'darkgrey', alpha = 0.7)
    
#calculate the area of this resulting object

st_area(pared_range) #defaults to area in m^2

as.numeric(st_area(pared_range)) / 1e7 #convert to km^2

#Cheatsheet for spatial transform operations in sf: https://github.com/rstudio/cheatsheets/blob/main/sf.pdf
  
### A quick demo of using different coordinate reference systems and projections

canada <- world_map[world_map$sovereignt == 'Canada',] #first select only Canada for demonstration purposes
provinces <- states[states$sov_a3 == 'CAN',] #pick out provincial borders

(base_canada_map <- ggplot()+
  geom_sf(data = canada)+
  geom_sf(data = provinces)+
  theme(panel.background = element_rect(fill='lightcyan'))) #create basic map of Canada and its provinces

base_canada_map+
  coord_sf(crs = st_crs(4326)) #standard WGS 84 coordinate system (same as above, this is the default)

base_canada_map+
  coord_sf(crs = st_crs(3347)) #Lambert projection

base_canada_map+
  coord_sf(crs = st_crs(3857)) #Mercator projection

base_canada_map+
  coord_sf(crs = "+proj=robin +lon_0=0w") #Robinson projection

### Custom shape files

#Downloaded shape file for Rocky Mountain National Park here: https://romo-nps.opendata.arcgis.com/datasets/7cb5f22df8c44900a9f6632adb5f96a5/explore?location=40.296464%2C-105.702647%2C7.00

rmnp <- read_sf('./Rocky_Mountain_National_Park_-_Boundary_Polygon/Boundary__Polygon_.shp')

colorado <- states[states$name == 'Colorado',]

ggplot()+
  geom_sf(data = colorado, fill = 'ivory')+
  geom_sf(data = rmnp, fill = 'darkgreen')

#using mapView for interactive plotting of coordinates

coords

Asclepias_ablicans <- st_as_sf(coords, coords = c('decimalLongitude','decimalLatitude'))

st_crs(Asclepias_ablicans) <- 4326 #assigns WGS84 coordinate system to sf object

mapView(Asclepias_ablicans)

#using ggmap to create static maps that utilize Google Maps 

register_google(key = "api key here-M")  #note, you must register for an API key through Google: https://support.google.com/googleapi/answer/6158862?hl=en

albicans_map_satellite <- get_map("Ensenada, Baja California, Mexico", zoom=5, maptype="satellite", source="google")

ggmap(albicans_map_satellite)+
  theme_bw()+
  geom_point(data = as.data.frame(coords), aes(x = coords[, 1], y = coords[, 2]), color = "yellow", size = 1)+
  theme(axis.text = element_blank(), axis.title = element_blank())

vancouver_map_terrain <- get_map("Vancouver, BC, Canada", zoom=11, maptype="terrain", source="google")

ggmap(vancouver_map_terrain)+
  theme_bw()+
  theme(axis.text = element_blank(), axis.title = element_blank())

#ggmap package also includes the potentially handy 'geocode' function

golf_course <- geocode(location = 'university golf club, ubc, vancouver')

ggmap(vancouver_map_terrain)+
  theme_bw()+
  theme(axis.text = element_blank(), axis.title = element_blank())+
  geom_point(data = data.frame(golf_course), aes(x = lon, y = lat), col = 'hotpink', size = 5)

#################################################################################### 
  
  ## Working with RASTER data ----
  
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
    
    ## saving raster we created
    # can save as a .asc or .tif extension
    # .tif is preferred (newer & more efficient)
    writeRaster(difference, here::here("difference.tif"))
  
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
    