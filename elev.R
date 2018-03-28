### ### ### ### ### ### ### ### ### ### ### ### ##
###  EXTRACT ELEVATIONS FOR ALL ELK LOCATIONS  ###
### 	  	 Kristin Barker  Nov 2017	      	   ###
###   OH AND ACROSS THE STUDY AREA Mar 2018    ###
### ### ### ### ### ### ### ### ### ### ### ### ##



#### Working directory ####
setwd("C:/Users/kristin/Documents/Migration/DEMs")


#### Packages ####
library(raster)
library(dplyr)


#### Download, unzip, and store DEMs ####

dat <- read.csv("filelist.csv") # exported from national map viewer
urllist <- as.character(dat$URL)

for (i in 1:length(urllist)) {
  web <- urllist[i]
  temp <- tempfile()
  download.file(web, temp, mode = "wb")
  a <- unzip(temp)
  
}

#### Combine all DEMs and store as 1 geotiff ####

dems <- list.files(".", "_13$") # list all DEMs
mosaic_rasters(gdalfile = dems,
               dst_dataset = "../alldems.tif",
               of = "GTiff")
ras <- raster("../alldems.tif")
plot(ras)


#### Extract elevation associated with eack elk location ####

locs <- read.csv("../locs-allcows.csv")
locs.sp <- spTransform(SpatialPointsDataFrame(data.frame("x"=locs$Longitude, "y"=locs$Latitude),
                       locs, proj4string = CRS("+init=epsg:4326")), ras@crs)
elevs <- as.data.frame(extract(ras, locs.sp))
colnames(elevs) <- "Elev"
locelevs <- cbind(locs, elevs) %>%
  within(Sex <- "Female") %>%
  dplyr::select(AnimalID, Herd, DateTime, Latitude, Longitude, Sex, Elev)
write.csv(locelevs, "../locs-allcows-withelevs.csv", row.names = FALSE)



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

### ###  ### ### ###
#### STUDY AREA ####
### ###  ### ### ###


  # goal: determine min and max elev, temp, and precip across western MT study area #


  #### misc prelim ####

    memory.limit(size = 7500000) # jic
    library(rgdal)
    library(raster)
    

  #### data ####
  
    # read in data
    aoi <- shapefile("../GIS/Shapefiles/Elk/AreaOfInterest.shp") # study area
    tmin <- getData('worldclim', var = 'tmin', res = 10) # min temp
    tmax <- getData('worldclim', var = 'tmax', res = 10) # max temp
    precip <- getData('worldclim', var = 'prec', res = 10) # precip
    ras <- raster("alldems.tif") # elevs (i created this previously)
    
    # note different projections...
    crs(aoi); crs(ras); crs(tmin); crs(precip)

    # list of the data
    allDat <- c("tmin", "tmax", "precip", "ras")
    str(allDat)
    allDat[1]
      

  #### extract ####
 
    pullClim <- function(x, y) { # function to clip data (x) to study area (y) 
     y <- spTransform(y, x@crs) # match study area projection to data       
     inAoi <- raster::mask(x, aoi) # extract only data within study area
     summary(inAoi) # display min/max [DOES NOT STORE]
    }
    
    
    # loop through each data source, clip and display min/max
    for (i in 1:length(allDat)) {
      sArea <- aoi
      cDat <- get(allDat[i])
      cCrop <- pullClim(cDat, sArea)
    }
    
    ## errored out on last iteration
    ## and you weren't clever enough to store outputs of each, du
    ## also you need to pull data from the right years...
    
    
    
    # can delete
    test <- tmin[[1]]
    test <- raster::mask(test, aoi)
    plot(test)
    plot(aoi, add = T)
    summary(test)
    
    
    crs(tmin)
    plot(tmin)
    ?getData
    
    
    
    
    