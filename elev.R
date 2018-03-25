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

library(beepr)
memory.limit(size = 7500000)
#### read in that raster i already conveniently created
ras <- raster("alldems.tif")
#plot(ras)
aoi <- shapefile("../GIS/Shapefiles/Elk/AreaOfInterest.shp")
aoi <- spTransform(aoi, ras@crs)
#plot(aoi, add = TRUE)
aoielevs <- raster::intersect(ras, aoi)
writeRaster(aoielevs, "elevsAoi", format = "GTiff")
min(aoielevs) # this isn't the right fcn probably but it works to display min/max