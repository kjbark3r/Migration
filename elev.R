### ### ### ### ### ### ### ### ### ### ### ### ##
###  EXTRACT ELEVATIONS FOR ALL ELK LOCATIONS  ###
### 	  	 Kristin Barker  Nov 2017	      	   ###
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
  mutate(DT = as.POSIXct(DT, format = "%Y-%m-%d")) %>%
  within(Time <- substr(Time, 12, 19)) %>%
  mutate(DateTime = as.POSIXct(paste(DT, Time, sep = " "),
                           format = "%Y-%m-%d %H:%M:%S")) %>%
    dplyr::select(AnimalID, Herd, DateTime, Latitude, Longitude, Sex, Elev)
write.csv(locelevs, "../locs-allcows-withelevs.csv", row.names = FALSE)
