### PLAYING AROUND WITH HANDLING NDVI DATA ###
# KJB 2017-07-13

setwd("C:/Users/kristin/Documents/ElkMigrationAnalyses/Forage")

library(raster)



#### INITIAL MUDDLING  ####


#### checking out file from mt.gov ####

test <- raster("NDVI20160406097.tif")
test
plot(test)
# sweet, this is entire state so should hopefully cover whatever i need
# downloaded from mt state website

elk <- raster("C:\\Users\\kristin\\Documents\\zzDesktop_2017-06-13\\GitHub\\KDEs\\KDE140630-15sum.tif")
plot(elk, add=T)

str(test); str(elk)
# oh duh, need to reproject one or the other
# first let's try making ndvi match our data

elk@crs
ndvi <- projectRaster(test, crs = elk@crs)
# obvs this takes forever. 
# if you do this in real life at least crop it first

plot(elk)
plot(ndvi, add=T)
#yasssss



#### playing with unzipping/extracting ####

temp <- tempfile() #stores file in temp appdata folder
download.file("ftp://mco.cfc.umt.edu/ndvi/terra/sixteenday/geotiff/2016/NDVI20160101001.zip", 
              temp, mode = "wb") #wb is binary file (compressed files are binary)
a <- unzip(temp, files = "NDVI20160101001.tif") #character list of files that were in folder
a #ok this is just filepath; need actual file...
b <- raster(a)
plot(b)
# yasssss

str(b)
b@data
(10*(360/16)*7)/60 #30ish mins to download all files 2005-2016



#### checking out what other people's code does ####

t=as.Date(paste0("2013-",seq(1,300,16)),"%Y-%j")
t #this fails to match dates properly after feb (short month)


#### STARTING SCRIPT ATTEMPTS ####


#### years

list.files("./NDVIdata/") #created these folders manually, how embarassing
d <- data.frame(list.files("./NDVIdata/"))
d
d[1]
length(d) #heh heh
nrow(d)
d[[1]]
d[1,1]

for (i in 1:nrow(d)) {
  yr <- d[i,1]
  print(yr)
}

e <- list.files("./NDVIdata/")
e
e[1]
length(e)

for (i in 1:length(e)) {
  yr <- e[i]
  print(yr)
}

e <- as.numeric(e)

for (i in 1:length(e)) {
  yr <- e[i]
  print(yr)
}


f <- as.numeric(list.files("./NDVIdata/"))

for (i in 1:length(f)) {
  yr <- f[i]
  print(yr)
}
#thar she blows


years <- as.numeric(list.files("./NDVIdata/"))

for ( i in 1:length(years)) {
  yr <- f[i] 
  temp <- tempfile()
  web <- paste0("ftp://mco.cfc.umt.edu/ndvi/terra/sixteenday/geotiff/", yr, "/")
  list.files(path = web) 
}
#newp, grep for .zip instead



for ( i in 1:length(years)) {
  yr <- f[i] 
  temp <- tempfile()
  web <- paste0("ftp://mco.cfc.umt.edu/ndvi/terra/sixteenday/geotiff/", yr, "/")
  list.files(path = web) #newp
}


# 


library(XML)
?getHTMLLinks
getHTMLLinks(web)
web2 <- as.character(web)
getHTMLLinks(web2)
# don't think this works with ftp, just html

library(RCurl)
filenameblob <- getURL(web, dirlistonly = TRUE)
filenameblob
files <- paste(web, strsplit(filenameblob, "\r*\n")[[1]], sep="")
files
# geez took ya along enough


#### trying tifs instead of zips, i think that's all you want anyway ####

library(RCurl)
library(raster)

temp <- tempfile() #stores file in temp appdata folder
download.file("ftp://mco.cfc.umt.edu/ndvi/terra/sixteenday/geotiff/2016/NDVI20160101001.tif", 
              temp, mode = "w") #wb is binary file (compressed files are binary)
b <- raster(temp)
plot(b)
# looks a lot different from the unzipped tif

test <- raster("C:\\Users\\kristin\\Downloads\\NDVI20050101001.tif")
plot(test)

a <- unzip(temp, files = "NDVI20160101001.tif") #character list of files that were in folder
a #ok this is just filepath; need actual file...
b <- raster(a)
plot(b)


# yeah ok no, tif on ftp is not what you want


#### AHHH SO CLOSE ####

library(RCurl)
library(raster)

# list of years, from names of subfolders created manually
years <- as.numeric(list.files("./NDVIdata/"))

for (i in 1:length(years)) {
  yr <- years[i] 
  temp <- tempfile()
  web <- paste0("ftp://mco.cfc.umt.edu/ndvi/terra/sixteenday/geotiff/", yr, "/")
  fileblob <- getURL(web, dirlistonly = TRUE) #list of files in weird format with extra chars
  files <- paste(web, strsplit(fileblob, "\r*\n")[[1]], sep="") #list of files
  todl <- grep("[^zscore].zip$", files) #index- zip files that aren't zscores
  
  for (j in 1:length(todl)) {
    n <- todl[j] #for each zip file
    temp <- tempfile() #create temp file (in C:/users/me/appdata/local/temp/Rtmpblahblahblah)
    download.file(files[n], temp, mode = "wb") #wb is binary (compressed) file
    tf <- grep(".tif$", temp, value = TRUE)
    a <- unzip(temp, files = grep(".tif$", temp, value = TRUE))
    b <- raster(substring(a[2], 3))
    writeRaster(b, filename = paste0("NDVIdata", "/", yr, "/", substr(a, 3, 14), ".tif"), 
                format = "GTiff")
  }
}

dude <- raster("NDVIdata/2005/NDVI20050101.tif")
plot(dude)

## issues to fix in the above:
#at least 2 problems with a<-unzip: 
  #1 pulls all files, not just tif 
    #(grep may not work inside unzip; try using grep before that line?)
  #2 unzips into working directory
#only works for the first j loop; does write the first raster
  #but thinks it can't(?) and never starts back over again

# what you want it to do is just unzip the one tif file
  # directly into the correct subfolder (correct NDVI year)

temp <- tempfile() #stores file in temp appdata folder
download.file("ftp://mco.cfc.umt.edu/ndvi/terra/sixteenday/geotiff/2016/NDVI20160101001.zip", 
              temp, mode = "wb") #wb is binary file (compressed files are binary)
a <- unzip(temp, files = "NDVI20160101001.tif") #character list of files that were in folder
a #ok this is just filepath; need actual file...
b <- raster(a)
plot(b)




#### CMONNNNNN BABYYYYYY ####


library(RCurl)
library(raster)

# list of years, from names of subfolders created manually
years <- as.numeric(list.files("./NDVIdata/"))


for (i in 1:length(years)) {
  yr <- years[i] 
  temp <- tempfile()
  web <- paste0("ftp://mco.cfc.umt.edu/ndvi/terra/sixteenday/geotiff/", yr, "/")
  fileblob <- getURL(web, dirlistonly = TRUE) #list of files in weird format with extra chars
  files <- paste(web, strsplit(fileblob, "\r*\n")[[1]], sep="") #fixed list of files
  todl <- grep("[^zscore].zip$", files) #index- zip files that aren't zscores
  
  for (j in 1:length(todl)) {
    n <- todl[j] #for each zip file
    temp <- tempfile()
    download.file(files[n], temp, mode = "wb") #wb is binary (compressed) file
    zipname <- sub("[^[:upper:]]+", "", x = files[n]) #pull correct filename from zip
    f <- sub("zip", "tif", zipname) #specify it's the tif (not the zip)
    ed <- paste0(getwd(), "/", "NDVIdata", "/", yr) #tell where to save file
    a <- unzip(temp, files = f, exdir = ed, overwrite = TRUE)
    #b <- raster(a)
    #writeRaster(b, filename = paste0("NDVIdata", "/", yr, "/", substr(a, 3, 14), ".tif"), 
    #            format = "GTiff")
  }
}