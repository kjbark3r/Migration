### PLAYING AROUND WITH HANDLING NDVI DATA ###
# KJB 2017-07-13

setwd("C:\\Users\\kristin\\Documents\\Migration")


#### NEW HELPFUL STUFF ####

basename(getwd()) # pulls name from filepath, holy shit
dirname() # does the opposite

# this didn't make any difference for stdev of ndvi stuff
# but could be helpful later for individual stuff?
# supposed to speed processing by using more cores or something
beginCluster(4, type = 'SOCK')
endCluster()

##
library(raster)



#### INITIAL MUDDLING  ####


#### checking out file from mt.gov ####

test <- raster("NDVI20160406097.tif")
test
plot(test)
# sweet, this is entire state so should hopefully cover whatever i need


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




#### old and probably useless preparatory code ####
      #  sudafed's a hell of a drug #

# test reading in ndviamp
test <- raster("./NDVIamp/NDVIamp2006.tif")

# match area of interest projection to ndviamp projection
# DONT FORGET YOULL NEED TO CHANGE BACK LATER

crs(test)
aoirast <- raster(aoi)
testreproj <- projectRaster(aoirast, crs = crs(test))
crs(aoirast)
plot(aoirast)
plot(aoi)

(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m
+no_defs")


crs(aoi)
# match ndviamp projection to area of interest projection

testreproj <- projectRaster(test, crs = "+proj=lcc +lat_1=45 +lat_2=49 +lat_0=44.25 +lon_0=-109.5 +x_0=600000 +y_0=0
                            +ellps=GRS80 +units=m +no_defs")
testcrop <- crop(test, aoi)




#### why am i having issues reprojecting??? ####

aoi <- spTransform(aoiraw, crs = ampstk@layers[[1]]@crs)
aoi <- spTransform(aoiraw, crs(ampstk))
crs(aoi)
# um ok, apparently i'm not




#### figuring out how to id correct layers from bricks ####


tpopyr <- popnyr[1,]
tpopyr$Herd <- factor(tpopyr$Herd)
names(ampstk)
class(ampstk@layers) # this is a list of RasterLayers (1 for each year)
class(ampstk@layers[[1]]) # this is the first RasterLayer
#ooh ooh lapply
?lapply # returns a list based on a function
  # so this could possibly list yrs but i'm still not sure how to pull the ones i want

# ok back up
tpopyr$Year
# year of interest is 2011
# pull amp 2011 from stk

test <- subset(ampcrop, 1)
names(test)
# this pulls amp2001

test <- subset(ampcrop, 1:2)
names(test)
# pulls 2001 and 2002, of course

# PLAN: create index mapping year to number (2004 = 1, 2005 = 2, etc...)

y <- c(1, 2, 3)
test <- subset(ampcrop, y)
names(test)
# excellent, this does pull all 3 years
str(popnyr$Year)

popdat <- popnyr %>%
  select(-nIndiv) %>%
  mutate(yIndex = Year-2003,
         Pop = ifelse(Herd == "Blacktail", "bla",
               ifelse(Herd == "Border", "bor",
               ifelse(Herd == "Dome", "dom",
               ifelse(Herd == "East Fork", "efk", 
               ifelse(Herd == "Elkhorns", "elk",
               ifelse(Herd == "Greeley", "grl",
               ifelse(Herd == "HD314", "gal",
               ifelse(Herd == "Madison", "mad",
               ifelse(Herd == "Mill Creek", "mil",
               ifelse(Herd == "NMadison", "nmd",
               ifelse(Herd == "Pioneers", "pio",
               ifelse(Herd == "Sage Creek", "sge",
               ifelse(Herd == "Sapphire", "sap",
               ifelse(Herd == "Silver Run", "sil",
               ifelse(Herd == "Tobacco Roots", "tob",
               ifelse(Herd == "West Fork", "wfk", 
                      NA)))))))))))))))))


# indexing correct home range from spolydf
str(popnhrs@polygons[[1]]@Polygons[[1]])



#### figuring out how to subset one home range poly from spolydf ####

names(popnhrs)
#"id" and "area"
popnhrs$id
# id maps to population name (same as Herd)
length(unique(popnhrs$id))

# try just keeping the first one
length(unique(popnhrs$id)) # 16 now, shooting for 1
popnhrs$id[1]
test <- popnhrs[popnhrs$id[1],]
length(unique(test$id))
# oh. duh. 
str(test)
# my god, why was that so hard yesterday?

## ok, now you have an extent issue
## ndvi layer extents don't overlap hr extent (but they should)
crs(amp)
crs(hr)
# why are these in different projections? i thought i reprojected hrs
# ...you only reprojected aoi, geez kristin, get your shit together
# and quit working past your ability to actually be effective

plot(ampc[[1]])
plot(hr, add = T)
# oh hell yes
# now you need to store loop outputs, 
  # which you have never figured out how to do well...
# also it looks like you miss a teeny bit of the ndvi data in the crop
# how to make sure it fills the entire polygon? (do this first, obvs)

# idea from stackoverflow 
  # which looks intuitive but apparently nobody likes...
#create list of cells that intersect home range polygon; set others to NA
#then create raster with NAs removed
testamp <- ampsub #rasterstack
testlist <- cellFromPolygon(ampsub, hr, weights = TRUE)[[1]][,"cell"] # list of cells and weights (overlappiness)
testamp[][-testlist] <- NA
plot(trim(testamp[[1]]))
plot(hr, add = T)
# ok this does work as expected
# but only for one layer of the brick
# easy enough to include all??
  # need to figure out that indexing portion of testlist

testlist <- cellFromPolygon(ampsub, hr, weights = TRUE)
  # i believe this should be applicable to all layers in that brick for that iteration

# create list without the index? doubt it...
#testlist2 <- cellFromPolygon(ampsub, hr, weights = TRUE)[,"cell"]
# yeah no

testlist <- cellFromPolygon(ampsub, hr, weights = TRUE)
testlist[[1]]
testlist[[1]]["cell"]
  # returns NA (like, an actual NA I think?)
  # so then that next line says anything OTHER THAN THIS is an NA
testamp2 <- ampsub
testamp2[][-testlist] <- NA
# yeah you really need that index. 
## take time to understand structure of testlist

# break down into its component parts
a <- cellFromPolygon(ampsub, hr, weights = TRUE)
a
  # list of 1, has what i assume is grid cells and associated attributes
  # the first attribute is "cell"; second is "weight"
  # ohhh ok it's 2 "columns", first is cell index, second is weight
  # weight was needed to id cells touching edge as well
attr(a[[1]], "dimnames")
attr(a[[1]])


#plot(testamp[]) #omg never do this again



## lets try that again
testamp <- ampsub
# rasterstack with 6 rasters (1 per year needed for this popn)
a <- cellFromPolygon(ampsub, hr, weights = TRUE)
testlist <- cellFromPolygon(ampsub, hr, weights = TRUE)[[1]][,"cell"]
  # ok diff bt a and testlist is that testlist is ONLY THE CELLS
  # no attributes, weights, etc - just the cells you want to keep
testamp[][-testlist] <- NA

# i believe it's finally time for you to use lapply (god i hope this works)
testamp@layers
class(testamp@layers)
# ok testamp@layers is a list of the 6 raster layers you want to fuck with

?lapply #returns as list, so hopefully can just get testamp@layers back (?)
  # but ithink i need to write a function to tell it what to do

rmjob <- function(x) {
  x[][-testlist] <- NA
}


#z <- lapply(testamp@layers[1], FUN = rmjob)
# that didn't work. test function first

rmjob(testamp)
# ok your function doesn't work

testamp <- ampsub
rmjob <- function(x) {
  x[][-testlist] <- NA
}
rmjob(testamp)
plot(trim(testamp[[1]]))
# yeah no

testamp <- ampsub
testlist <- cellFromPolygon(ampsub, hr, weights = TRUE)[[1]][,"cell"]
testamp[][-testlist] <- NA
plot(trim(testamp[[2]]))

plot(testamp@layers[[1]])
plot(testamp@layers[[2]])

testamp@layers[][-testlist] <- NA
#error unable to find an inherited method for function 'nlayers' for signature '"logical"'

testamp@layers[[1]][-testlist] <- NA

for (i in 1:6) {
  lay <- testamp[[i]]
  lay[][-testlist] <- NA
  #laybrk <- 
}
# feck


install.packages("gdalUtils")
library(gdalUtils)
?gdal_rasterize

# ok i think you do need to use that argument instead of the one nobody else liked
# shocker... 
# because output can be a rasterbrick! :)


# ## stackoverflow code
# writeOGR(IT, tempdir(), f <- basename(tempfile()), 'ESRI Shapefile')
# gdal_rasterize(sprintf('%s/%s.shp', tempdir(), f), 
#                f2 <- tempfile(fileext='.tif'), at=T,
#                tr=res(x_crop), te=c(bbox(x_crop)), burn=1, 
#                init=0, a_nodata=0, ot='Byte')
# 
# plot(x_crop*raster(f2)) # multiply the raster by 1 or NA
# plot(IT, add=TRUE)


install.packages("gdal")
library(gdalUtils)
test <- ampsub
writeOGR(hr, tempdir(), f <- basename(tempfile()), 'ESRI Shapefile')
gdal_rasterize(sprintf('%s/%s.shp', tempdir(), f),
               f2 <- tempfile(fileext = '.tif'), at = T,
               tr = res(test), te = c(bbox(test)), burn = 1,
               init = 0, a_nodata = 0, ot = 'Byte')
plot(test*raster(f2))
#plot(hr, add = T)

class(test[[1]])
class(test@layers[[1]])
test[[1]]

z <- calc(test, fun = function(x){x*2})
plot(z)
# baby steps...

plot(raster(f2))

z<- calc(test, fun = function(x){x*raster(f2)})
class(test)

## ok so
library(gdalUtils)
test <- ampsub
writeOGR(hr, tempdir(), f <- basename(tempfile()), 'ESRI Shapefile')
gdal_rasterize(sprintf('%s/%s.shp', tempdir(), f),
               f2 <- tempfile(fileext = '.tif'), at = T,
               tr = res(test), te = c(bbox(test)), burn = 1,
               init = 0, a_nodata = 0, ot = 'Byte')
z <- calc(test, fun = function(x){x*raster(f2)},
          forceapply = FALSE)
plot(z)
# hey maybe you shouldn't have ignored the warning that gdal isn't installed

hm <- raster(f2)

# got error abt not being able to allocate vector of that size
library(gdalUtils)
memory.limit(size = 7500000)
test <- ampsub
writeOGR(hr, tempdir(), f <- basename(tempfile()), 'ESRI Shapefile')
gdal_rasterize(sprintf('%s/%s.shp', tempdir(), f),
               f2 <- tempfile(fileext = '.tif'), at = T,
               tr = res(test), te = c(bbox(test)), burn = 1,
               init = 0, a_nodata = 0, ot = 'Byte')
z <- calc(test, fun = function(x){x*raster(f2)})
# issue: this is way too memory-intensive

# plan a: remove all unnecessary objects and make test a brick, not a stack
# plan b: figure out how to make that other code work on each successive layer
# plan c: break rasterbricks into individual rasters (not a good idea)

# plan a
library(gdalUtils)
memory.limit(size = 7500000)
test <- ampsub
writeOGR(hr, tempdir(), f <- basename(tempfile()), 'ESRI Shapefile')
test <- brick(test)
rm(list=setdiff(ls(), c("test", "f")))
gdal_rasterize(sprintf('%s/%s.shp', tempdir(), f),
               f2 <- tempfile(fileext = '.tif'), at = T,
               tr = res(test), te = c(bbox(test)), burn = 1,
               init = 0, a_nodata = 0, ot = 'Byte')
gc()
z <- calc(test, fun = function(x){x*raster(f2)})
# shit, ok, that's not gonna work unless i use another server or something

# plan b
testamp <- ampsub #rasterstack of just years pertinent to a popn
testlist <- cellFromPolygon(ampsub, hr, weights = TRUE)[[1]][,"cell"] # list of cells and weights (overlappies)
testamp[][-testlist] <- NA
plot(trim(testamp[[1]]))
plot(hr, add = T)
plot(trim(testamp[[2]]))
  # error is now that everything after that first layer are only NAs
plot(ampsub[[2]])
  # whereas they should exist if you were doing this right

# pesky line is
# testamp[][-testlist] <- NA
class(testamp[])
  # this is a matrix
class(testamp)
  # this is a rasterbrick

## ok. youve reached your limit of productivity,
## pick this back up in the am (wah wah)

names(testamp)


## back at it
testamp <- ampsub
amp <- mask(testamp, hr)
plot(amp)
ampc <- crop(amp, hr)
plot(ampc)
plot(ampc[[1]])
plot(hr, add=T)
plot(hr)
plot(ampc[[1]], add=T)

#ok. crop is the issue (as you've known for 2 fucking days)
int <- intersect(testamp, hr)

# y'know what, maybe arcmap would do this better and manual is faster in long run
# to test that:
writeOGR(ampcropsp, #existing arcmap is stateplane
         verbose = TRUE,
         dsn = "../GIS/Shapefiles/NDVI/amp", 
         layer = "ndviamp2001", 
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# oops, can't use writeOGR with a brick apparently

writeRaster(ampcropsp, 
            paste0("../GIS/Shapefiles/NDVI/amp", names(ampcropsp)), 
            bylayer = TRUE, 
            format = "GTiff", 
            overwrite = TRUE)

# no, no, arcmap is a terrible idea

# next best plan is to break brick into separate rasters
# and use that jenky code that at least works




#### calculating stdev of each pixel ####

# populations of interest
popnsyrs <- read.csv("popns-yrs.csv")

# add pop code column matching start of tif filenames
popdat <- popnsyrs %>%
  mutate(Pop = ifelse(Herd == "Blacktail", "bla",
               ifelse(Herd == "Border", "bor",
               ifelse(Herd == "Dome", "dom",
               ifelse(Herd == "East Fork", "efk", 
               ifelse(Herd == "Elkhorns", "elk",
               ifelse(Herd == "Greeley", "grl",
               ifelse(Herd == "HD314", "gal",
               ifelse(Herd == "Madison", "mad",
               ifelse(Herd == "Mill Creek", "mil",
               ifelse(Herd == "NMadison", "nmd",
               ifelse(Herd == "Pioneers", "pio",
               ifelse(Herd == "Sage Creek", "sge",
               ifelse(Herd == "Sapphire", "sap",
               ifelse(Herd == "Silver Run", "sil",
               ifelse(Herd == "Tobacco Roots", "tob",
               ifelse(Herd == "West Fork", "wfk", 
                      NA))))))))))))))))) 

# just try for one population, no loop
# to figure out how to calculate across rasters for each pixel

    # pull NDVI amplitude specific to popn (from rasterclip.R)
    popcodes <- popdat$Pop

    npops <- length(popcodes)
    

    
    pop <- popcodes[i] # replace 1 with i for loop

    files.amp <- list.files(
        path = "./NDVIamp/processed",
        pattern = paste0("^", noquote(pop)),
        full.names = TRUE)
    ampstk <- stack(files.amp)
    names(ampstk)
    
    files.ti <- list.files(
        path = "./NDVIti/processed",
        pattern = paste0("^", noquote(pop)),
        full.names = TRUE)
    tistk <- stack(files.ti)
    names(tistk)

    
    predfor <- data.frame(Pop = popcodes, 
                          StDevAmp = NA,
                          StDevTi = NA)
    
    # use this cluster thing to speed processing
    
    beginCluster(4, type = 'SOCK')
    ampvar <- calc(ampstk, fun = sd)
    tivar <- calc(tistk, fun = sd)
    endCluster()

    asd <- cellStats(ampvar, stat = 'sd')
    tsd <- cellStats(tivar, stat = 'sd')
    
    predfor[i, 2] <- asd
    predfor[i, 3] <- tsd
    
    
#### checking that extracted values make sense ####
    
    # goal: glance at histogram of amp and ti values in ampcropsp and ticropsp
    
    summary(ampcropsp@data@values)
    str(ampcropsp[[1]])
    str(ampcropsp[[1]]@data)
    str(ampcropsp[[1]]@data@attributes)
    attributes(ampcropsp[[1]]@data)    
    cellStats(ampcropsp, stat = 'max')
    hist(ampcropsp[[1]]) # duh, quit making things harder than they have to be

    
#### double-checking i cropped rasters for predfor correctly ####    
    
    test <-  raster("./NDVIamp/bla-amp2006.tif")    
    plot(test) #whaddaya know, i actually did
    
    
    
#### cut code from calculating deltafor ####  
    
     amp2crop2 <- resample(amp2crop, popnampstk, method = "bilinear")
     # this slightly changes values; trying to avoid resampling
    
     extent(amp2crop) <- extent(popnampstk) 
     extent(ti2crop) <- extent(popntistk)
     # this just changes what it *says* the extent is, not the actual extent
     
         
          popnampstk <- stack()
          extent(popnampstk) <- extent(aoi)
          popntistk <- stack()
          extent(popntistk) <- extent(aoi)
          # nixed stacking because don't want to alter values
          # so i don't want to change resolution
          
          
              
            # expand extent to include full study area
            amp2crop2 <- extend(amp2crop, extent(aoi))
            # don't need to do this bc am interested in just this popn raster, not also indiv one
          
            
            
#### figure out how to remove intersecting polygon areas ####

# start with one population's amp plot                
plot(amp.cfk)
# find a clarks fork indiv - BRUC15031
testi <- "BRUC15031"
testihr <- subset(indivhrswin2, id == testi)
plot(testihr)
# now overlay the indiv hr on the popnhr and remove the intersecting area
testirast <- raster(testihr)
plot(testirast)
# newp
testpoly <- rasterToPolygons(amp.cfk)
plot(testpoly)
# if do this will need to just create this shape and then use it to reclip the raster
# bc raster vals appear to be shot
# would prefer to rasterize the indivhr
testirast <- rasterize(testihr)
# newp
testisp <- SpatialPolygons(testihr@polygons)
testirast <- rasterize(testisp)
# newp
# need reference raster
testirast <- rasterize(testisp, amp.cfk)
plot(testirast)
plot(testisp)
# newp, must need to add field
testirast <- rasterize(testisp, amp.cfk, field == 1)
plot(testirast)
crs(testihr)
crs(amp.cfk)
# oh. oops.
testi <- spTransform(testihr, crs(amp.cfk))
testisp <- SpatialPolygons(testi@polygons)
testirast <- rasterize(testisp, amp.cfk)
plot(testirast)    
plot(amp.cfk, add = T)

plot(amp.cfk); plot(testirast, add = T, col = "red")


# mexcellent. ok so, so far:

testi <- "BRUC15031"
testihr <- subset(indivhrswin2, id == testi)
testi <- spTransform(testihr, crs(amp.cfk))
testisp <- SpatialPolygons(testi@polygons)
testirast <- rasterize(testisp, amp.cfk)

# now remove intersecting area

# aw shit, did i not have to make it a raster?
test <- mask(amp.cfk, mask = testi)
plot(test)
# motherfucker

test <- mask(amp.cfk, mask = testi, inverse = TRUE)
plot(test) 
# <3
      


#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
# ok so heres what you need to do for each indiv

# identify correct popn raster, naming convention "amp." or "ti." then "popncode"
amp.cfk
# identify indiv
testi <- "BRUC15031"
# subset out indiv hr from all indiv hrs
testihr <- subset(indivhrswin2, id == testi)
# match projection to ndvi data
testi <- spTransform(testihr, crs(amp.cfk))
# use INVERSE mask to remove indiv hr area
test <- mask(amp.cfk, mask = testi, inverse = TRUE)
# calculate and store max value

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

      
#### make this code not suck ####            
          
                # now determine max "forage" avail outside each indiv's winter range
      for (i in 1:nindiv) {
        
        # for each elk, identify year of interest, home range, and ndvi rasters
        elk <- indivdat[i,"AnimalID"]
        yr <- indivdat[i,"Year"]
        herd <- indivdat[i, "Herd"]
        outamp <- subset(popnampstk, id == herd)
        outti <- subset(popntistk, id == herd)
        extent(hr) <- extent(outamp, id == elk)
      #   
      #   # read in population ndvi data 
      #   # identify cells that are within or touching edge of home range
      #   ampcells <- cellFromPolygon(amp, hr, weights = TRUE)[[1]][, "cell"]
      #   ticells <- cellFromPolygon(ti, hr, weights = TRUE)[[1]][, "cell"]
      #   
      #   # set all other cells to NA
      #   amp[][-ampcells] <- NA
      #   ti[][-ticells] <- NA
      #   
      #   # and set "no data" cells to NA
      #   amp[amp==255] <- NA
      #   ti[ti==255] <- NA
      #   
      #   # remove NA cells
      #   ampcrop <- trim(amp)
      #   ticrop <- trim(ti)
      #  
      #   # store results
        
      #   
      # 
      }


#### assessing whether 255 values are legit ####


writeRaster(ampcrop, paste0("../GIS/Shapefiles/NDVI/", names(ampcrop)), 
  bylayer = TRUE, format = "GTiff", overwrite = TRUE)
writeRaster(ticrop, paste0("../GIS/Shapefiles/NDVI/", names(ticrop)), 
  bylayer = TRUE, format = "GTiff", overwrite = TRUE)

hist(ampcrop)
par(mfrow=c(1,1))
hist(ampcrop[[16]], breaks = 100)
hist(ticrop[[16]], breaks = 100)

test.a <- data.frame(unique(ampcrop[[16]]))
test.t <- data.frame(unique(ticrop[[16]]))

testamp <- ampcrop
testamp[][255] <- NA
test.a2 <- data.frame(unique(testamp[[16]]))

# baby's first reclassification function

rc <- function(x) { 
  ifelse(x == 255, NA, x)
}

# test
test <- c(3,4,5,6,7,8,255)
test <- rc(test)

# yeah ok that was not actually difficult