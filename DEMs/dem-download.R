###-#########################-###
###  DOWNLOAD DEMs FROM USGS  ###
###  Kristin Barker  Nov 2017 ###
###-#########################-###



#### WD ####
setwd("C:\\Users\\kristin\\Documents/Migration/DEMs")


#### Packages ####
library()


#### Download DEMs ####

dat <- read.csv("filelist.csv") # exported from national map viewer
urllist <- as.character(dat$URL)

for (i in 1:length(urllist)) {
  web <- urllist[i]
  temp <- tempfile()
  download.file(web, temp, mode = "wb")
  a <- unzip(temp)
  
}

