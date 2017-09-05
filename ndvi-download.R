###-#######################################-###
###  DOWNLOAD NDVI GEOTIFFS FROM STATE FTP  ###
###-####  Kristin Barker  Jul 2017  #######-###
###-#######################################-###



#### WD ####
wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\KDEtest"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\KDEtest"
wd_worklaptop <- "C:\\Users\\kristin\\Documents/ElkMigrationAnalyses/Forage"
if (file.exists(wd_workcomp)) {setwd(wd_workcomp)
} else {if(file.exists(wd_laptop)) {setwd(wd_laptop)
  } else {setwd(wd_worklaptop)
  }
}
rm(wd_workcomp, wd_laptop, wd_worklaptop)



#### PACKAGES ####
library(RCurl) ## for getURL()



#### DATA ####

## specify years you want data from (here, years are names of subfolders within folder called NDVIdata in the working directory)
years <- as.numeric(list.files("./NDVIdata/"))

for (i in 1:length(years)) {
  yr <- years[i]  ## for each year
  web <- paste0("ftp://mco.cfc.umt.edu/ndvi/terra/sixteenday/geotiff/", yr, "/")  ## go to site with that year's data
  fileblob <- getURL(web, dirlistonly = TRUE) ## list files/folders on site (in weird format with extra characters)
  files <- paste(web, strsplit(fileblob, "\r*\n")[[1]], sep="") ## fix list 
  todl <- grep("[^zscore].zip$", files) ## identify folders from list to download (the ones that aren't zscores)
  
  for (j in 1:length(todl)) {
    n <- todl[j] ## for each zipped folder you want from that year,
    temp <- tempfile() ## create temporary folder to store zip in
    download.file(files[n], temp, mode = "wb") ## download and store (wb is binary (compressed) file)
    zipname <- sub("[^[:upper:]]+", "", x = files[n]) ## pull filename from zip
    f <- sub("zip", "tif", zipname) ## specify which file to store permanently (just want tif, not whole zip)
    ed <- paste0(getwd(), "/", "NDVIdata", "/", yr) ## specify where to store file (in correct subfolder for that year)
    a <- unzip(temp, files = f, exdir = ed, overwrite = TRUE) ## unzip and store file of interest
  }
}
