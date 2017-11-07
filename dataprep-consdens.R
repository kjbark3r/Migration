### ### ### ### ### ### ### ### ### ### ### ### ### ##
#     PROCESSING COUNT DATA FROM MONTANA BIOLOGISTS  #
#    TO ASSESS CONSPECIFIC DENSITY DURING WINTER     #
#                   KRISTIN BARKER                   #
#                    NOVEMBER 2017                   #
### ### ### ### ### ### ### ### ### ### ### ### ### ##




### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###



#### Packages ####

library(sp) # spatial
library(rgeos) # buffer
library(adehabitatHR) # home ranges and kernel centroids
library(rgdal) # latlong/stateplane conversions
library(gsubfn) # no idea, possibly unnecessary
library(maptools) # writeSpatialShape
library(dplyr) # joins, data work, general awesomeness



#### Working directory ####

wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\Migration"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\Migration"
wd_worklaptop <- "C:\\Users\\kristin\\Documents\\Migration"
if (file.exists(wd_workcomp)) {setwd(wd_workcomp)
} else {
  if(file.exists(wd_laptop)) {setwd(wd_laptop)
  } else {
    setwd(wd_worklaptop)
  }
}



#### Projections ####

latlong <- CRS("+init=epsg:4326")
stateplane <- CRS("+init=epsg:2818")




### ### ### ### ###
####  |DATA|   ####
### ### ### ### ###


#### All count data (to update with Gravellys) ####
ests <- read.csv("../DatabasesEtc/Statewide/Elk/mt-elk-popn-ests-toupdate.csv")


#### Count data from Gravellys and Tobacco Roots ####

grav <- read.csv("../DatabasesEtc/Statewide/Elk/PopnEsts-RawFromBios/gravelly-rawdata.csv") %>%
  filter(Species == "elk") %>%
  rename(Group = Winter.Herd) %>%
  select(Year, Lat, Long, Group, Total)
tobac <- read.csv("../DatabasesEtc/Statewide/Elk/PopnEsts-RawFromBios/tobaccoroots-rawdata.csv") %>%
  filter(Species == "elk") %>%
  select(Year, Lat, Long, Total)


#### Population home ranges ####

hrs.all <- readOGR("../GIS/Shapefiles/Elk/PopnHRs", layer ='PopnYrHRs')
unique(hrs.all$id)

mad.hr <- hrs.all[hrs.all$id == "Madison",]
bla.hr <- hrs.all[hrs.all$id == "Blacktail",]
sge.hr <- hrs.all[hrs.all$id == "Sage Creek",]
tob.hr <- hrs.all[hrs.all$id == "Tobacco Roots",]




### ### ### ### ### ### ###
####  |COUNTS PER POP|  ###
### ### ### ### ### ### ###
 

   
#### Madison ####

mad.yr <- mad.hr@data$Year
mad.dat <- subset(grav, Year == mad.yr)
mad.yrlocs <- spTransform(SpatialPointsDataFrame(data.frame("x"=mad.dat$Long,"y"=mad.dat$Lat),
                                   mad.dat, proj4string = latlong), mad.hr@proj4string)
plot(mad.hr); plot(mad.yrlocs, add=T)
mad.locs <- mad.yrlocs[complete.cases(over(mad.yrlocs, mad.hr)), ] 
mad.locs@data$Total <- as.numeric(mad.locs@data$Total)
plot(mad.locs, add = T, col = "red")
sum(mad.locs@data$Total)
ests[ests$Herd == "Madison", 3]
# sum from plotted points does not match dean's estimate
# probably because herds are fluid and he's including those known to comingle from other locations



#### Blacktail ####

bla.yr <- bla.hr@data$Year
bla.dat <- subset(grav, Year == bla.yr)
bla.yrlocs <- spTransform(SpatialPointsDataFrame(data.frame("x"=bla.dat$Long,"y"=bla.dat$Lat),
                                                 bla.dat, proj4string = latlong), bla.hr@proj4string)
plot(bla.hr); plot(bla.yrlocs, add=T)
# expect dean's estimate to be higher than this count (same reason as madison)
bla.locs <- bla.yrlocs[complete.cases(over(bla.yrlocs, bla.hr)), ] 
bla.locs@data$Total <- as.numeric(bla.locs@data$Total)
plot(bla.locs, add = T, col = "red")
sum(bla.locs@data$Total) # this count
ests[ests$Herd == "Blacktail", 3] # dean's estimate
# opposite of expectation, that's not good



#### Tobacco Roots ####

tob.yr <- tob.hr@data$Year
tob.dat <- subset(tobac, Year == tob.yr)
tob.yrlocs <- spTransform(SpatialPointsDataFrame(data.frame("x"=tob.dat$Long,"y"=tob.dat$Lat),
                                                 tob.dat, proj4string = latlong), tob.hr@proj4string)
plot(tob.hr); plot(tob.yrlocs, add=T)
tob.locs <- tob.yrlocs[complete.cases(over(tob.yrlocs, tob.hr)), ] 
tob.locs@data$Total <- as.numeric(tob.locs@data$Total)
plot(tob.locs, add = T, col = "red")
sum(tob.locs@data$Total)
ests[ests$Herd == "Tobacco Roots", 3]
# this summed count is more than dean's provided estimate
