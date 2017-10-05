### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#           FORMATTING STATEWIDE ELK GPS DATA             #
#    TO ASSESS FACTORS INFLUENCING MIGRATORY BEHAVIOR     #
#                   KRISTIN BARKER                        #
#                   SEPTEMBER 2017                        #
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###




#### packages ####

library(RODBC) #possibly unnecessary; for connecting to access
library(sp) #for kernel centroid estimate
library(adehabitatHR) #for kernel centroid estimate
library(rgdal) #for latlong/stateplane conversions
library(gsubfn)
library(maptools) #for writeSpatialShape
library(dplyr) #for joins




#### working directories and database connection ####

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
if(file.exists(wd_worklaptop)) {
 channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                              dbq=C:/Users/kristin/Documents/DatabasesEtc/Statewide_Elk_GPS.accdb")
} else {  cat("Maybe you shouldn't have been so lazy when you made this code") }
rm(wd_workcomp, wd_laptop, wd_worklaptop)



#### define projections ####

latlong <- CRS("+init=epsg:4326")
stateplane <- CRS("+init=epsg:2818")



#### read in and format elk collar location data ####

#locs <- read.csv("../DatabasesEtc/Statewide/mtelkcollardata.csv", as.is = TRUE, header = TRUE)
rawlocs <- read.csv("../DatabasesEtc/collardata-locsonly-equalsampling.csv", as.is = TRUE, header = TRUE)

alllocs <- rawlocs %>%
  # format date
  within(Date <- as.Date(Date, "%Y-%m-%d")) %>%
  # add month, year, season, and HRYear (match Dec from previous calendar year with subsequent Jan and Feb)
  mutate(Month = months(Date, abbreviate = TRUE),
         Year = format(as.Date(Date, format = "%Y-%m-%d"), "%Y"),
         Season = ifelse(Month == "Dec" | Month == "Jan" | Month == "Feb", "Winter", 
                         ifelse(Month == "Mar" | Month == "Apr" | Month == "May", "Spring",
                                ifelse(Month == "Jun" | Month == "Jul" | Month == "Aug", "Summer",
                                       "Fall"))),
         HRYear = ifelse(Month == "Dec", as.numeric(Year)+1, as.numeric(Year)))



#### keep only indivs who have both winter and summer HR in the same year ####  

# subset only winter and summer locations (just to speed computing)
vidat <- subset(alllocs, Season == "Winter" | Season == "Summer")

# list relevant years of data
yrs <- unique(vidat$HRYear)

# define number of years
nyrs <- length(yrs)

# create empty dataframe to store relevant locations in
# with the same number of columns and column names as the full dataframe
alldat <- data.frame(matrix(NA, nrow = 0, ncol = ncol(vidat)))
colnames(alldat) <- colnames(vidat)

for (i in 1:nyrs) {
  # for each year
  yr <- yrs[i]
  # subset that year's data
  yrdat <- subset(vidat, HRYear == yr)
  # and make a list of indivs from that year
  indivs <- unique(yrdat$AnimalID)
  
  for(j in 1:length(indivs)) {
    # for each individual from that year
    indiv <- indivs[j]
    indivdat <- subset(yrdat, AnimalID == indiv)
    # if it has both winter and summer locations
    indivdatsub <- data.frame(ifelse(length(unique(indivdat$Season)) > 1, indivdat, next))
    # keep its data; otherwise discard (bc volume intersection requires both seasonal HRs)
    alldat <- rbind(alldat, indivdat)
  }
} 

write.csv(alldat, file = "elklocs-behavclassn.csv", row.names=F)
