### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#    CLASSIFYING MIGRATORY BEHAVIOR OF INDIVIDUALS        #
# BASED ON VOLUME INTERSECTION OF SEASONAL USE AREAS      #
#                   KRISTIN BARKER                        #
#                   SEPTEMBER 2017                        #
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###




#### packages ####

  library(adehabitatHR) #home range estimates
  library(rgdal) #latlong/stateplane conversions
  library(gsubfn)
  library(dplyr) #joins, pipes, general awesomeness



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
  rm(wd_workcomp, wd_laptop, wd_worklaptop)


#### prep data and definitions for loop ####
  
  # source("dataprep-behav-classn.R"); rm(list=ls())
  
  # read in data; subset females; add unique identifier per year
  elklocs <- read.csv("elklocs-behavclassn.csv") %>%
    subset(Sex == "Female") %>%
    mutate(IndivHRYr = paste0(AnimalID, "-", 
                              substr(HRYear, 
                                     (nchar(HRYear)+1)-2, 
                                     nchar(HRYear))))

  # set list and number of individuals to loop through
  elk <- unique(elklocs$IndivHRYr) 
  nelk <- length(elk) 

  # define projections
  latlong <- CRS("+init=epsg:4326")
  stateplane <- CRS("+init=epsg:2818")
  
  # empty dataframe to store results in
  migbehav <- data.frame(IndivHRYr = NA,
                         #km2Win = NA,
                         #km2Sum = NA,
                         VI95 = NA,
                         VI50 = NA)

  
#### calculate seasonal HR area and volume intersection for each indiv ####
  
  for (i in 1:nelk) {
    indiv <- elk[i]
    dat <- subset(elklocs, IndivHRYr == indiv)
    
    # get xy points; write to dataframe, to spatial data frame, to stateplane projection
    xy <- data.frame("x"=dat$Long,"y"=dat$Lat)
    spdf.ll <- SpatialPointsDataFrame(xy, dat, proj4string = latlong)
    spdf.sp <- spTransform(spdf.ll,stateplane)
    
    # create winter and summer kdes; calculate area and volume intersection ([,20] is MigHR)
    kud <- kernelUD(spdf.sp[,match("Season", names(dat))], h = "href") # reference bandwidth;
    #km2 <- getverticeshr(kud, unin = "m", unout = "km2") # kde areas
    #area <- as.data.frame(km2) 
    vi95 <- kerneloverlaphr(kud, method = "VI", percent = 95, conditional = TRUE)
    vi50 <- kerneloverlaphr(kud, method = "VI", percent = 50, conditional = TRUE)
    
    # store results
    migbehav[i,1] <- indiv # IndivHRYr
    migbehav[i,2] <- vi95[2,1]
    migbehav[i,3] <- vi50[2,1]
    # migbehav[i,2] <- area[which(area$id == "Winter"),2] # km2Win
    # migbehav[i,3] <- area[which(area$id == "Summer"),2] # km2Sum
    # migbehav[i,4] <- vi95[2,1] # VI95
    # migbehav[i,5] <- vi50[2,1] # VI50
    
  }
  
##### classify individual behavior ####
  
  # resident if core use areas intersect
  # migrant if no use areas intersect
  # intermediate if hrs intersect but core use areas don't
  migbehav <- migbehav %>%
    mutate(MigStatus = ifelse(VI50 > 0, "Resident",
                              ifelse(VI95 == 0, "Migrant",
                                     "Intermediate")),
           Mig = ifelse(MigStatus == "Migrant", 1, 0),
           Int = ifelse(MigStatus == "Intermediate", 1, 0),
           Res = ifelse(MigStatus == "Resident", 1, 0))
    

  

write.csv(migbehav, file = "mig-behav.csv", row.names = F)  
  



### ### ### ### ### ### ### ### ### ### ### ### ### ### 
####                  IN PROGRESS                  ####
### ### ### ### ### ### ### ### ### ### ### ### ### ###

## commented out code above
  # is intended to also calculate and store home range areas
  # but it fails after only 3 individuals
  # and i'm not sure if that's just because the winter HR is too small
  # due to the fact that i'm just using the capture year for now
  # or if it's some other issue
  # but regardless, the VI part works and that's the important thing
  # so in the interest of time i'm dropping area estimation for now