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



#### RAW DATA FROM ACCESS DATABASE ####

collardays <- sqlQuery(channel, paste("select * from GPS_Herd_TimePeriod"))
rawcap <- sqlQuery(channel, paste("select * from CaptureInfo_allelk"))
rawlocs <- sqlQuery(channel, paste("select * from ElkGPS"))


#### DATA PER POPULATION ####


# herds that have at least 1 and 2 yrs of collar data ####

    n365 <- filter(collardays, CollarDays >= 365)
    length(unique(n365$Herd)) # 19 herds with at least 1 year of data for at least 1 elk
    
    n730 <- filter(collardays, CollarDays >= 730)
    length(unique(n730$Herd)) # 10 herds with at least 2 yrs of data for at least 1 elk
    # since moving to 2 yrs of data cuts number of herds nearly in half, prob will only use 1 yr

    
# indivduals in each herd with 1 or 2 years of data ####
    
    i365 <- n365 %>%
      group_by(Herd) %>%
      count(nIndiv = n_distinct(AnimalID)) %>%
      ungroup() 
    # range 1 - 48 (11 have >= 10)

    i730 <- n730 %>%
      group_by(Herd) %>%
      count(nIndiv = n_distinct(AnimalID)) %>%
      ungroup()     
    # yeeeeah, definitely not getting more than 1 year per herd
    
    
# herds with at least 1 yr of data (to filter from larger db) ####
    
    herds <- data.frame(Herd = unique(n365$Herd))
    
    
# check whether we're losing herds with collardays in the 300s (but below 365)
      
    hm <- collardays %>%
      filter(CollarDays >299 & CollarDays <= 365) %>%
      anti_join(hm, n365, by = "Herd")
    # excellent, not losing any herds in the 300s 
    # making 315 the generic cutoff because that's the fewest days between
        # a capture event and the subsequent winter
        # (so all indivs should have a chance to return to winter range)
    

# populations and years of interest ####
    
    popnyrs <- collardays %>%
      filter(CollarDays >= 315) %>%
      group_by(Herd) %>%
      summarise(FirstDate = min(MinOfDate),
                LastDate = max(MaxOfDate),
                nIndiv = n_distinct(AnimalID)) %>%
      ungroup() %>%
      filter(nIndiv >= 10) %>%
      mutate(Yr1 = as.numeric(substr(FirstDate, 0, 4)),
             Yr2 = Yr1+1,
             YrEnd = as.numeric(substr(LastDate, 0, 4))) %>%
      arrange(nIndiv) %>%
      filter(YrEnd > 2006) # no NDVI data prior to 2005
    write.csv(popnyrs, file = "popns-yrs.csv", row.names = F)

    herds <- data.frame(Herd = popnyrs$Herd)
    write.csv(herds, file = "herds.csv", row.names = F)
    
    sum(yrs$nIndiv)
    nrow(yrs)
    # 457 individuals across 15 populations :) (but incls males etc)

  
# summary capture information per population of interest ####

    # capture dates per popn (to define start date of analysis for indivs)
    capdates <- rawcap %>%
      filter(!is.na(CaptureDate)) %>%
      semi_join(herds, by = "Herd") %>%
      mutate(CapYr = substr(CaptureDate, 0, 4)) %>%
      group_by(Herd, CapYr) %>%
      summarise(LastCapDate = max(CaptureDate)) %>%
      ungroup()
    
    # capture dates only for indivs of interest in popns of interest
    capindivs <- rawcap %>%
      filter(!is.na(CaptureDate)) %>%
      semi_join(herds, by = "Herd") %>%
      filter(Sex == "F") %>%
      select(AnimalID, CollarID, Herd, CaptureArea, CaptureDate, Age, Age_Type, GPSdata)
    write.csv(capindivs, file = "prelim-capindivs.csv", row.names=F)
      
    
#### DATA PER INDIV ####
    
    #### ** in progress ** ####
    
    # read in data created above (if skipping from raw data to here #
    
    popnyrs <- read.csv("popns-yrs.csv")
    herds <- read.csv("herds.csv")
    capindivs <- read.csv("prelim-capindivs.csv")

    # only females in popns of interest ####
    allindivs <- rawlocs %>%
      semi_join(herds, by = "Herd") %>% # only popns of interest
      filter(Sex == "F") %>% # only females
      select(-c(Herd, CollarID)) %>% # avoid duplicate columns
      right_join(capindivs, by = "AnimalID") %>% # add capture info
      mutate(CapYr = substr(CaptureDate, 0, 4)) %>%
      filter(Date <= paste0(CapYr, "-12-31")) %>%
      mutate(Month = substr(Date, 6, 7)) 

    #### YOU NEED TO SOMEHOW
      # use the INFO above to pull only locs from yr1 for each indiv
      # but group_by will fuck it up because you'll lose all your locations
      # so figure out a way to remove individuals that have locations in dec during capture year
      # without losing their actual locations from the df above

      
    
    # number of locations per day per indiv ####

      
      
      
#### CHECKING THINGS OUT ####

# testcode: pulling month from posix date [in dplyr pipe]
dats <- sample(indivs$Date, 10)
mos <- dats$month

# any december captures? (to define year1 properly)
  unique(as.Date(collardays$MinOfDate)) # whoa, yep, even november.
    
    
    
    
    
        
# OLD CODE ####    
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




