### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#           FORMATTING STATEWIDE ELK GPS DATA             #
#    TO ASSESS FACTORS INFLUENCING MIGRATORY BEHAVIOR     #
#                   KRISTIN BARKER                        #
#                    OCTOBER 2017                         #
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###




### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###



#### packages ####

  library(RODBC) # for connecting to access
  library(dplyr) # for joins and general awesomeness



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
                                dbq=C:/Users/kristin/Documents/DatabasesEtc/Statewide/Elk/Statewide_Elk_GPS.accdb")
  } else {  cat("Maybe you shouldn't have been so lazy when you made this code") }
  rm(wd_workcomp, wd_laptop, wd_worklaptop)


  
  
### ### ### ### ### ###
####  |DATA PREP|  ####
### ### ### ### ### ###
  

  
#### raw data from access database ####

  rawcap <- sqlQuery(channel, paste("select * from CaptureInfo_allelk"))
  rawlocs <- sqlQuery(channel, paste("select * from ElkGPS"))
  

  
#### alterations/fixes prior to actual summaries and analyses ####

  
  ## individual location data ##

  rmelk <- filter(rawcap, CaptureArea == "Ski Hill") # this also removes NAs
  allcowlocs <- rawlocs %>%
    # remove bulls, missing locations, and "ski hill" elk
    filter(Sex == "F" & !is.na(Latitude)) %>%
    anti_join(rmelk, by = "AnimalID") %>%
    # remove trailing whitespace from Madison herd name
    mutate(Herd = trimws(Herd)) %>%
    # rename S Pioneers and W Pioneers as Pioneers; rename Border to Clarks Fork
    mutate(Herd = ifelse(grepl("Pioneer", Herd), "Pioneers",
                         ifelse(Herd == "Border", "Clarks Fork", Herd))) %>%
    # format date-times (DT had NAs; replace with properly formatted DateTime)
    select(-DT) %>% 
    within(Date <- as.Date(Date)) %>%
    within(Time <- strftime(Time, format="%H:%M:%S")) %>%
    mutate(Year = substr(Date, 0, 4), 
      Month = as.numeric(substr(Date, 6, 7))) %>%
    mutate(DateTime = paste(Date, Time, sep = " ")) 
  write.csv(allcowlocs, file = "locs-allcows.csv", row.names = F)  
  # allcowlocs <- read.csv("locs-allcows.csv") 
  
  
  ## capture data ##
  
  fixedcap <- rawcap %>%
    # remove trailing whitespace from Madison herd name
    mutate(Herd = trimws(Herd)) %>%
    # rename S Pioneers and W Pioneers as Pioneers; rename Border to Clarks Fork
    mutate(Herd = ifelse(grepl("Pioneer", Herd), "Pioneers",
                         ifelse(Herd == "Border", "Clarks Fork", Herd))) %>%
    filter(AnimalID != "") %>%
    # remove "ski hill" elk
    anti_join(rmelk, by = "AnimalID")
  write.csv(fixedcap, file = "capdat-allindivs.csv", row.names = F)
  # fixedcap <- read.csv("capdat-allindivs.csv")
  
  
  # close database connection
  odbcCloseAll()
  
  

  
### ### ### ### ### ###
####  |DATA WORK|  ####
### ### ### ### ### ###


  
####  POPULATIONS  ####
  
  
  # identify populations (and years) to include in analysis
  
  popnyrs <- allcowlocs %>%
    # only incl indivs with locs spanning entire yr (thru dec)
    group_by(AnimalID, Year) %>%
    # month>8 accts for e/w fork captures starting in nov
    filter(max(Month == 12) & dplyr::n_distinct(Month > 8)) %>%
    ungroup() %>%
    # count number indivs per herd per yr
    group_by(Herd, Year) %>%
    summarise(nIndiv = n_distinct(AnimalID)) %>%
    ungroup() %>%
    # only consider second yr of data for hd314 (cap too late to estimate winHR yr1)
    filter(Herd != "HD314" | (Herd == "HD314" & Year != 2009)) %>%
    # identify yr with most indivs
    group_by(Herd) %>%
    filter(nIndiv == max(nIndiv),
           nIndiv > 2, # 2 indivs per grp not enough
           Year > 2004) %>% # need 5+ yrs NDVIamp/ti per popn
    ungroup() 
  write.csv(popnyrs, file = "popns-yrs.csv", row.names = F)
  sum(popnyrs$nIndiv) # print total indivs
  # popnyrs <- read.csv("popns-yrs.csv")
    

  # summary capture info for each popn ####
  
  capdates <- fixedcap %>%
    filter(!is.na(CaptureDate) & Sex == "F") %>%
    # format date; determine month of capture
    mutate(CaptureDate = as.Date(CaptureDate)) %>%
    mutate(CaptureMonth = as.numeric(substr(CaptureDate, 6, 7))) %>%
    mutate(Year = as.integer(substr(CaptureDate, 0, 4))) %>%
    # only keep herds and years of interest
    semi_join(popnyrs, by = c("Herd")) %>%
    # account for e/w fork captures starting in nov for subsequent yr
    filter(CaptureMonth < 10) %>%
    # determine first and last capture date per popn
    group_by(Herd, Year) %>%
    summarise(FirstCapDate = min(CaptureDate),
              LastCapDate = max(CaptureDate),
              nIndiv = n_distinct(AnimalID)) %>% # just for general idea
    ungroup()
  sum(capdates$nIndiv) # note incls more indivs than we can actually use
  write.csv(capdates, file = "popn-capdates.csv", row.names=F)
  # capdates <- read.csv("popn-capdates.csv")
    
    
  
####  INDIVIDUALS ####
  
  
  # remove locs collected during capture of that popn
  # but keep all others (even outside year of interest)
     
  locs <- allcowlocs %>%
    mutate(Date = as.Date(Date)) %>%
    # only use locs from popns and yrs of interest
    semi_join(popnyrs, by = c("Herd")) %>%
    # avoid duplicate column
    select(-Year) %>%
    # add capture date info
    left_join(capdates, by = c("Herd")) %>%
    # remov locs collected during capture
    mutate(LastCapDate = as.Date(LastCapDate)) %>%
    filter(Date > LastCapDate) 
  write.csv(locs, file = "locs.csv", row.names = F)
  # locs <- read.csv("locs.csv")

  
  # ## -skip spot- ####
  #   rm(channel)
  #   fixedcap <- read.csv("capdat-allindivs.csv")
  #   popnyrs <- read.csv("popns-yrs.csv")
  #   locs <- read.csv("locs.csv")

  # difference between number of indivs of interest and indivs in popns of interest
  sum(popnyrs$nIndiv)
  length(unique(locs$AnimalID))
  length(unique(locs$AnimalID)) - sum(popnyrs$nIndiv) # diff of 101 indivs
  
  # identify individuals of interest
  indivs <- data.frame(AnimalID = unique(locs$AnimalID))
  
  # individual data - herd, age during year of interest, some capture info
  indivcap <- fixedcap %>%
    left_join(popnyrs, by = "Herd") %>%
    # only keep original capture info for indivs captured >1x
    group_by(AnimalID) %>% 
    arrange(CaptureDate) %>% 
    slice(1) %>%  
    ungroup() %>%
    # remove stored data about herds not to be used in analysis
    mutate(Herd = factor(Herd)) %>%
    # determine animal age during year of interest
    rename(CaptureAge = Age) %>%
    mutate(Year = as.integer(Year),
           CaptureYear = as.integer(substr(CaptureDate, 0, 4)),
           Age = CaptureAge + (Year - CaptureYear)) %>%
    # create dummary variable for whether indiv is >10yrs old
    mutate(Old = as.factor(ifelse(Age >= 10, 1, 0))) %>%
    # clean up dataframe
    dplyr::select(AnimalID, Herd, CaptureArea, CaptureDate, CaptureYear, 
           Serology, Age_Type, Age, Old)
  write.csv(indivcap, "indiv-dat.csv", row.names = F)
  # indivcap <- read.csv(indiv-dat.csv")
    

    

    
    

  


  

