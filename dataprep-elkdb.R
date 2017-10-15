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
                                dbq=C:/Users/kristin/Documents/DatabasesEtc/Statewide_Elk_GPS.accdb")
  } else {  cat("Maybe you shouldn't have been so lazy when you made this code") }
  rm(wd_workcomp, wd_laptop, wd_worklaptop)


  
  
### ### ### ### ### ###
####  |DATA PREP|  ####
### ### ### ### ### ###
  

  
#### raw data from access database ####

  rawcap <- sqlQuery(channel, paste("select * from CaptureInfo_allelk"))
  rawlocs <- sqlQuery(channel, paste("select * from ElkGPS"))
  

  
#### alterations/fixes prior to actual summaries and analyses ####

  
  # remove indivs that should not be considered in popn HR estimation
  # ("ski hill" elk from sapphires and bulls from any population)
  rmelk <- filter(rawcap, CaptureArea == "Ski Hill") # this also removes NAs
  allcowlocs <- rawlocs %>%
    filter(Sex == "F" & !is.na(Latitude)) %>%
    anti_join(rmelk, by = "AnimalID")
  write.csv(allcowlocs, file = "locs-allcows.csv", row.names = F)  
  
  
  # remove trailing whitespace from Madison herd name;
  # identify South Pioneers and West Pioneers as Pioneers;
  # remove "ski hill" elk
  fixedcap <- rawcap %>%
    mutate(Herd = trimws(Herd)) %>%
    mutate(Herd = ifelse(grepl("Pioneer", Herd), "Pioneers", Herd)) %>%
    filter(AnimalID != "") %>%
    anti_join(rmelk, by = "AnimalID")
  write.csv(fixedcap, file = "capdat-allindivs.csv", row.names = F)
  
  

  
### ### ### ### ### ###
####  |DATA WORK|  ####
### ### ### ### ### ###


  
####  Identify populations and years of interest  ####
  
  popnyrs <- rawlocs %>%
    # remove bulls, NA locations, and ski hill elk
    filter(Sex =="F" & !is.na(Latitude)) %>% 
    anti_join(rmelk, by = "AnimalID") %>%
    # format and create date/time/year/month columns
    within(Date <- as.Date(Date)) %>%
    within(Time <- strftime(Time, format="%H:%M:%S")) %>%
    mutate(Year = substr(Date, 0, 4), 
           Month = as.numeric(substr(Date, 6, 7))) %>%
    # only incl indivs with locs spanning entire yr (thru dec)
    group_by(AnimalID, Year) %>%
    # month>8 accts for e/w fork captures starting in nov
    filter(max(Month == 12) & dplyr::n_distinct(Month > 8)) %>%
    ungroup() %>%
    # count number indivs per herd per yr
    group_by(Herd, Year) %>%
    summarise(nIndiv = n_distinct(AnimalID)) %>%
    ungroup() %>%
    group_by(Herd) %>%
    # identify yr with most indivs
    filter(nIndiv == max(nIndiv),
           nIndiv > 2, # 2 indivs per grp not enough
           Year > 2004) %>% # need 5+ yrs NDVIamp/ti per popn
    ungroup() 
  write.csv(popnyrs, file = "popns-yrs.csv", row.names = F)
  sum(popnyrs$nIndiv) # print total indivs
    

  
####  Identify individuals and locations of interest ####

  
  # determine last date of capture for each popn
  capdates <- fixedcap %>%
    filter(!is.na(CaptureDate)) %>%
    mutate(Year = substr(CaptureDate, 0, 4)) %>%
    # only for popns and years of interest
    semi_join(popnyrs, by = c("Herd", "Year")) %>%
    # determine month of capture
    mutate(CaptureMonth = as.numeric(substr(CaptureDate, 6, 7))) %>%
    # determine last date of capture for each popn
    group_by(Herd) %>%
    # account for e/w fork captures starting in nov for subsequent yr
    filter(CaptureMonth < 10) %>%
    summarise(LastCapDate = max(CaptureDate)) %>%
    ungroup()
    

  
  # only keep locs not collected during capture of that popn
  locs <- rawlocs %>%
    filter(Sex == "F" & !is.na(Latitude)) %>%
    anti_join(rmelk, by = "AnimalID") %>%
    # format and create date/time/year/month columns
    within(Date <- as.Date(Date)) %>%
    within(Time <- strftime(Time, format="%H:%M:%S")) %>%
    mutate(Year = substr(Date, 0, 4),
           Month = as.numeric(substr(Date, 6, 7))) %>%
    # only use locs from popns and yrs of interest
    semi_join(popnyrs, by = c("Herd", "Year")) %>%
    # add capture date info
    left_join(capdates, by = "Herd") %>%
    # remov locs collected during capture
    mutate(LastCapDate = as.Date(LastCapDate)) %>%
    filter(Date > LastCapDate) %>%
    # remove indivs who didn't make it through complete yr
    group_by(AnimalID) %>%
    mutate(MaxMonth = max(Month)) %>%
    ungroup() %>%
    filter(MaxMonth > 11)
  write.csv(locs, file = "locs.csv", row.names = F)

  # sanity check- verify capture and collar data id'd same #indivs
  length(unique(locs$AnimalID)); sum(popnyrs$nIndiv)  # yes, n=356
  

  # filter capture data to only include individuals of interest
  # and only keep original capture data for those who were recaptured
  indivs <- data.frame(AnimalID = unique(locs$AnimalID))
  allcap <- fixedcap %>%
    semi_join(indivs, by = "AnimalID") %>%
    arrange(AnimalID, CaptureDate)
  cap <- allcap[!duplicated(allcap$AnimalID),]
  write.csv(cap, file = "capdat.csv", row.names = F)
  

# POSSIBLE ADDITIONAL TASKS TO INCLUDE HERE
  
  # create dataframe of indiv, ageDURINGYEAR OF INTEREST, popn, year?
  
