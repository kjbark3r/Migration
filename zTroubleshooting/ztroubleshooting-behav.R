### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
###            CHECKING OUT SPITZ'S MIGRATER            ###
###  TEST CODE INCLUDING DATA PREP & PRELIM ANALYSES    ###
###              KRISTIN BARKER 2017-2018               ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###



### GENERAL PLAN: Work through the vignette with only NSapph data ###




### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###



  #### Packages ####
  
  
  library(RODBC) # possibly unnecessary; for connecting to access
  library(migrateR) # for, y'know, this entire analysis
  library(raster) # for elevation raster 
  library(dplyr) #i just assume i'll use this several times
  
  
  # # delete below if unnecessary
  # library(sp) #for kernel centroid estimate
  # library(adehabitatHR) #for kernel centroid estimate
  # library(rgdal) #for latlong/stateplane conversions
  # library(gsubfn)
  # library(maptools) #for writeSpatialShape
  
  
  
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
  
  
  
  #### Database connection ####
  
  if(file.exists(wd_worklaptop)) {
    channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                dbq=C:/Users/kristin/Documents/DatabasesEtc/Statewide/Elk/Statewide_Elk_GPS.accdb")
  } else {  cat("Maybe you shouldn't have been so lazy when you made this code") }

  
  
  #### Projections ####
  
  latlong <- CRS("+init=epsg:4326")
  stateplane <- CRS("+init=epsg:2818") #this is NAD83
  utm <- CRS("+init=epsg:3742") # NAD83(HARN)/UTMzone12N
  
  
  #### "Raw" data ####
  
  rawlocs <- sqlQuery(channel, paste("select * from ElkGPS"))
  elev <- raster("../Vegetation/writtenrasters/orig/elev.tif")
  
  
  

      # pull 10 random rows to play with for little tests
      
      play <- rawlocs[sample(nrow(trawlocs), 10), ]
      head(play)   
      
      
      
      # keep it clean
      odbcCloseAll()
      rm(wd_workcomp, wd_laptop, wd_worklaptop, channel)
  

### ### ### ### ### ###
####  |DATA PREP|  ####
### ### ### ### ### ###

  

  #### Subset and format data ####

  
  # Prep original data
  testlocs <- rawlocs %>%
    # only use Sapph for this trial run
    filter(Herd == "Sapphire") %>%
    # prep datetime
    within(DT = as.POSIXct(DT, format = "%Y-%m-%d"),
           Time <- substr(Time, 12, 19)) %>%
    # get rid of stored info about removed indivs
    mutate(AnimalID = factor(AnimalID)) %>%
    # create POSIXct DateTime for ltraj obj
    mutate(Date = as.POSIXct(paste(DT, Time, sep = " "),
                             format = "%Y-%m-%d %H:%M:%S")) %>%
    # randomly select one loc per day per indiv
    group_by(AnimalID, DT) %>%
    sample_n(1) %>%
    ungroup()

  

  # Convert Lat/Longs to UTMs
  testlocs <- as.data.frame(spTransform(SpatialPointsDataFrame(
                            data.frame("X" = testlocs$Longitude, 
                                       "Y" = testlocs$Latitude), 
                            testlocs, proj4string = latlong), utm))


      
  # Create ltraj object
  lt <- as.ltraj(xy = testlocs[,c("X", "Y")], 
                 date = testlocs$Date, 
                 id = testlocs$AnimalID)
  
  # Sweet baby jesus
  lt
  
  
  # Fit NSD movement model 
  mod.nsd <- mvmtClass(lt)
  mod.nsd
  
  # holy balls
  # although it's like 90% mixed mig
  # which could weither be due to them being more elvational
  # or something wonky with automatical parameters
  
  # check out instances where not all mod.nsdels were fit
  length(which(!fullmvmt(mod.nsd))) # 17 indivs 
  which(!fullmvmt(mod.nsd)) # this is them
  fullmvmt(mod.nsd, out = "name") # these are the ones they DID fit
  
  
  # play with tweaking parameters
  mod.nsd2 <- mvmtClass(lt, p.est = pEst(s.d = 1))
  mod.nsd2
  length(which(!fullmvmt(mod.nsd2))) # up to 26 failures
  # this gives a bunch of dispersers
  
  mod.nsd3 <- mvmtClass(lt, p.est = pEst(s.d = 2))
  mod.nsd3
  length(which(!fullmvmt(mod.nsd3))) # now 38
  
  # y'know, these ladies are kinda weird
  # maybe it makes the most sense to check elevational stuff?
  
  #### check out plots ####
  plot(mod.nsd)
  # eeee so fun

  
  
  
  
  
  
  
  
    
  #### Try elevational [IN PROGRESS - NEED ELEVS, DUH] #### 
  
  # Fit NSD movement model 
  mod.elv <- mvmtClass(lt, fam = "elev")
  mod.elv
  
  # holy balls
  
  # check out instances where not all mod.elvels were fit
  length(which(!fullmvmt(mod.elv))) # 17 indivs 
  which(!fullmvmt(mod.elv)) # this is them
  fullmvmt(mod.elv, out = "name") # these are the ones they DID fit
  
  

  
  
  
  
  

  ## ADDITIONAL TASKS ##
  
    # figure out subsetting #locs/indiv
      # want equal sampling effort and dec'd data dependence




  
  
### ### ### ###
## TO DELETE ##
### ### ### ###
  
  
  # figuring out the ltraj game
  data(puechabonsp)
  str(puechabonsp)
  str(puechabonsp$relocs)
  str(testlocs)
  
  
  # find duplicate dates in bursts
  hm <- testlocs %>%
    group_by(AnimalID) %>%
    filter(duplicated(Date)) %>%
    ungroup()
  
  
  # remove odd duplicate locs from some collars (use 1st from burst)
  group_by(AnimalID, Date) %>%
    filter(row_number() == 1) %>%
    ungroup()