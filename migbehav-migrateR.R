### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
###       CLASSIFYING MIGRATORY BEHAVIOR OF ELK         ###
###             USING THE MIGRATER PACKAGE              ###
###              KRISTIN BARKER 2017-2018               ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###



# This code uses Derek Spitz's migrateR package
# to classify elk migratory behavior as
# migratory, resident, or something else based on NSD



### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###



  #### Packages ####
  
  
    library(migrateR) 
    library(dplyr) 
  

  
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
    rm(wd_workcomp, wd_laptop, wd_worklaptop)
  

  
  #### Projections ####
  
    latlong <- CRS("+init=epsg:4326") # elk GPS collars
    utm <- CRS("+init=epsg:3742") # NAD83(HARN)/UTMzone12N
  
  
  
  #### "Raw" data ####
  
    rawlocs <- read.csv("locs-allcows-withelevs.csv")
    herds <- read.csv("popns-yrs.csv")
    herdsonly <- select(herds, Herd)

  

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
      

### ### ### ### ### ###
####  |DATA PREP|  ####
### ### ### ### ### ###


  
  #### Subset and format raw data for use in models ####
    
    modlocs <- rawlocs %>%
      # only consider populations of interest
      semi_join(herdsonly, by = c("Herd")) %>%
      # create POSIXct DateTime for ltraj object; pull just Date from this
      mutate(Date = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")) %>%
      mutate(Day = as.Date(DateTime)) %>%
      # remove stored factor levels that include removed indivs and popns
      mutate(AnimalID = factor(AnimalID), Herd = factor(Herd)) %>%
      group_by(AnimalID) %>%
      # identify 1st date of data
      mutate(Day1 = min(as.Date(DateTime))) %>%
      ungroup() %>%
      # only include 1st full yr of data, plus extra month for full return to winter
      filter(Day <= Day1 + 395) %>% 
      # remove stored factor levels that include removed indivs
      mutate(AnimalID = factor(AnimalID)) %>%
      # randomly select one loc per day per indiv
      group_by(AnimalID, Day) %>%
      sample_n(1) %>%
      ungroup() %>%
      # only indivs that had time to get back to winter range
      group_by(AnimalID) %>%
      filter(n() > 275) %>%
      ungroup() 
    
    # remove stored factor levels and Date NAs
    modlocs <- droplevels(modlocs)
    modlocs <- filter(modlocs, !is.na(Date))
    
    
    # identify indivs
    modindivs <- data.frame(AnimalID = unique(modlocs$AnimalID))
   
  
   
  #### Convert Lat/Longs to UTMs ####
    modlocs <- as.data.frame(spTransform(SpatialPointsDataFrame(
                              data.frame("X" = modlocs$Longitude, 
                                         "Y" = modlocs$Latitude), 
                              modlocs, proj4string = latlong), utm))


    
  #### Create ltraj object ### 
    lt <- as.ltraj(xy = modlocs[,c("X", "Y")], 
                   # note date must be POSIXct
                   date = modlocs$Date, 
                   # specify indiv (also serves as default burst)
                   id = modlocs$AnimalID,
                   # specify infolocs to allow elevational mign model
                   infolocs = data.frame(elev = modlocs$Elev))
    
    
    
 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
    
    
### ### ### ### ### ###
####    |MODELS|   ####
### ### ### ### ### ###     

    
    # identify best starting location for each indivdual
    rlocs <- findrloc(lt)
 
    # define base model with no tweaks, using best starting loc
    mb <- mvmtClass(lt, rloc = rlocs$rloc)
    length(which(!fullmvmt(mb))) # 48 convergence issues
    length(which(!fullmvmt(mb)))/length(fullmvmt(mb)) # ~12% of indivs
    
    # refine model to reduce convergence issues (starting dist 0.5km, min duration 2wks)
    mr <- refine(mb, p.est = pEst(l.r = 14, s.d = 0.5))
    all(fullmvmt(mr))
    length(which(!fullmvmt(mr)))/length(fullmvmt(mr)) # only 4% now

    # omit mixed-migrant model and require 2mo duration on range2
    mot2 <- topmvmt(mr, omit = "mixmig", mrho = 60)
    attributes(mot2) 
        
    # store results
    rslts <- data.frame(attributes(mot2)) %>%
      rename(AnimalID = burst, Behav = names) 
	  summary(rslts)
	  

	  
	  ##############################
	  # for comparison, model using weird -1 starting delta
	      # identify best starting location for each indivdual
    
    # refine model to reduce convergence issues (starting dist -1)
    testmr <- refine(mb, p.est = pEst(s.d = -1))
    all(fullmvmt(testmr))

    # omit mixed-migrant model, require 2mo duration on range2, make min dist positive
    testmot2 <- topmvmt(testmr, omit = "mixmig", mrho = 60, mdelta = 0.01)
    attributes(testmot2) 
        
    # store results
    testrslts <- data.frame(attributes(testmot2)) %>%
      rename(AnimalID = burst, BehavNegSd = names) 
	  summary(testrslts)
	  
	  comp <- full_join(rslts, testrslts, by = "AnimalID") %>%
	    mutate(Samesies = ifelse(Behav == BehavNegSd, T, F))
	  
	  # check discrepancies
	  plot(mr[[3]]) # ah, she's the one who ran away after capture
	  
	  # BROOT130035 - not in gis shp
	  discrep <- filter(modlocs, AnimalID == "BROOT130035")
	  # ok, she has the correct amount/duration of locations
	  plot(mr[[281]]) # made one super short trip, maybe just shy of 2 mo
	  
	  
	  write.csv(rslts, file = "behav-classn-nsd-prelim.csv", row.names = F)

	  
	  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
    
    
### ### ### ### ### ###
####   |VISUALS|   ####
### ### ### ### ### ###     

		
	  #### In progress --
	  #### You want to make a grid of these and try to plot altogether
	  # programmatically defining the number of rows and columns
	  # maybe splitting by herd first with a loop?
	  
	  ### you will need to compare visual estimations against classifications
	  
	  plot(mb)
		# spatmig(lt, mb)
		  


