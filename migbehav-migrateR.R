### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
###       CLASSIFYING MIGRATORY BEHAVIOR OF ELK         ###
###             USING THE MIGRATER PACKAGE              ###
###              KRISTIN BARKER 2017-2018               ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###



# This code uses Derek Spitz's migrateR package
# to classify elk migratory behavior as
# migratory, resident, or something else based on rNSD



### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###



  
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
  

    
    
  #### Packages ####
  
  
    library(migrateR) 
    source("NSDresults/test_plotmvmt2.R")
    library(dplyr) 
    
    
    
  # until i get the models figured out...
  # load this and skip to |MODELS|
  load("nsd-baselocs.RData")
  
  
  
  #### Projections ####
  
    latlong <- CRS("+init=epsg:4326") # elk GPS collars
    utm <- CRS("+init=epsg:3742") # NAD83(HARN)/UTMzone12N
  
  
  
  #### "Raw" data ####
  
    rawlocs <- read.csv("locs.csv")
    herds <- read.csv("popns-yrs.csv")
    herdsonly <- select(herds, Herd)

  

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
      

### ### ### ### ### ###
####  |DATA PREP|  ####
### ### ### ### ### ###


  
  #### Subset and format raw data for use in models ####
    
    modlocs <- rawlocs %>%
      # only consider populations of interest and locations with dates
      semi_join(herdsonly, by = "Herd") %>%
      # remove HD314 from consideration because captured too late to estimate winterHR
      filter(Herd != "HD314") %>%
      # create POSIXct DateTime for ltraj object; pull just date (Day) from this
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
      # only include indivs with locations during the year of interest
      # who also had time to get back to winter range
      mutate(LocYr = substr(Day, 1, 4), # year of that collar location
             YrMatch = ifelse(LocYr == Year, 1, 0)) %>% # 1 if matches yr of interest
      group_by(AnimalID) %>%
      filter(sum(YrMatch) > 275) %>% # appx 9 mo (thru dec)
      ungroup() %>%
      # do NOT specify format with time to avoid daylight savings time NAs 
      mutate(Date = as.POSIXct(DateTime))

    
  #### Identify indivs ###
    modindivs <- data.frame(AnimalID = unique(modlocs$AnimalID))
   
  
   
  #### Convert Lat/Longs to UTMs ####
    modlocs <- as.data.frame(spTransform(SpatialPointsDataFrame(
                              data.frame("X" = modlocs$Longitude, 
                                         "Y" = modlocs$Latitude), 
                              modlocs, proj4string = latlong), utm))
    modlocs <- droplevels(modlocs) # because this somehow keeps happening
    modlocs$AnimalID <- as.character(modlocs$AnimalID) # maybe this'll fix it
    # write.csv(modlocs, "locs.csv", row.names=F)


    
  #### Create ltraj object ### 
    lt <- as.ltraj(xy = modlocs[,c("X", "Y")], 
                   # note Date must be POSIXct
                   date = modlocs$Date, 
                   # specify indiv 
                   id = modlocs$AnimalID)
    
    

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
    
    
### ### ### ### ### ###
####    |MODELS|   ####
### ### ### ### ### ###     

    
    # identify best starting location for each indivdual
    rlocs <- findrloc(lt)
    
    
    
#### store ####
save.image(file = "nsd-baselocs.RData") 
    
    
    
    # expand default duration on summer range to allow up to 8 months (default was 84 days)
    dur8 <- pEst(u.r = 240)  

    
    # define base model, rNSD with expanded duration parameter 
    mbase <- mvmtClass(lt, p.est = dur8, rloc = rlocs$rloc)
    length(which(!fullmvmt(mbase))) # 26 convergence issues
    
    
    # refine base model to address convergence issues #
    
      # allow up to 8km daily displacement within the same resident range
      uk64 <- pEst(u.k = log(64))
      mref1 <- refine(mbase, p.est = uk64)
      length(which(!fullmvmt(mref1))) # 12 convergence issues

       
      # migrant only has to move 50 km2 (to incl short-distance migrants)
      ld50 <- pEst(u.r = 240, l.d = 50)
      mref2 <- refine(mref1, p.est = ld50)
      length(which(!fullmvmt(mref2))) # 7 remaining convergence issues

      
    # identify top model for each individual #
      
      # require 2 months on summer range; require move 5km
      mtop <- topmvmt(mref2, omit = "mixmig", mrho = 60, mdelta = 25)
      topmods <- data.frame(AnimalID = modindivs, MigClassn = names(mtop))
      write.csv(topmods, file = "./rNSDresults/initialclassns.csv", row.names=F)

    
    
    # summarize and store results
    rslts <- data.frame(attributes(mtop)) %>%
      rename(AnimalID = burst, Behav = names) 
	  summary(rslts)
	  write.csv(rslts, file = "behav-classn-nsd-prelim.csv", row.names = F)

	  

	  

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
    
    
### ### ### ### ### ###
####   |VISUALS|   ####
### ### ### ### ### ###     

	  
	  
	  #### behavioral classification plots ####

      num.plots <- nrow(modindivs)
      my.plots <- vector(num.plots, mode='list')
      
      for(i in 1:num.plots) {
        plot(mref2[[i]])
        my.plots[[i]] <- recordPlot()
      }
      graphics.off()
      
      pdf('./rNSDresults/migbehav-plots.pdf', onefile = TRUE)
      for(my.plot in my.plots) {
        replayPlot(my.plot)
      }
      graphics.off()
      
      
      
  #### checking location classifications on some indiv plots ####
      
      i140400 <- which(modindivs$AnimalID == 140400)
      spatmig(lt[i140400], mref2[i140400])
      spatmig(lt[i140400], mref2[i140400], mod = "disperser")
      
      
      i140630 <- which(modindivs$AnimalID == 140630)
      spatmig(lt[i140630], mref2[i140630])
      spatmig(lt[i140630], mref2[i140630], mod = "disperser")  

      
      i140710 <- which(modindivs$AnimalID == 140710)
      spatmig(lt[i140710], mref2[i140710])
      spatmig(lt[i140710], mref2[i140710], mod = "disperser")  
      
      
      i140890 <- which(modindivs$AnimalID == 140890)
      spatmig(lt[i140890], mref2[i140890])
      spatmig(lt[i140890], mref2[i140890], mod = "disperser")  
      
      
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
    
    
### ### ### ### ### ### ##
####    |SUMMARIES|   ####
### ### ### ### ### ### ##
	  
	  
	  indivpops <- modlocs %>%
	    select(AnimalID, Herd) %>%
	    distinct()
	  
	  behav <- left_join(rslts, indivpops, by = "AnimalID") %>%
	    rename(BehavNSD = Behav) %>%
	    dplyr::select(Herd, AnimalID, BehavNSD)
	  write.csv(behav, "behav-nsd.csv", row.names=F)
	  
	  popbehav <- behav %>%
	    group_by(Herd) %>%
	    summarise(nIndivs = n(),
	              nMig = length(which(Behav == "migrant")),
	              nRes = length(which(Behav == "resident")),
	              nDisp = length(which(Behav == "disperser")),
	              nNom = length(which(Behav == "nomad")),
	              nOther = nDisp + nNom)
	  
	  popbehavsum <- popbehav %>%
	    mutate(ppnMig = round(nMig/nIndivs, digits = 2),
	           ppnRes = round(nRes/nIndivs, digits =2),
	           ppnOther = round(nOther/nIndivs, digits = 2)) %>%
	    dplyr::select(Herd, ppnMig, ppnRes, ppnOther, nIndivs)
    
    
    
    
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
    
    
### ### ### ### ### ### ###
####    |PARAMETERS|   ####
### ### ### ### ### ### ###    

    
	  # extract parameters from all models
    paramsall <- mvmt2df(mot2)
    paramsall
    
    # dispersers
    paramsdisp <- paramsall[[1]] 
    paramsdisp$AnimalID <- rownames(paramsdisp)
    
    # migrants
    paramsmig <- paramsall[[2]]
    paramsmig$AnimalID <- rownames(paramsmig)
    
    # nomads
    paramsnom <- paramsall[[3]]
    paramsnom$AnimalID <- rownames(paramsnom)
    
    # residents
    paramsres <- paramsall[[4]]
    paramsres$AnimalID <- rownames(paramsres)

	  
	  #### * IN PROGRESS * ####
	  # goal: identify migration timing per popn
	  # (to help define timeframe for winhr est'n)
	  
	  # only use migrant and disperser models
	  # for each indiv, figure out their unique start date
	  # add theta to the start date
	  # add herd information (join)
	  # for each herd, identify min startdate and average startdata

    
    
##### ADD THIS CODE AFTER YOU RUN THE RLOCS #####

## make sure rlocs start date doesn't occur after migration might have started ##


    # disperser parameters
    paramsdisp <- mvmt2df(mot2)[[1]] 
    paramsdisp$AnimalID <- rownames(paramsdisp)
    paramsdisp$Behav <- "disperser"
  	thetasdisp <- dplyr::select(paramsdisp, c(AnimalID, Behav, theta))
    
    # migrant parameters
    paramsmig <- mvmt2df(mot2)[[2]]
    paramsmig$AnimalID <- rownames(paramsmig)
    paramsmig$Behav <- "migrant"
  	thetasmig <- dplyr::select(paramsmig, c(AnimalID, Behav, theta))
	
	# movement date per indiv
	thetasindivs <- rbind(thetasdisp, thetasmig) %>%
		left_join(modlocs, by = "AnimalID") %>%
		dplyr::select(Herd, AnimalID, Behav, Day1, theta) %>%
		mutate(MvmtStart = as.Date(Day1 + as.integer(theta)),
		       DayDiff = as.integer(MvmtStart - Day1)) %>%
	  distinct()
	          # hahaha duh you could've just used the theta value...
	
	# movement date per herd
	thetaspopns <- thetasindivs %>%
	group_by(Herd, Behav) %>%
	summarise(Day1 = min(Day1),
	          MinStdt = min(MvmtStart),
		    	  AvgStdt = mean(MvmtStart))
		
    
	
	# create df of AnimalID, Herd, MinMigStart, AvgMigStart
	migstart <- indivpops %>% # you make this is the summary section
		left_join(thetaspopns, by = "Herd")

	
	
	
stdt <- rlocs %>%
	# identify start date determined by rlocs (migrateR)
	mutate(rlocDay = as.Date(substr(location, 1, 10))) %>%
	# add migration/movement start date info 
	left_join(migstart, by = "Herd") %>%
	# if rloc identified a day after movement had started, 
	# just change it back to using the first location
	# and make a column identifying elk you changed (make sure double-check them visually)
	mutate(fixedRloc = ifelse(rlocDay < MinStdt, rloc, 1),
		   changeRloc = ifelse(fixedRloc != rloc, 1, 0))
	    
	  

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
    
    
### ### ### ### ### ### ### ###
####    |STORING THINGS|   ####
### ### ### ### ### ### ### ###
    
    save.image(file = "nsd-inprogress.RData")
    save.image(file = "nsd-fullmodel.RData")
    save(modlocs, lt, rlocs, mb, mr, mot2, rslts,
         file = "nsd-data.RData")