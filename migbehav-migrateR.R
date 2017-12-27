### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
###       CLASSIFYING MIGRATORY BEHAVIOR OF ELK         ###
###             USING THE MIGRATER PACKAGE              ###
###              KRISTIN BARKER 2017-2018               ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###



# This code uses Derek Spitz's migrateR package
# to classify elk migratory behavior as
# migratory, resident, or something else ("other")
# based on rNSD and parameter constraints



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
  
  
    library(beepr)
    library(migrateR) 
    source("test_plotmvmt2.R")
    library(dplyr) 

  
  
  #### Projections ####
  
    latlong <- CRS("+init=epsg:4326") # elk GPS collars
    utm <- CRS("+init=epsg:3742") # NAD83(HARN)/UTMzone12N
  
  
  
  #### "Raw" data ####
  
    rawlocs <- read.csv("locs.csv")
    herds <- read.csv("popns-yrs.csv")
    herdsonly <- dplyr::select(herds, Herd)

  

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
      

### ### ### ### ### ###
####  |DATA PREP|  ####
### ### ### ### ### ###


  
  #### Subset and format raw data for use in models ####
    
    
    # identify indivs of interest
    indivs <- rawlocs %>%
      # filter to only locations collected during year of interest
      filter(YrOfLoc == YrOfInterest) %>%
      # only include indivs with at least 9 months of locations
      distinct(AnimalID, Month) %>%
      group_by(AnimalID) %>%
      filter(n() > 8) %>%
      dplyr::select(AnimalID) %>%
      distinct()
      

    # extract locations for individuals of interest
    modlocs <- rawlocs %>%
      # only consider individuals of interest
      semi_join(indivs, by = "AnimalID") %>%
      # create POSIXct DateTime for ltraj object; pull just date (Day) from this
      mutate(Date = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")) %>%
      mutate(Day = as.Date(DateTime))  %>%
      # remove daylight savings NAs
      filter(!is.na(Date)) %>%
      # identify time of day
      mutate(Hour = as.numeric(substr(Time, 0, 2))) %>%
      mutate(TimeOfDay = ifelse(Hour >= 8 & Hour <= 20, "Day", "Night")) %>%
      # randomly select one loc per time of day (so, 2 locs per 24-hour period) per indiv
      group_by(AnimalID, Day, TimeOfDay) %>%
      sample_n(1) %>%
      ungroup() %>%
      # identify 1st date of data
      filter(YrOfLoc >= YrOfInterest) %>%
      group_by(AnimalID) %>%
      mutate(Day1 = min(as.Date(DateTime))) %>%
      ungroup() %>%
      # only include 1st full yr of data, plus extra month for full return to winter
      filter(Day <= Day1 + 395) %>% 
      # remove indivs who didn't have opportunity to return to winter range
      group_by(AnimalID) %>%
      filter(max(Month) == 12) %>%
      ungroup() %>%
      # specify datetime format 
      mutate(Date = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
   
    # remove any stored factor levels that above code removed; store indivlist
    indivs <- droplevels(indivs)
    modlocs <- droplevels(modlocs)
    modindivs <- data.frame(AnimalID = unique(modlocs$AnimalID))
    write.csv(modindivs, "modindivs.csv", row.names = F)
   
  
   
  #### Convert Lat/Longs to UTMs ####
      
    modlocs <- as.data.frame(spTransform(SpatialPointsDataFrame(
                              data.frame("X" = modlocs$Longitude, 
                                         "Y" = modlocs$Latitude), 
                              modlocs, proj4string = latlong), utm))
    write.csv(modlocs, "modlocs.csv", row.names=F)


    
  #### Create ltraj object ### 
    
    lt <- as.ltraj(xy = modlocs[,c("X", "Y")], 
                   # note Date must be POSIXct
                   date = modlocs$Date, 
                   # specify indiv 
                   id = modlocs$AnimalID)
    
  
   #### Identify most parsimonious starting loc for rNSD ####
    
    rlocs <- findrloc(lt)

  
  
   #### store progress ####
    
     save.image(file = "nsd-baselocs.RData")  
  
  
  
   #### Find indivs whose models produce an error due to rloc; give them new rloc
  
     # define parameter causing the issue
      uk64 <- pEst(u.k = log(64))
      
      # create dataframe to store error messages in
      errors <- data.frame(AnimalID = unique(modlocs$AnimalID), Err = NA)
      
      # for each individual
      for(i in 1:nrow(rlocs)) {
        
        # subset its locations
        ilocs <- droplevels(semi_join(modlocs, rlocs[i,], by = c("AnimalID" = "burst")))
        
        # make it ltraj
        ilt <- as.ltraj(xy = ilocs[,c("X", "Y")], date = ilocs$Date, id = ilocs$AnimalID)
        
        # try the model and store error message if any
        tryCatch(mvmtClass(ilt, p.est = uk64, rloc = rlocs[i,"rloc"]), error = function(e) {
                errors[i,"Err"] <<- conditionMessage(e)
                NULL
            })
      }

      
      # identify individuals who had errors
      errorindivs <- errors %>%
        filter(!is.na(Err)) %>%
        left_join(rlocs, by = c("AnimalID" = "burst"))
      nrow(errorindivs)
      
      beep("fanfare")
      
      # change their rloc  
      rlocs$newrloc <- ifelse(rlocs$burst == errorindivs[1,1] | rlocs$burst == errorindivs[2,1] , 
        rlocs$rloc-1, rlocs$rloc)
      
      
      
      ## check whether new rloc fixes errors ##
      
      # create dataframe to store error messages in
      errors <- data.frame(AnimalID = unique(modlocs$AnimalID), Err = NA)
      
      # for each individual
      for(i in 1:nrow(rlocs)) {
        
        # subset its locations
        ilocs <- droplevels(semi_join(modlocs, rlocs[i,], by = c("AnimalID" = "burst")))
        
        # make it ltraj
        ilt <- as.ltraj(xy = ilocs[,c("X", "Y")], date = ilocs$Date, id = ilocs$AnimalID)
        
        # try the model and store error message if any
        tryCatch(mvmtClass(ilt, p.est = uk64, rloc = rlocs[i,"newrloc"]), error = function(e) {
                errors[i,"Err"] <<- conditionMessage(e)
                NULL
            })
      }

      # identify individuals who had errors
      errorindivs <- errors %>%
        filter(!is.na(Err)) %>%
        left_join(rlocs, by = c("AnimalID" = "burst"))
      
      nrow(errorindivs) # fixed one but not both
      beep("sword")

      
      # change remaining rloc  
      rlocs$newrloc <- ifelse(rlocs$burst == errorindivs[1,1], 
        rlocs$newrloc+3, rlocs$newrloc)
            
      ## check whether new rloc fixes errors ##
      
      # create dataframe to store error messages in
      errors <- data.frame(AnimalID = unique(modlocs$AnimalID), Err = NA)
      
      # for each individual
      for(i in 1:nrow(rlocs)) {
        
        # subset its locations
        ilocs <- droplevels(semi_join(modlocs, rlocs[i,], by = c("AnimalID" = "burst")))
        
        # make it ltraj
        ilt <- as.ltraj(xy = ilocs[,c("X", "Y")], date = ilocs$Date, id = ilocs$AnimalID)
        
        # try the model and store error message if any
        tryCatch(mvmtClass(ilt, p.est = uk64, rloc = rlocs[i,"newrloc"]), error = function(e) {
                errors[i,"Err"] <<- conditionMessage(e)
                NULL
            })
      }

      # identify individuals who had errors
      errorindivs <- errors %>%
        filter(!is.na(Err)) %>%
        left_join(rlocs, by = c("AnimalID" = "burst"))
      
      nrow(errorindivs) # 0 = :)
      beep("sword")
      
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
    
    
### ### ### ### ### ###
####    |MODELS|   ####
### ### ### ### ### ###     

       
    
    ## Define initial parameter constraints ##
      
      # expand default duration on summer range to allow up to 8 months (default was 84 days)
      timing <- pEst(u.r = 240)  

      
    
    ## Define base model, rNSD with expanded duration parameter and updated rloc ##
      
      mbase <- mvmtClass(lt, rloc = rlocs$newrloc, p.est = timing) 
      length(which(!fullmvmt(mbase))) # 40 convergence issues
    
    
      
    ## Refine base model to address convergence issues ##
    
      # allow up to 8km daily displacement within the same resident range
      uk64 <- pEst(u.k = log(64))
      mref1 <- refine(mbase, p.est = uk64)
      length(which(!fullmvmt(mref1))) # 21 convergence issues


      
    ## Identify top model for each individual ##
      
      # don't consider mixmig or nomad (see notes)
      mtop <- topmvmt(mref1, omit = c("mixmig", "nomad"))
      topmods <- data.frame(AnimalID = modindivs, PrelimClassn = names(mtop))
      write.csv(topmods, file = "./rNSDresults/initialclassns.csv", row.names=F)
      
      
     # quick look at prelim classns 
     summary(topmods$PrelimClassn)


	  

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
    
    
### ### ### ### ### ###
####   |VISUALS|   ####
### ### ### ### ### ###     

	  
	  
	  #### behavioral classification plots ####

      num.plots <- nrow(modindivs)
      my.plots <- vector(num.plots, mode='list')
      
      for(i in 1:num.plots) {
        plot(mref1[[i]])
        my.plots[[i]] <- recordPlot()
      }
      graphics.off()
      
      pdf('./rNSDresults/migbehav-plots.pdf', onefile = TRUE)
      for(my.plot in my.plots) {
        replayPlot(my.plot)
      }
      graphics.off()
      
      
      
      
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
    
    
### ### ### ### ### ### ###
####    |PARAMETERS|   ####
### ### ### ### ### ### ###    

      
      
      
      ## use parameters to correctly classify behavior (after visually inspecting all plots) ##
      
      
          paramlist <- mvmt2df(mtop)
    
          # create blank df to store parameters of top models in
          params <- data.frame(AnimalID = as.character(), 
                             Model = as.character(),
                             delta = as.numeric(),
                             phi = as.numeric(),
                             theta = as.numeric(),
                             rho = as.numeric(),
                             phi2 = as.numeric(),
                             kappa = as.numeric())
          
          # extract and store parameters of top models
          for (i in 1:length(paramlist)){
            # extract data for each model
            dat <- paramlist[[i]]
            # map parameters to correct individuals
            dat$AnimalID <- row.names(dat)
            # identify model 
            dat$Model <- paste(names(paramlist)[i])
            # store all info
            params <- full_join(params, dat)
          }

      
      
      ## implement rules to reclassify some behaviors based on parameters ##
      
          
          reclass <- params %>%
            # if "resident" moved >900km2, reclassify as Migrant (these are actually mixed migrants)
            mutate(Reclass = ifelse(Model == "resident" & delta > 900, "migrant", Model)) %>%
            # if animal "dispersed" or "migrated" <6.7 km, reclassify as Resident
            mutate(Reclass = ifelse(Reclass == "disperser" &  delta < 45 | Reclass == "migrant" &  delta < 45, "resident", Reclass)) %>%
            # if animal "migrated" 6.8-8.7km, reclassify as Other
            mutate(Reclass = ifelse(Reclass == "migrant" & delta < 75, "other", Reclass)) %>%
            # similarly, if animal "resided" within 6.8-8.7km, reclassify as Other
            mutate(Reclass = ifelse(Reclass == "resident" & delta > 45, "other", Reclass)) %>%  
            # rename dispersers as Other
            mutate(Reclass = ifelse(Reclass == "disperser", "other", Reclass)) %>%
            # if animal "dispersed" or "migrated" after summer, reclassify as Other
            mutate(Reclass = ifelse(!is.na(theta) & theta > 270, "other", Reclass)) 



  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
    
    
### ### ### ### ### ### ##
####    |SUMMARIES|   ####
### ### ### ### ### ### ##
	  
         
        ## add herd info, rename columns to avoid future confusion, and store ##
          
          
          # herd per indiv
          indivherds <- modlocs %>%
            dplyr::select(AnimalID, Herd) %>%
            distinct() 
          
          
          # add herd info to classification info
          behav <- reclass %>%
            left_join(indivherds, by = "AnimalID") %>%
            rename(Behav = Reclass, ParamMod = Model) %>%
            dplyr::select(c(AnimalID, Herd, Behav, delta, phi, theta, rho, phi2, kappa, ParamMod)) %>%
            arrange(AnimalID)
          write.csv(behav, "mig-behav.csv", row.names = F)
          
          
          # summarize behaviors by herd
          popbehav <- behav %>%
      	    group_by(Herd) %>%
      	    summarise(nIndivs = n(),
      	              nMig = length(which(Behav == "migrant")),
      	              nRes = length(which(Behav == "resident")),
      	              nOth = length(which(Behav == "other"))) %>%
      	    mutate(ppnMig = round(nMig/nIndivs, digits = 2),
      	           ppnRes = round(nRes/nIndivs, digits =2),
      	           ppnOth = round(nOth/nIndivs, digits = 2)) %>%
      	    dplyr::select(Herd, ppnMig, ppnRes, ppnOth, nIndivs)
          write.csv(popbehav, "behav-per-popn.csv", row.names=F)
          
          
          # use migration dates to define winter
          indivdate <- modlocs %>%
            distinct(AnimalID, Day1) %>%
            left_join(reclass, by = "AnimalID") %>%
            filter(Reclass == "migrant") %>%
            left_join(rlocs, by = c("AnimalID" = "burst")) %>%
            mutate(stdt =  Day1+newrloc) %>%
            mutate(migDate = stdt+theta) %>%
            mutate(MigDay = substr(migDate, 6, 10)) %>%
            dplyr::select(AnimalID, stdt, migDate, MigDay) %>%
            mutate(nDay = migDate-stdt) %>%
            left_join(indivherds) %>%
            filter(!is.na(migDate))
          capyrs <- dplyr::select(popnyrs, -nIndiv)
          capdat <- read.csv("popn-capdates.csv") %>% 
            semi_join(capyrs, by = c("Herd", "Year")) 
          herddate <- indivdate %>%
            group_by(Herd) %>%
            summarise(firstDate = min(migDate),
                      avgDate = mean(migDate)) %>%
            left_join(capdat) %>%
            dplyr::select(-nIndiv) %>%
            mutate(nDay = firstDate - as.Date(LastCapDate))

            
          
          
          # store everything
          save.image(file = "rNSD.RData")
	  

	  

	  

    
    
    
    
