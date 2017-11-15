### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
###       CLASSIFYING MIGRATORY BEHAVIOR OF ELK         ###
###             USING THE MIGRATER PACKAGE              ###
###              KRISTIN BARKER 2017-2018               ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###



# This code semi-walks through through the migrater vignette 
# using a  small subset of individuals of "known" behavior
# to get a feel for how the anaysis would work


# take-homes from running the code:
  # elevational and nsd models aren't always directly comparable
      # e.g. no mixed-migrant or nomad options for elevational
  # basic idea for how this code works is,
      # specify models with as few constraints as possible
      # then use refine() to address convergence issues
      # then use topmvmt() to further constrain parameters
          # and exclude models from consideration




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
    
    
    
    #### Create sample subset of "known behavior" individuals ####
    
    testindivs <- data.frame(
      AnimalID = as.character(c("61730", "60450", "140480", "140560")),
      DesiredStatus = as.character(c("MixedMig", "MigLD", "Resident", "MigSD")))
    
    
    testlocs <- modlocs %>%
      semi_join(testindivs, by = "AnimalID") %>%
      droplevels()
    
    testlt <- as.ltraj(xy = testlocs[,c("X", "Y")], 
                     # note date must be POSIXct
                     date = testlocs$Date, 
                     # specify indiv (also serves as default burst)
                     id = testlocs$AnimalID,
                     # specify infolocs to allow elevational mign model
                     infolocs = data.frame(elev = testlocs$Elev))
      
      
      
  
    
    
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
    
    
### ### ### ### ### ###
####    |MODELS|   ####
### ### ### ### ### ###     

    
    
    # base model, no tweaks
    mb <- mvmtClass(testlt)
    
    # refine delta to avoid convergence issues
    mr <- refine(mb, p.est = pEst(s.d = -1))
    mr
    
      #### omitting mixedmig ####
    
        # omit mixed 
        mo <- topmvmt(mr, omit = "mixmig")
        attributes(mo) # all mig except the mixmig, who's nomad
        
        # omit mixed and constrain duration 2mo on range2
        mot2 <- topmvmt(mr, omit = "mixmig", mrho = 60)
        attributes(mot2) # ditto above
        
        # omit mixed and constrain duration 3mo on range 2
        mot3 <- topmvmt(mr, omit = "mixmig", mrho = 90)
        attributes(mot3) # now the weird resident is a disperser, better
        
        # omit mixed, constrain duration 2mo, constrain dist 2km
        mot2d2 <- topmvmt(mr, omit = "mixmig", mrho = 60, mdelta = 2000)
        attributes(mot2d2) # makes SD mig a resident, fair enough
        
        # omit mixed, constrain duration 2 mos, constrain dist 1km
        mot2d1 <- topmvmt(mr, omit = "mixmig", mrho = 60, mdelta = 1000)
        attributes(mot2d1) # ditto above
        
        # omit mixed, constrain duration 2 mos, constrain dist 0.5km
        mot2d05 <- topmvmt(mr, omit = "mixmig", mrho = 60, mdelta = 500)
        attributes(mot2d05) # ditto above
            
        # omit mixed, constrain duration 1 mo, constrain dist 0.5km
        mot1d05 <- topmvmt(mr, omit = "mixmig", mrho = 30, mdelta = 500)
        attributes(mot1d05) # ditto above
        
        
     #### omitting nomad ####
    
        # omit mixed 
        mn <- topmvmt(mr, omit = "nomad")
        attributes(mn) # all mixmig except the mixmig, who's disperser
        

      #### removing penalty for complex models
        
        mc <- topmvmt(mr, a.rule = F)
        attributes(mc) # now the mixmig is a nomad, i am so over this
        
        
      #### specific troubleshooting ####
        
        
        # confused about 140560, think she should be obvious SD mig
        plot(mr[[2]])
        spatmig(testlt, mr)
        spatmig(testlt, mr, mod = "mixmig")
        
        # ok your issue for the short-distance migration is the distance constraint
        # so just get rid of that, duh
        
        # check starting locs, is this worthwhile?
        rlocs <- findrloc(testlt)
        
      
        
        
      #### model specification i think will work best ####
      
        # base model, no tweaks
        mb <- mvmtClass(testlt, rloc = rlocs$rloc)
        
        # refine delta to avoid convergence issues
        mr <- refine(mb, p.est = pEst(s.d = -1))
        mr
        
        # omit mixed and constrain duration 2mo on range2
        mot2 <- topmvmt(mr, omit = "mixmig", mrho = 90)
        attributes(mot2) # ditto above
        
        # IT WAS THE FUCKING STARTING LOCATION THE WHOLE TIIIIME
        
        

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #         
    
    ### now starting to clean up the code
    ### to use it the full dataset

    
    #### Base model - no tweaks ####
    m.base <- mvmtClass(lt)
    m.base
    d.base <- data.frame(attributes(topmvmt(m.base))) %>%
      rename(AnimalID = burst, baseAll = names) 
	  
	  
    #### Negative starting delta
    m.sd <- refine(m.base, p.est = pEst(s.d = -1))
    d.sd <- data.frame(attributes(topmvmt(m.sd))) %>%
      rename(AnimalID = burst, sdAll = names)
    
    
    #### Omit mixed-migrant classification
	  m.base.mm <- topmvmt(m.base, omit = "mixmig")
	  d.base.mm <- data.frame(attributes(m.base.mm)) %>%
	    rename(AnimalID = burst, baseNoMix = names)
	  
	  m.sd.mm <- topmvmt(m.sd, omit = "mixmig")
	  d.sd.mm <- data.frame(attributes(m.sd.mm)) %>%
	    rename(AnimalID = burst, sdNoMix = names)
	  
	  #### Store results
	  moddat.base <- full_join(d.base, d.base.mm, by = "AnimalID")
	  moddat.sd <- full_join(d.sd, d.sd.mm, by = "AnimalID")
	  moddat <-  moddat.base %>%
	    full_join(moddat.sd, by = "AnimalID") %>%
	    select(c(AnimalID, sdAll, sdNoMix, baseAll, baseNoMix))
	  write.csv(moddat, file = "./zOldAndMisc/migrateRprelimrun.csv",
	            row.names = F)

	  
	  
	  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     

    

    ##### prelim: effect of using different starting loc ####
    rlocs <- findrloc(testlt) # there is an effect for many; ponder for later
 
    
           
   	# m. : nsapph
		# m2. : madison
  		   
    
    #### n1: base models ####
		
			m.n1 <- mvmtClass(testlt)
			m.n1 # 
			
			m2.n1 <- mvmtClass(test2lt)
			m2.n1 # 
        
		
    #### n2: refine with weird negative delta value to fix convergence issues ####
			
			m.n2 <- refine(m.n1, p.est = pEst(s.d = -1))
		  all(fullmvmt(m.n2))
		  m.n2
		  
			m2.n2 <- refine(m2.n1, p.est = pEst(s.d = -1))
		  all(fullmvmt(m2.n2))
		  m2.n2
		  
		  
		#### n3: omit mixedmig ####
		
			m.n3 <- topmvmt(m.n2, omit = "mixmig")
			names(m.n3) # 4 res, rest mig
			
			a <- data.frame(attributes(m.n3))
			a <- a %>%
			  rename(AnimalID = burst, NSD = names) %>%
			  full_join(testindivs, by = "AnimalID")

			
			m2.n3 <- topmvmt(m2.n2, omit = "mixmig")
		  names(m2.n3) # 2 mig, 1 disp

		  
		#### n4: min occupancy 2mo & omit mixedmig ####
		
			m.n4 <- topmvmt(m.n2, mrho = 60, omit = "mixmig")
			names(m.n4) # all mix, one res
			
			b <- data.frame(attributes(m.n4))
			b <- b %>%
			  rename(AnimalID = burst, NSD = names) %>%
			  full_join(testindivs, by = "AnimalID")
			
			m2.n4 <- topmvmt(m2.n2, mrho = 60, omit = "mixmig")
		  names(m2.n4) # 2 mix, one disp
		  
		  
		  #### n5: min occupancy 2mo & mixedmig allowed ####
		
			m.n5 <- topmvmt(m.n2, mrho = 60)
			names(m.n5) # all mix, one res
			
			c <- data.frame(attributes(m.n5))
			c <- c %>%
			  rename(AnimalID = burst, NSD = names) %>%
			  full_join(testindivs, by = "AnimalID")
			
			
			# just playing with a bunch of options now, trying to get mig from mixmig
			m2.n5 <- topmvmt(m2.n2, mrho = 60, mdelta = 1000)
		  names(m2.n5) # 2 mix, one disp
		   z <- data.frame(attributes(m2.n5))
		   
		  m.n6 <- mvmtClass(testlt, fam = "nsd3d")
		  fullmvmt(m.n6, out = "numer")
		  m.n6b <- topmvmt(m.n6, mrho = 60)
		  attributes(m.n6b)

		  d <- data.frame(attributes(m.n6b))
			d <- d %>%
			  rename(AnimalID = burst, NSD = names) %>%
			  full_join(testindivs, by = "AnimalID")
		  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
    
    
### ### ### ### ### ###
####   |VISUALS|   ####
### ### ### ### ### ###     

		plot(m.n2)
		spatmig(testlt, m.n2)
		  
		plot(m2.n2)
		spatmig(test2lt, m2.n2)
		
		plot(m.n2[[4]]) # clear resident getting vlassified as a migrant


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  #### this would store results of each model in same df ####
  testresults <- left_join(testresults, 
                           data.frame(table(names(topmvmt(m.n1)))) %>%
                             rename(Behav = Var1, m.n1 = Freq), 
                           by = "Behav")
          

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     

  

  ### ### ### ### ### ###
  #### |IN PROGRESS| ####
  ### ### ### ### ### ###    


    #### extract indiv info from topmvmt() post-specification tweaks #### 
		  m2.n2
      m2.n4
      names(m2.n4)
      # ok what you need to to is extract attributes from a list
      attributes(m2.n4)
      # wow. duh, kristin.
      z <- as.data.frame(attributes(m2.n4))
      View(z)
      
      z2 <- as.data.frame(attributes(m.n4))
      View(z2)
    
      
      
      
      
      #### store everything ####             
    save.image(file = "./zOldAndMisc/nsd-prelim.RData")