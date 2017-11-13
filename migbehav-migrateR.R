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
      semi_join(herdsonly, by = c("Herd")) 
      # create POSIXct DateTime for ltraj object; pull just Date from this
      mutate(Date = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")) %>%
      mutate(Day = as.Date(DateTime)) %>%
      # remove stored factor levels that include removed indivs and popns
      mutate(AnimalID = factor(AnimalID), Herd = factor(Herd)) %>%
      group_by(AnimalID) %>%
      # identify 1st date of data
      mutate(Day1 = min(as.Date(DateTime))) %>%
      ungroup() %>%
      # only include 1st yr of data, plus couple extra months for return to winter
      filter(Day <= Day1+425) %>% 
      # remove stored factor levels that include removed indivs
      mutate(AnimalID = factor(AnimalID)) %>%
      # randomly select one loc per day per indiv
      group_by(AnimalID, Day) %>%
      sample_n(1) %>%
      ungroup() %>%
      # remove indivs that don't have at least 10 months of data
      group_by(AnimalID) %>%
      filter(n() > 300) %>%
      ungroup() 
    
    # remove all stored factor levels
    modlocs <- droplevels(modlocs)
   
  
   
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
    
    
    ### ones above the asterisks are from when i started to clean up the code
    ### before realizing it made more sense to try something else
    
    
    #### Base model - no tweaks ####
    m.base <- mvmtClass(modlocs)
    m.base
    
    
    #### Refine (with negative delta value?) to fix convergence issues ####
    m.ref <- refine(m.base,  p.est = pEst(s.d = -1))
	  all(fullmvmt(m.ref))
	  
	  
	  #### Omit mixed-migrant classification
	  mod <- topmvmt(m.ref, omit = "mixmig")

	  
	  
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