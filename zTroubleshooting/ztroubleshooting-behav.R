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

  
  #### Projections ####
  
  latlong <- CRS("+init=epsg:4326") # elk collar data 
  utm <- CRS("+init=epsg:3742") # NAD83(HARN)/UTMzone12N
  
  
  #### "Raw" data ####
  
  # rawlocs <- sqlQuery(channel, paste("select * from ElkGPS"))
  rawlocs <- read.csv("locs-allcows-withelevs.csv")


      
      
      # keep it clean
      rm(wd_workcomp, wd_laptop, wd_worklaptop)
  

### ### ### ### ### ###
####  |DATA PREP|  ####
### ### ### ### ### ###

  

  #### Subset and format data ####

  
  # Prep original data
  testlocs <- rawlocs %>%
    # only use Sapph for this trial run
    filter(Herd == "Sapphire") %>%
    # create POSIXct DateTime for ltraj obj
    mutate(Date = as.POSIXct(DateTime, 
                                 format = "%Y-%m-%d %H:%M:%S")) %>%
    # remove stored info about filtered out indivs
    mutate(AnimalID = factor(AnimalID)) %>%
    # randomly select one loc per day per indiv
    group_by(AnimalID, Date) %>%
    sample_n(1) %>%
    ungroup()

  

  # Convert Lat/Longs to UTMs
  testlocs <- as.data.frame(spTransform(SpatialPointsDataFrame(
                            data.frame("X" = testlocs$Longitude, 
                                       "Y" = testlocs$Latitude), 
                            testlocs, proj4string = latlong), utm))


      
  # Create ltraj object
  lt <- as.ltraj(xy = testlocs[,c("X", "Y")], 
                 # note date must be POSIXct
                 date = testlocs$Date, 
                 # specify indiv (also serves as default burst)
                 id = testlocs$AnimalID,
                 # specify infolocs to allow elevational mign model
                 infolocs = data.frame(elev = testlocs$Elev))
  
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

  
  
  #### figuring out subsetting to full first year of data ####
  
  #. 1. check whether included locs after 2014 (i.e., full year)
  test <- testlocs %>%
    group_by(AnimalID) %>%
    summarise(MaxDate = max(Date)) %>%
    ungroup()
  View(test)
  # ok yes, but you're keeping ALL the dates. 
  # need to subset to just that first year -
  # that's prob part of what's throwing shit off
  
  testlocs$Date[1]
  testlocs$Date[2]
  
  test <- testlocs
  test$DateLt <- as.POSIXlt(test$Date)
  test$DateLt[2]
  test$DateLt[2] + 1 # adds one second
  test$DateLt[2]$hour
  unclass(test$DateLt[2])
  
  # ok what are you doing...
  # want to remove all locations after one full year
  # so either remove locations > 365 days after start date
  # or keep locations < 365 days
    
  test2 <- testlocs[1:10,]
  test2$Day <- as.Date(test2$DateTime)
  test2$NextYr <- test2$Day + 365 
  
  
  test3 <- modlocs[1:10,]
  test3$Day <- as.Date(test3$Date)
  test3$NextYr <- test3$Day + 365 
  
  

  save.image(file = "./zOldAndMisc/nsd-prelim.RData")



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###  
  
#### **FROM INITIAL RUNS, PRE-FOCUSED MODEL TESTING ETC** ####

  
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #   
  
  ##### FULL SAPPH DATA MODEL RUNS ####
  
  #### Default parameters ####
  
  ### NSD ### 
  m.n1 <- mvmtClass(lt)
  m.n1
  d.n1 <- data.frame(table(names(topmvmt(m.n1)))) %>%
    rename(Behav = Var1, m.n1 = Freq)
  results <- left_join(results, d.n1, by = "Behav")
  
  
  ### Elev ###
  m.e1 <- mvmtClass(lt, fam = "elev")
  m.e1
  d.e1 <- data.frame(table(names(topmvmt(m.e1)))) %>%
    rename(Behav = Var1, m.e1 = Freq)
  results <- left_join(results, d.e1, by = "Behav")  
  
  
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  #### Tweaked: Minimum distance bt ranges ####  
  
  #### NSD, minimum distance 1km, tweaked in topmvmt() #### 
  m.n2 <- topmvmt(m.n1, mdelta = 1000)
  m.n2
  d.n2 <- data.frame(table(names(m.n2))) %>%
    rename(Behav = Var1, m.n2 = Freq)
  results <- left_join(results, d.n2, by = "Behav")
  
  #### NSD, minimum distance 0.5km, tweaked in topmvmt() #### 
  m.n3 <- topmvmt(m.n1, mdelta = 500)
  m.n3
  d.n3 <- data.frame(table(names(m.n3))) %>%
    rename(Behav = Var1, m.n3 = Freq)
  results <- left_join(results, d.n3, by = "Behav")
  
  # essentially what this tweak is doing is moving all indivs
  # from mixmig and disperser to (mostly) res and nomad
  # their results are identical
  
  
  #### NSD, minimum distance 1km, tweaked in mvmtClass() ####  
  pest.m.n4 <- pEst(l.d = 1000)
  m.n4 <- mvmtClass(lt, p.est = pest.m.n4)
  m.n4
  d.n4 <- data.frame(table(names(topmvmt(m.n4)))) %>%
    rename(Behav = Var1, m.n4 = Freq)
  results <- left_join(results, d.n4, by = "Behav")
  
  # this tweak keeps a moderate amount of mixmig
  # and then calls almost everyone else nomad ,w 4 migs
  
  # ok. so if i tweak distance parameter after the model's run,
  # it favors mostly resident, some nomad (just a couple mig)
  # whereas if i tweak distance in the initial model specification,
  # everyone's evenly split bt nomad and mixmig with just a few mig
  # so tweaked after the model's run (as suggested by spitz)
  # makes the most sense and produces the expected effect
  
  
  
  #### NSD, minimum distance 1km, don't penalize complex models, tweaked in topmvmt() #### 
  # idea here is to allow more mixedmig classifications, maybe?
  m.n5 <- topmvmt(m.n1, mdelta = 1000, a.rule = F)
  m.n5
  d.n5 <- data.frame(table(names(m.n5))) %>%
    rename(Behav = Var1, m.n5 = Freq)
  results <- left_join(results, d.n5, by = "Behav")
  
  # no, that made no difference, so it's not just the penalized model complexity
  # that's keeping it from classifying behavior as mixedmig
  
  
  #### Elev, minimum distance 1km, tweaked in topmvmt() #### 
  m.e2 <- topmvmt(m.e1, mdelta = 1000)
  m.e2
  d.e2 <- data.frame(table(names(m.e2))) %>%
    rename(Behav = Var1, m.e2 = Freq)
  results <- left_join(results, d.e2, by = "Behav")
  
  # oh interesting, this includes strictly residents (most) and migrants
  
  
  #### Elev, minimum distance 0.5km, tweaked in topmvmt() #### 
  m.e3 <- topmvmt(m.e1, mdelta = 500)
  results <- left_join(results, 
                       data.frame(table(names(m.e3))) %>%
                         rename(Behav = Var1, m.e3 = Freq), 
                       by = "Behav")
  # ha! now mostly even split bt residents and migrants
  
  
  
  #### Elev, minimum distance 0.5km, no complexity penalization, tweaked in topmvmt() #### 
  m.e4 <- topmvmt(m.e1, mdelta = 500, a.rule = F)
  d.e4 <- data.frame(table(names(m.e4))) %>%
    rename(Behav = Var1, m.e4 = Freq)
  results <- left_join(results, d.e4, by = "Behav")
  
  # same result as with NSD, which tells me 
  # model complexity isn't what's stopping classification as mixedmig
  
  # take-homes from this exercise:
  # it does make the most sense to tweak parameters after running the default model, and
  # any lack of mixedmig classn is not just an artifact of aic penalizing model complexity
  
  
  
  ##### SAPPH "KNOWN" INDIV DATA SUBSET MODEL RUNS ####
    
  #### Default parameters ####
  
  ### NSD ### 
  m.n1 <- mvmtClass(testlt)
  m.n1
  # all mixmig except resident with most core overlap, disperser (?!)
  testresults <- left_join(testresults, 
                           data.frame(table(names(topmvmt(m.n1)))) %>%
                             rename(Behav = Var1, m.n1 = Freq), 
                           by = "Behav")
  
  ### Elev ###
  m.e1 <- mvmtClass(testlt, fam = "elev")
  m.e1
  # lots of convergence errors
  testresults <- left_join(testresults, 
                           data.frame(table(names(topmvmt(m.e1)))) %>%
                             rename(Behav = Var1, m.e1 = Freq), 
                           by = "Behav") 
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  
  #### Tweaked parameters - min dist ####
  
  
  ### NSD ### 
  m.n2 <- topmvmt(m.n1, mdelta = 1000)
  m.n2
  summary(m.n2)
  # all mixmig except resident with most core overlap, disperser (?!)
  testresults <- left_join(testresults, 
                           data.frame(table(names(m.n2))) %>%
                             rename(Behav = Var1, m.n2 = Freq), 
                           by = "Behav")
  
  ### Elev ###
  m.e2 <- topmvmt(m.e1, mdelta = 1000)
  m.e2
  # lots of convergence errors
  testresults <- left_join(testresults, 
                           data.frame(table(names(m.e2))) %>%
                             rename(Behav = Var1, m.e2 = Freq), 
                           by = "Behav") 

  
  
### ### ### ###
## TO DELETE ##
### ### ### ###
  
  
  # from using access db (DT/Date/Time formatting)
  # diff now bc using csv i made that includes elevation
  # and i already made these formatting changes in that csv
    
      odbcCloseAll()
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
  
  
  
  
  #### from after creating actual R script for this ####
  
  # figure out how to store output
  # make a list of top-supported models for each indiv
  hm <- topmvmt(mod.nsd)
  hm2 <- names(hm)
  table(hm2)
  table(names(topmvmt(mod.nsd))) # summary table of classns
  test <- matrix(hm)
  a <- hm$mixmig$data # not that...
  hm$mixmig$m # no this is actual function code
  hm$mixmig$message # newp
  # ok stop burning time
  
  
  
#### fixing dates for 61730 (and presumably others) ####
  
z <- filter(rawlocs, AnimalID == "61730")
head(z)
# there's your problem, looks like a typo in the database
# this is because you're pulling from DateTime column of Access
# which you already knew was wrong for some indivs 
# remember only use this for TIME; use Date for Date

# fixed; reran all elkdb stuff with corrected DateTime column (killed DT)


#### more older code from prelim runs ####
#### decided not to use elev because not comparable to nsd, unfortunately ####

   ####~ELEVATION MODELS~####
      
            
            # using "known" sapph elk #
            
            
            #### straight nsd, no tweaks ####
            m.e1 <- mvmtClass(testlt, fam = "elev")
            m.e1 # all mig but 2 dispersers (the furthest mig and int indivs)
            
            
            #### tweak distance > 1km ####
            
            #### specified in initial model ####
            m.e2 <- mvmtClass(testlt, p.est = pEst(s.d = 1))
            m.e2 # 2 residents, least far int and most core res
                 # 2 migrants, most hr int and least hr res
                 # rest mixmig
            
            #### specified for topmodel selection ####
            m.e3 <- topmvmt(m.e1, mdelta = 1000) 
            names(m.e3) # just residents and migrants
            

            
            # using obvious LD migrants #
            
            #### straight nsd, no tweaks ####
            m2.e1 <- mvmtClass(test2lt, fam = "elev")
            m2.e1 # all mig 
            
            
            #### tweak distance > 1km ####
            
            #### specified in initial model ####
            m2.e2 <- mvmtClass(test2lt, p.est = pEst(s.d = 1))
            m2.e2 # 
            
            #### specified for topmodel selection ####
            m2.e3 <- topmvmt(m2.e1, mdelta = 1000) 
            names(m2.e3) # all resident (i could be confused about units here)
          
		  
                  
            #### specified for topmodel selection ####
            m.n3 <- topmvmt(m.n1, mdelta = 9000) 
            names(m.n3) # all resident RUN1, except 2 nomads RUN2
            names(m.n3[1])
            
            
            # using obvious LD migrants #
            
            #### specified in initial model ####
            m2.n2 <- mvmtClass(test2lt, p.est = pEst(s.d = 9))
            m2.n2 # all mixmig except 140100(strongR), now disperser RUN1
            # all mixmig except 141130(weakI), now migrant RUN2
            # all mixmig except 140050(shortI) now res, 140100 now disperser
            
            #### specified for topmodel selection ####
            m2.n3 <- topmvmt(m2.n1, mdelta = 9000) 
            names(m2.n3) # all resident RUN1, except 2 nomads RUN2
            names(m2.n3[1])
            
            

            #### specified for topmodel selection ####
            m.n5 <- topmvmt(m.n1, mdelta = 200)
            names(m.n5) # all resident again
		  
		          
        #### tweak distance > 9km (from dist bt centroids of migrants) ####
        
            # using "known" sapph elk #
        
            #### specified in initial model ####
            m.n2 <- mvmtClass(testlt, p.est = pEst(s.d = 9))
            m.n2 # all mixmig except 140100(strongR), now disperser RUN1
                 # all mixmig except 141130(weakI), now migrant RUN2
                 # all mixmig except 140050(shortI) now res, 140100 now disperser
				 
				 
				         #### tweak distance > 2km (for comparison) ####
            
            #### specified in initial model ####
            m.n4 <- mvmtClass(testlt, p.est = pEst(s.d = 2))
            m.n4 # all mixmig except the residents, who are now mig (wtf)
            
            

            
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###           
####            *from actual got-my-shit-together runs*            ####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

            
    #### n1: straight nsd, no tweaks ####
		
			m.n1 <- mvmtClass(testlt)
			m.n1 # all mixmig; some convergence issues
			
			m2.n1 <- mvmtClass(test2lt)
			m2.n1 # 2 mixmig and 1 usu other; some convergence issues
      
			# both have some convergence issues
			
					
			
    #### n2: shared pre-defined start date (end feb) ####
		
			m.n2 <- mvmtClass(testlt, stdt = "02-28")
			m.n2 # very similar to no tweaks
			
			m2.n2 <- mvmtClass(test2lt, stdt = "02-28")
			m2.n2 # very similar to no tweaks
			
			# this actually makes convergence worse
			
			
		
		#### more stuff ####
		
			m.n1 <- mvmtClass(testlt, stdt = "02-28", p.est = pEst(s.d = 1))
			m.n1 # all mixmig; some convergence issues
			
			m2.n1 <- mvmtClass(test2lt, stdt = "02-28", p.est = pEst(s.d = 1))
			m2.n1 # 2 mixmig and 1 usu other; some convergence issues
			
			# yep, convergence even worse
			
			
			
		#### n1: base models ####
		
			m.n1 <- mvmtClass(testlt, stdt = "02-28", p.est = pEst(s.d = -1))
			m.n1 # 
			
			m2.n1 <- mvmtClass(test2lt, stdt = "02-28", p.est = pEst(s.d = -1))
			m2.n1 # 
			
			# this fixes all convergence issues, but i don't understand biological interpretation
			# minimum delta is min dist between the 2 seasonal ranges
			# s.d. = 0.001 results in convergence issues, but s.d. = -1 doesn't
			# so... ranges have to be negatively far away from each other?
			# do these elk exist in an alternate dimension?
			# maybe i'll try refine() for this like derek did in vignette
			
			
		#### n4: omit mixedmig and remove complexity penalization ####
		
			m.n3 <- topmvmt(m.n2, omit = "mixmig", a.rule = F)
			names(m.n3) # 2 res, rest mig
			
			m2.n3 <- topmvmt(m2.n2, omit = "mixmig", a.rule = F)
		  names(m2.n3) # 1 mig, 2 disp
		  
		  # this makes no diff
		  
		  
		#### n4: start date + min dist tweaks, no mixmig ####

			m.n4 <- topmvmt(m.n2, mdelta = 100, omit = "mixmig")
			names(m.n4) # mdelta = 100 gives mix of res and mig
		  
			m2.n4 <- topmvmt(m2.n2, mdelta = 1000, omit = "mixmig")
			names(m2.n4) # 2 mig, one disp
			
			
						
		#### n5: start date + min occupancy time 30d ####
		
			m.n5 <- topmvmt(m.n2, mrho = 30, omit = "mixmig")
			names(m.n5) # all mix, one res
			
			m2.n5 <- topmvmt(m2.n2, mrho = 30, omit = "mixmig")
		  names(m2.n5) # 2 mix, one disp
		
		  # found no diff bt min occupancy time 30 or 60 days
		  
		  
		  		    
		#### n5: min occupancy 2mo & incl mixedmig ####
		
			m.n5 <- topmvmt(m.n2, mrho = 60)
			names(m.n5) # all mix, one res
			
			m2.n5 <- topmvmt(m2.n2, mrho = 60)
		  names(m2.n5) # 2 mix, one disp
		  
		  # this just makes almost everybody mixed still, which isn't helpful
		
		#### n6: start date + min dist 1k + min occupancy time ####
			
			m.n5 <- topmvmt(m.n2, mdelta = 1000, mrho = 30, omit = "mixmig")
			names(m.n5) # all res but one nomad
			
			m2.n5 <- topmvmt(m2.n2, mdelta = 1000, mrho = 30, omit = "mixmig")	
      names(m2.n5) # 2 mix, one dis
        
      
      
#### NOT MODELS ANY MORE ####

play <- rawlocs[sample(nrow(rawlocs), 10),]    
      
# only keep indivs w certin number days      
play <- play %>%
  group_by(AnimalID) %>%
  mutate(Num = n()) %>%
  ungroup()
  nrow(play)
      
      
play <- play %>%
  group_by(AnimalID) %>%
  filter(n() > 300) %>%
  ungroup()
nrow(play)