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



#### not keeping all the dates (cutting off at end of calendar yr) ####

hm <- filter(rawlocs, AnimalID == "70440")
View(hm)
# ok. rawlocs does include all the recorded locations, so the issue comes later

hm2 <- filter(modlocs, AnimalID == "70440")
View(hm2)

hm3 <- filter(modlocs, AnimalID == "BROOT0014")
View(hm3)

rndmindivs <- sample(modindivs$AnimalID, 5)
rndmindivs <- data.frame(as.character(rndmindivs))
colnames(rndmindivs) = "AnimalID"
rndmindivs$AnimalID <- as.character(rndmindivs$AnimalID)
wtf <- rawlocs %>%
  semi_join(rndmindivs, by = "AnimalID") %>%
  mutate(AnimalID = as.character(AnimalID))
unique(wtf$AnimalID)

wtfmodlocs <- wtf %>%
  ##DELETED 2 LINES HERE##    
  # create POSIXct DateTime for ltraj object; pull just Date from this
      mutate(Date = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")) %>%
      mutate(Day = as.Date(DateTime)) %>%
      # remove stored factor levels that include removed indivs and popns
      mutate(AnimalID = as.character(AnimalID), Herd = as.character(Herd)) %>%
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
       ungroup()

rawsub <- rawlocs[rawlocs$AnimalID == rndmindivs[2,1],]
wtfsub <- wtfmodlocs[wtfmodlocs$AnimalID == rndmindivs[2,1],]




#### ooops you re-included individuals captured in subsequent years ####

## subset 3 sapph elk to practice code on, 1 that should be removed

testdate <- as.Date("2014-05-26")
substr(testdate, 1, 4) == modlocs[1,12]

testindivs <- data.frame(AnimalID = c("140040", "140050", "150920"))

testlocs <- rawlocs %>%
  semi_join(testindivs, by = "AnimalID") %>%
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
      mutate(LocYr = substr(Day, 1, 4),
             YrMatch = ifelse(LocYr == Year, 1, 0)) %>%
      group_by(AnimalID) %>%
      filter(sum(YrMatch) > 275) %>%
      ungroup()




#### motherfucking stupid shitty date times wtffff ####
str(modlocs)
hm <- modlocs[modlocs$AnimalID == modindivs[77,1],]

modlocs[28835,16]
modlocs[28835,5]
paste(modlocs[28835,16], modlocs[28835,5], sep = " ")
paste(modlocs[28835,16], modlocs[28835,5], sep = " ")



#### determining definition of winter for each herd based on migration start dates ####
            # because some have unexpectedly early thetas #

# just pull popns where it's an issue
testdat <- filter(modlocs, Herd == "Blacktail" | Herd == "East Fork" | Herd == "Greeley" | Herd == "NMadision" | Herd == "Silver Run")
testlt <- as.ltraj(xy = testdat[,c("X", "Y")], 
                   # note Date must be POSIXct
                   date = testdat$Date, 
                   # specify indiv 
                   id = testdat$AnimalID)
# and rerun models so you can plot indivs
rlocs2 <- findrloc(testlt)
mb2 <- mvmtClass(testlt, rloc = rlocs2$rloc)
mr2 <- refine(mb2, p.est = pEst(s.d = -1))
all(fullmvmt(mr2))
# oh wait that may have been pointless

# just pull INDIVS where it's an issue
testindivs <- thetasindivs %>%
  filter(DayDiff < 30) %>% # less than a month bt capture and mvmt
  distinct() # 30 indivs to look at (incls nomads who may not be impt)

# now we figure out how to only spatmig these indivs, i think
spatmig(testlt[[1]], testdat[[1]])
spatmig(testlt[1], testdat[1])
spatmig # ohhhh, yeah, i don't think i can subset without rewriting that function
        # which we all know i'm not gonna bother doing

# can i extract those indivs from the lt, or do i need to run a new one?
?Extract.ltraj

testlt2 <- lt[id = "140060"] # cool
testlt2 <- lt[id = "140060" | id = "140340"] # not cool
testlt2 <- lt[id = "140060" | "140340"] # also not cool
testlt2 <- lt[id = c("140060", "140340")] # ooooooh the coolest
testlist <- c("140060", "140340")
testlt2 <- lt[id = testlist] # damn i'm good

rlocs2 <- findrloc(testlt2)

testlist <- as.character(unique(testindivs$AnimalID))
testlt <- lt[id = testlist] # i love it when a plan comes together
# don't see quick answer for filtering modles unfortunately
mb2 <- mvmtClass(testlt, rloc = rlocs2$rloc)
mr2 <- refine(mb2, p.est = pEst(s.d = -1))

# store csv to make comments in
write.csv(testindivs, "./NSDresults/weirdthetas.csv", row.names=F)

# plot

spatmig(testlt, mr2)
  # made notes about each one in /NSDresults/weirdthetas.csv




 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
    
    
### ### ### ### ### ### ### ### ### ### ##
####    |OK OK FOR REALZ THIS TIME|   ####
### ### ### ### ### ### ### ### ### ### ##

# (loaded nsd-baselocs.RData)


# test base model 1 - no tweaks
tm1 <- mvmtClass(lt)
length(which(!fullmvmt(tm1))) # 48 convergence failures, yikes
m1 <- topmvmt(tm1)
table(names(m1)) # 2disp 47mig 229mix 8nom 11res


# # # # # # # # # # # # # 


# test parameter constraints 1 
  # require 60 days on summer range; allow up to 6 months
tp2 <- pEst(l.r = 60, u.r = 180)

# test base model 1
tm2 <- mvmtClass(lt, p.est = tp2)
length(which(!fullmvmt(tm2))) # 40 convergence failures, better
m2 <- topmvmt(tm2)
table(names((m2))) #7disp 47mig 223mix 9nom 11res

# what is up with all these mixed migrants??
# gotta be something with the zetas i guess?
# that's the only special thing about the mixmig model
	  # extract parameters from all models
    tparams <- mvmt2df(m2)
    tparams
    
    # mixed mig only
    tzeta <- tparams[[3]] 
    tzeta$AnimalID <- rownames(tzeta)
    summary(tzeta$zeta)
    # zeta ranges from ~0 to ~1, seems pretty left-skewed
    # oh, dur, it's a percent... pay attention kristin
    # ok so this tells me over half of the elk classified as mixed
    # had a distance between their 2nd and 3rd ranges 
      # that was 75% of the distance between the 1st and second 
      # (i think)
    # another option to address this is phi2, forgot that one
      # phi2 is the duration of mvmt (time to complete most of the return)
    summary(tzeta$phi2)
      # phi2 (return dur) ranges from 1 day to 3 weeks
    # and compare mixedmig phi to mig phi
    summary(tzeta$phi); summary(tparams[[2]]$phi)
      # ok obviuosly it defaults to max out at 21 days
      # also phi for mixmig is quite a bit bigger than for mig
      # which means they take longer to get to wherever
      # but interestingly, they take half-ish the time to get back
        # which maybe doesn't make a ton of sense

# # # # # # # # # # # # # 


# test parameter constraints 2 
  # require 60 days on summer range; allow up to 9 months
tp3 <- pEst(l.r = 60, u.r = 280)

# model 3 (constraints 2)
tm3 <- mvmtClass(lt, p.est = tp3)
length(which(!fullmvmt(tm3))) # back to 48, interesting
m3 <- topmvmt(tm3)
table(names((m3))) #6disp 46mig 221mix 21nom 12res


# # # # # # # # # # # # # 


# same parameter constraints as 1, but now with rloc
  # require 60 days on summer range; allow up to 6 months
tm3 <- mvmtClass(lt, p.est = tp2, rloc = rlocs$rloc)
length(which(!fullmvmt(tm3))) # down to 27 convergence failures, much better
m3 <- topmvmt(tm3)
table(names((m3))) #8disp 35mig 236mix 3nom 15res
            # but it still doesn't solve the mixedmig issue
plot(tm3[[45]])
plot(tm3[[42]], legend = F)
plot(tm3[[44]], ranked = FALSE)
plot(tm3[[44]], ranked = TRUE)
migrateR::plot(tm3[[45]])




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
    
    
### ### ### ### ### ### ### ### ### ### ### ###
####   |INVESTIGATING CONVERGENCE ISSUES|  ####
### ### ### ### ### ### ### ### ### ### ### ###


# plan:

#X identify indivs with issues
conv <- data.frame(which(!fullmvmt(mb2))) 
conv$AnimalID <- row.names(conv)
colnames(conv)[1] <- "index"
    

# identify models for each indiv that failed to converge
a <- fullmvmt(mb2, out = "name")
b <- lapply(a, sort)
b <- lapply(a, length)
any(b < 5)
any(b < 4) # T
any(b < 3) # F - so everyone has at least 3 converged models
c <- which(b < 5) # pointless, you have this in conv

ci <- matrix(a)

indexlist <- conv$index
# want to pull these from a and add to conv (indiv info)

# ok yknow what this is taking too long
# just use your lt subset and keep rolling


convlt <- lt[id = conv$AnimalID]


# plot only individuals with convergence issues



       # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
    
    
### ### ### ### ### ### ### ### ### ### ### ###
####    |Example plot stuff for derek|   ####
### ### ### ### ### ###    ### ### ### ### ### ### 
        
      sublocs <- filter(modlocs, AnimalID == "140340")
      write.csv(sublocs, file = "./NSDresults/indiv140340.csv", row.names=F)
      
      library(migrateR)
      
      sublocs <- read.csv("indiv140340.csv")
      sublocs$Date <- as.POSIXct(sublocs$Date)
      
      sublt <- as.ltraj(xy = sublocs[,c("X", "Y")], 
                   # note Date must be POSIXct
                   date = sublocs$Date, 
                   # specify indiv 
                   id = sublocs$AnimalID)
      rlocs <- findrloc(sublt) 
      
      
      # expand default duration on summer range to allow up to 8 months
      dur8 <- pEst(u.r = 240) 
      
      
      # define base model, rNSD with expanded duration parameter 
      msub <- mvmtClass(sublt, p.est = dur8, rloc = rlocs$rloc)
      fullmvmt(msub, out = "name") 

      # plotting attempts
      plot(msub)
      plot(msub, ranked = F)
      plot(msub, legend = T)
      spatmig(sublt, msub)
      
      
      
      
#### subsetting mvmts obj (for prelim visual plot inspection) ####
      
      dev.off()
      test <- mb4
      str(mb4)
      test2 <- test[[1]]
      plot(test2) # ok, that worked
      test2 <- test[[1:3]] # that didn't
      test2 <- test[[1,3]] # of course not
      test2 <- test[2] #this does not extract ALL the info
      test3 <- test[[2]]
      plot(test3)
      test4 <- c(test2, test3)
      plot(test4) # newp
      plot(test4[[1]]) # yup
      plot(test4[[2]]) # yup
      # fine, we'll do it quick and dirty, make new mvmts
      
      # random subset of 20 indivs
      a <- data.frame(AnimalID = modindivs[sample(nrow(modindivs), 20), 1])
      
      # add a few "known" indivs
      #b <- data.frame(AnimalID = factor(c(61730, 60450, 140480, 140560, 141490)))
      b <- data.frame(AnimalID = factor(c(140320, 15020, 15045, 60280)))
        
      # combine 'em
      subi <- bind_rows(a, b)
      
      # and subset 'em from original dataframe
      sublocs <- modlocs
      sublocs <- semi_join(modlocs, subi, by = "AnimalID") 
      sublocs$AnimalID <- factor(sublocs$AnimalID)
      unique(sublocs$AnimalID)
      
      #### Create ltraj object ### 
      ltsub <- as.ltraj(xy = sublocs[,c("X", "Y")], 
                   # note Date must be POSIXct
                   date = sublocs$Date, 
                   # specify indiv 
                   id = sublocs$AnimalID)
    
      ##
      
      # identify best starting location for each indivdual
      rlocssub <- findrloc(ltsub)
    
      # define base model, rNSD with expanded duration parameter 
      mbsub <- mvmtClass(ltsub, p.est = dur8, rloc = rlocssub$rloc) # 4 conv issues
      
      # allow up to 80km2 mvmt within the same resident range
      mbsub2 <- refine(mbsub, p.est = restest)
      length(which(!fullmvmt(mbsub2))) # 2 convergence issues
      
      # only has to move 25 km2
      mbsub4 <- refine(mbsub2, p.est = mv5)
      length(which(!fullmvmt(mbsub4))) # 0 issues of course
      
      
      ## 
      # top mods (all require 2 months on summer range; require move 5km)
      mtop1 <- topmvmt(mbsub, omit = "mixmig",  mrho = 60, mdelta = 25)
      mtop2 <- topmvmt(mbsub2, omit = "mixmig",  mrho = 60, mdelta = 25)
     # mtop3 <- topmvmt(mbsub3, omit = "mixmig",  mrho = 60, mdelta = 25)
      mtop4 <- topmvmt(mbsub4, omit = "mixmig",  mrho = 60, mdelta = 25)
      ##

      # check whether classifications change in refined models            
      table(names(mtop1)) 
      table(names(mtop2)) # no change
      table(names(mtop3)) # makes 1 mig a res (good? prob moved late)
      table(names(mtop4)) # no change
      
    
      ##
##### plots ####
      subindivs <- data.frame(AnimalID = unique(sublocs$AnimalID))
      dev.off()
      for(i in 1:nrow(subindivs)) {
        plot(mbsub[[i]])
      }
      
      
      ##
      # saving plots - subset
      
      num.plots <- nrow(subindivs)
      my.plots <- vector(num.plots, mode='list')
      
      for(i in 1:num.plots) {
        plot(mref1[[i]])
        my.plots[[i]] <- recordPlot()
      }
      graphics.off()
      
      pdf('./test/myplots3.pdf', onefile = TRUE)
      for(my.plot in my.plots) {
        replayPlot(my.plot)
      }
      graphics.off()
      
      # holy goddamn fucking shitballs
      
      
      # do it gridded?
      par(mfrow=c(1,3))
      # ew, no, saves each iteration of that
      # id have to tell it to plot three at a time or something
      # and frankly i don't give that much of a shit
      
      
      
            ##
      # saving plots - all (looking for errors due to nonconvergence)
      
      num.plots <- nrow(modindivs)
      my.plots <- vector(num.plots, mode='list')
      
      for(i in 1:num.plots) {
        plot(mref1[[i]])
        my.plots[[i]] <- recordPlot()
      }
      graphics.off()
      
      pdf('./test/myplots4.pdf', onefile = TRUE)
      for(my.plot in my.plots) {
        replayPlot(my.plot)
      }
      graphics.off()
      
      
      
      num.plots <- nrow(modindivs)
      my.plots <- vector(num.plots, mode='list')
      
      for(i in 1:num.plots) {
        plot(mb[[i]])
        my.plots[[i]] <- recordPlot()
      }
      graphics.off()
      
      pdf('./test/myplots5-mb.pdf', onefile = TRUE)
      for(my.plot in my.plots) {
        replayPlot(my.plot)
      }
      graphics.off()
      
### compare including all initial constraints in same model ####
    ### vs refine
    

    constr <- pEst(u.r = 240, u.k = log(80),  u.t = 150, l.d = 50)
    mod <- mvmtClass(lt, p.est = constr)
    length(which(!fullmvmt(mod))) # 99, holy balls
    # ok so not doing this
    
#### check differences in top model selection after refining and not ####
    ### bc most models that dind't converge initially didn't fit the data anyway
    
    
    mcomp <- topmvmt(mb, omit = "mixmig", mrho = 60, mdelta = 25)
    table(names(mcomp))
    #yeah ok there's a pretty minor difference here
    #base model has +1disp +1mig +2nom -4res
    
    # diff due to not having better res def?
    mcomp2 <- topmvmt(mb2, omit = "mixmig", mrho = 60, mdelta = 25)
    table(names(mcomp2))        
    #now base model would have +1disp -1mig -1nom +1res
    
    
#### check differences in top model selection if refine base model all at once ####
    ### vs re-refining 3x
    
    mtest <- refine(mb, p.est = constr)
    toptest <- topmvmt(mtest, omit = "mixmig", mrho = 60, mdelta = 25)
    table(names(toptest))
    
    # this IS different
    # refining all at once results in +2 disp, +2nom, -4res (=mig)
        

      
      
      
      
#### discrepancy in indivs included before and after re-adding HD314 (and fixing up code) ####
      
      
    oldindivs <- read.csv("./rNSDresults/initialclassns.csv")
    newindivs <- read.csv("modindivs-afterhd314readded.csv")
    indivherds <- rawlocs %>%
      dplyr::select(c(AnimalID, Herd)) %>%
      distinct() %>%
      mutate(AnimalID = factor(AnimalID), Herd = factor(Herd))
    
    diffa <- anti_join(oldindivs, newindivs, by = "AnimalID") %>%
      dplyr::select(AnimalID) %>%
      mutate(Diff = "OldOnly")
    diffb <- anti_join(newindivs, oldindivs, by = "AnimalID") %>%
      mutate(Diff = "NewOnly")
    diff <- rbind(diffa, diffb) 
    diff <- left_join(diff, indivherds, by = "AnimalID")
    
    summary(diff$Herd)
    # 4 NAs are ski hill elk, they should be gone
    # others are entire sample from dome and madison, yeesh, that's probably not right...
    
    ## issue: modlocs does have all popn data stored but lost it along the way
    
    # modlocs came from rawlocs
    any(rawlocs$Herd == "HD314")
    any(rawlocs$Herd == "Madison")
    # and they are present there
    
    ## i think the issue was with the code subsetting to year of interest
    
    # already subsetted to herds of interest
      # and removed locs prior to year of interest
      # so don't need to worry about either of those things
    
    newnew <- modindivs %>%
      left_join(indivherds, by = "AnimalID")
    summary(newnew$Herd)
    
    # ok you effed up the east fork for sure, 
    efk <- subset(rawlocs, Herd == "East Fork")
    length(unique(efk$AnimalID))
    
    
    # seeing whether slice() works with more than one grouping
    df <- data.frame(id=c(1,1,1,2,2,2,3,3,3), 
                 stopId=c("a","a","c","a","a","c","a","a","c"), 
                 stopSequence=c(1,2,3,3,1,4,3,1,2))
    test <- df %>%
      group_by(id, stopId) %>%
      slice(1) %>%
      ungroup()
    test2 <- df %>%
      group_by(id, stopId) %>%
      distinct()
    
    
    
#### figuring out missing value or infinity error with u.k = log(64) ####
   
    
     
    # this error did not occur prior to including HD314 in analyses
    
    # so first, look at the individuals tha are different between this and previous runs
    
    oldindivs <- read.csv("./test/initialclassns.csv")
    newindivs <- modindivs
    indivherds <- rawlocs %>%
      dplyr::select(c(AnimalID, Herd)) %>%
      distinct() %>%
      mutate(AnimalID = factor(AnimalID), Herd = factor(Herd))
    
    diffa <- anti_join(oldindivs, newindivs, by = "AnimalID") %>%
      dplyr::select(AnimalID) %>%
      mutate(Diff = "OldOnly")
    diffb <- anti_join(newindivs, oldindivs, by = "AnimalID") %>%
      mutate(Diff = "NewOnly")
    diff <- rbind(diffa, diffb) 
    diff <- left_join(diff, indivherds, by = "AnimalID")
          
    
    # alright, diff is that 4 ski hill elk were removed and 6 HD314 elk were added
    # therefore it must be the HD 314 elk causing the issue, look at their plots first (185XXX)
    
    # don't notice anything obvious; double-check that error is from them
    
      hds <- filter(modlocs, Herd == "HD314")
      hds <- droplevels(hds)

      lthd <- as.ltraj(xy = hds[,c("X", "Y")], 
                   # note Date must be POSIXct
                   date = hds$Date, 
                   # specify indiv 
                   id = hds$AnimalID)
    
      # identify best starting location for each indivdual
      rlocshd <- findrloc(lthd)
    
      # define base model, rNSD with expanded duration parameter 
      mbhd <- mvmtClass(lthd, p.est = timing, rloc = rlocshd$rloc) 
      
      # allow up to 80km2 mvmt within the same resident range
      mbhd2 <- refine(mbhd, p.est = uk64)
      
      # huh. no, the error does not occur when only using hd314 locations
      
      
      ## only other diff i can think of is that i also made the u.t constraint now
      ## so try various iterations of introducing constraints ##
      
      # put uk constraint in original model specification
      a <- pEst(u.r = 240, u.t = 150, u.k = log(64))  
      t1 <- mvmtClass(lt, p.est = a, rloc = rlocs$rloc) 
      # yes this throws the error
      
      # put uk constraint in original model specification and remove ut
      b <- pEst(u.r = 240, u.k = log(64))  
      t2 <- mvmtClass(lt, p.est = b, rloc = rlocs$rloc) 
      # this also throws the error, so it can't be solely related to ut
      
      # make u.k the only constraint
      c <- pEst(u.k = log(64))
      t3 <- mvmtClass(lt, p.est = c, rloc = rlocs$rloc)
      # this also throws the error
      
      # only use u.k in refine
      t4 <- mvmtClass(lt, rloc = rlocs$rloc)  
      t5 <- refine(t4, p.est = uk64)  
      # this also throws the error
    
      
      ## verify error does not occur with previous data
      ## {in separate rstudio session}
      
      # see if packages are loaded in new sesh
      ?pEst
      
      # nope, rerun code from beginning
      
      wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\Migration\\test"
      wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\Migration\\test"
      wd_worklaptop <- "C:\\Users\\kristin\\Documents\\Migration\\test"
      if (file.exists(wd_workcomp)) {setwd(wd_workcomp)
      } else {
        if(file.exists(wd_laptop)) {setwd(wd_laptop)
        } else {
          setwd(wd_worklaptop)
        }
      }
      rm(wd_workcomp, wd_laptop, wd_worklaptop)
   
    
      library(migrateR) 
      source("NSDresults/test_plotmvmt2.R")
      library(dplyr) 
      
      load("nsd-baselocs.RData")
    
      
      # base model with both timing constraints (duration and start)
      timing <- pEst(u.r = 240, u.t = 150)  
      mbase <- mvmtClass(lt, p.est = timing, rloc = rlocs$rloc)
      
      # refined with u.k
      uk64 <- pEst(u.k = log(64))
      mref1 <- refine(mbase, p.est = uk64)
      # confirmed, this works fine. what the shit.
      
      
      # ok think, this makes sense, but how?
      
      # issue is the uk constraint, kappa parameter
      # kappa is the log of the movement rate per day
      
      # only other thing i can think of is it's due to diff random locs being selected
      # but if that's an issue then that's a pretty big problem in general that 
      # should be addressed (and i doubt that would be it anyway)
      
      
      
      # alright let's try subsetting each population to pinpoint the problem
      # back to original rstudio sesh
      
      
      unique(modlocs$Herd)
      sap <- filter(modlocs, Herd == "Sapphire")
      elk <- filter(modlocs, Herd == "Elkhorns")
      nmd <- filter(modlocs, Herd == "NMadison")
      tob <- filter(modlocs, Herd == "Tobacco Roots")
      mad <- filter(modlocs, Herd == "Madison") #E
      dom <- filter(modlocs, Herd == "Dome")
      efk <- filter(modlocs, Herd == "East Fork")      
      wfk <- filter(modlocs, Herd == "West Fork")      
      cfk <- filter(modlocs, Herd == "Clarks Fork")
      sil <- filter(modlocs, Herd == "Silver Run")
      bla <- filter(modlocs, Herd == "Blacktail")
      mil <- filter(modlocs, Herd == "Mill Creek")      
      gre <- filter(modlocs, Herd == "Greeley")
      pio <- filter(modlocs, Herd == "Pioneers")
      sag <- filter(modlocs, Herd == "Sage Creek")      
      
      
      ## SAPPHIRE ##
      sap <- droplevels(sap)
      ltsap <- as.ltraj(xy = sap[,c("X", "Y")], 
                   # note Date must be POSIXct
                   date = sap$Date, 
                   # specify indiv 
                   id = sap$AnimalID)
      # identify best starting location for each indivdual
      rlocssap <- findrloc(ltsap)
      # define base model, rNSD with expanded duration parameter 
      mbsap <- mvmtClass(ltsap, p.est = timing, rloc = rlocssap$rloc) 
      # allow up to 80km2 mvmt within the same resident range
      mbsap2 <- refine(mbsap, p.est = uk64)
      ## NO ERROR ##
      
      
      
      ## ELKHORNS ##
      elk <- droplevels(elk)
      ltelk <- as.ltraj(xy = elk[,c("X", "Y")], 
                   # note Date must be POSIXct
                   date = elk$Date, 
                   # specify indiv 
                   id = elk$AnimalID)
      # identify best starting location for each indivdual
      rlocselk <- findrloc(ltelk)
      # define base model, rNSD with expanded duration parameter 
      mbelk <- mvmtClass(ltelk, p.est = timing, rloc = rlocselk$rloc) 
      # allow up to 80km2 mvmt within the same resident range
      mbelk2 <- refine(mbelk, p.est = uk64)
      ## NO ERROR ##
      
      
      
      ## NMAD ##
      nmd <- droplevels(nmd)
      ltnmd <- as.ltraj(xy = nmd[,c("X", "Y")], 
                   # note Date must be POSIXct
                   date = nmd$Date, 
                   # specify indiv 
                   id = nmd$AnimalID)
      # identify best starting location for each indivdual
      rlocsnmd <- findrloc(ltnmd)
      # define base model, rNSD with expanded duration parameter 
      mbnmd <- mvmtClass(ltnmd, p.est = timing, rloc = rlocsnmd$rloc) 
      # allow up to 80km2 mvmt within the same resident range
      mbnmd2 <- refine(mbnmd, p.est = uk64)
      ## NO ERROR ##
      
      
      
      ## TOBROOTS ##
      tob <- droplevels(tob)
      lttob <- as.ltraj(xy = tob[,c("X", "Y")], 
                   # note Date must be POSIXct
                   date = tob$Date, 
                   # specify indiv 
                   id = tob$AnimalID)
      # identify best starting location for each indivdual
      rlocstob <- findrloc(lttob)
      # define base model, rNSD with expanded duration parameter 
      mbtob <- mvmtClass(lttob, p.est = timing, rloc = rlocstob$rloc) 
      # allow up to 80km2 mvmt within the same resident range
      mbtob2 <- refine(mbtob, p.est = uk64)
      ## NO ERROR ##
      
      
      
      ## MADISON ##
      mad <- droplevels(mad)
      ltmad <- as.ltraj(xy = mad[,c("X", "Y")], 
                   # note Date must be POSIXct
                   date = mad$Date, 
                   # specify indiv 
                   id = mad$AnimalID)
      # identify best starting location for each indivdual
      rlocsmad <- findrloc(ltmad)
      # define base model, rNSD with expanded duration parameter 
      mbmad <- mvmtClass(ltmad, p.est = timing, rloc = rlocsmad$rloc) 
      # allow up to 80km2 mvmt within the same resident range
      mbmad2 <- refine(mbmad, p.est = uk64)
      ## GOTCHA ##
      
      
      ## DOME ##
      dom <- droplevels(dom)
      ltdom <- as.ltraj(xy = dom[,c("X", "Y")], 
                   # note Date must be POSIXct
                   date = dom$Date, 
                   # specify indiv 
                   id = dom$AnimalID)
      # identify best starting location for each indivdual
      rlocsdom <- findrloc(ltdom)
      # define base model, rNSD with expanded duration parameter 
      mbdom <- mvmtClass(ltdom, p.est = timing, rloc = rlocsdom$rloc) 
      # allow up to 80km2 mvmt within the same resident range
      mbdom2 <- refine(mbdom, p.est = uk64)
      # NO ERROR #
      
      
      
  ## alright, just remove madison to see whether that's the only issue
      
      modlocs2 <- filter(modlocs, Herd != "Madison")
      modlocs2 <- droplevels(modlocs2)
      lt2 <- as.ltraj(xy = modlocs2[,c("X", "Y")], 
               # note Date must be POSIXct
               date = modlocs2$Date, 
               # specify indiv 
               id = modlocs2$AnimalID)
      rlocs2 <- findrloc(lt2)
      m2base <- mvmtClass(lt2, p.est = timing, rloc = rlocs2$rloc)
      m2ref1 <- refine(m2base, p.est = uk64)
      # shit. still error. check remaining populations...
      
      
      
            
      ## EFK ##
      efk <- droplevels(efk)
      ltefk <- as.ltraj(xy = efk[,c("X", "Y")], 
                   # note Date must be POSIXct
                   date = efk$Date, 
                   # specify indiv 
                   id = efk$AnimalID)
      # identify best starting location for each indivdual
      rlocsefk <- findrloc(ltefk)
      # define base model, rNSD with expanded duration parameter 
      mbefk <- mvmtClass(ltefk, p.est = timing, rloc = rlocsefk$rloc) 
      # allow up to 80km2 mvmt within the same resident range
      mbefk2 <- refine(mbefk, p.est = uk64)
      # NO ERROR #
      
      
      ## wfk ##
      wfk <- droplevels(wfk)
      ltwfk <- as.ltraj(xy = wfk[,c("X", "Y")], 
                   # note Date must be POSIXct
                   date = wfk$Date, 
                   # specify indiv 
                   id = wfk$AnimalID)
      # identify best starting location for each indivdual
      rlocswfk <- findrloc(ltwfk)
      # define base model, rNSD with expanded duration parameter 
      mbwfk <- mvmtClass(ltwfk, p.est = timing, rloc = rlocswfk$rloc) 
      # allow up to 80km2 mvmt within the same resident range
      mbwfk2 <- refine(mbwfk, p.est = uk64)
      # NO ERROR (but only 1 indiv, wtf...) #
      
      
      
      ## cfk ##
      cfk <- droplevels(cfk)
      ltcfk <- as.ltraj(xy = cfk[,c("X", "Y")], 
                   # note Date must be POSIXct
                   date = cfk$Date, 
                   # specify indiv 
                   id = cfk$AnimalID)
      # identify best starting location for each indivdual
      rlocscfk <- findrloc(ltcfk)
      # define base model, rNSD with expanded duration parameter 
      mbcfk <- mvmtClass(ltcfk, p.est = timing, rloc = rlocscfk$rloc) 
      # allow up to 80km2 mvmt within the same resident range
      mbcfk2 <- refine(mbcfk, p.est = uk64)      
      # NO ERROR #

      
      
      ## sil ##
      sil <- droplevels(sil)
      ltsil <- as.ltraj(xy = sil[,c("X", "Y")], 
                   # note Date must be POSIXct
                   date = sil$Date, 
                   # specify indiv 
                   id = sil$AnimalID)
      # identify best starting location for each indivdual
      rlocssil <- findrloc(ltsil)
      # define base model, rNSD with expanded duration parameter 
      mbsil <- mvmtClass(ltsil, p.est = timing, rloc = rlocssil$rloc) 
      # allow up to 80km2 mvmt within the same resident range
      mbsil2 <- refine(mbsil, p.est = uk64)      
      # NO ERROR #
      

      ## bla ##
      bla <- droplevels(bla)
      ltbla <- as.ltraj(xy = bla[,c("X", "Y")], 
                   # note Date must be POSIXct
                   date = bla$Date, 
                   # specify indiv 
                   id = bla$AnimalID)
      # identify best starting location for each indivdual
      rlocsbla <- findrloc(ltbla)
      # define base model, rNSD with expanded duration parameter 
      mbbla <- mvmtClass(ltbla, p.est = timing, rloc = rlocsbla$rloc) 
      # allow up to 80km2 mvmt within the same resident range
      mbbla2 <- refine(mbbla, p.est = uk64)  
      # NO ERROR #
      
      
      ## mil ##
      mil <- droplevels(mil)
      ltmil <- as.ltraj(xy = mil[,c("X", "Y")], 
                   # note Date must be POSIXct
                   date = mil$Date, 
                   # specify indiv 
                   id = mil$AnimalID)
      # identify best starting location for each indivdual
      rlocsmil <- findrloc(ltmil)
      # define base model, rNSD with expanded duration parameter 
      mbmil <- mvmtClass(ltmil, p.est = timing, rloc = rlocsmil$rloc) 
      # allow up to 80km2 mvmt within the same resident range
      mbmil2 <- refine(mbmil, p.est = uk64)  
      # NO ERROR #
      
      
      ## greeley ##
      gre <- droplevels(gre)
      ltgre <- as.ltraj(xy = gre[,c("X", "Y")], 
                   # note Date must be POSIXct
                   date = gre$Date, 
                   # specify indiv 
                   id = gre$AnimalID)
      # identify best starting location for each indivdual
      rlocsgre <- findrloc(ltgre)
      # define base model, rNSD with expanded duration parameter 
      mbgre <- mvmtClass(ltgre, p.est = timing, rloc = rlocsgre$rloc) 
      # allow up to 80km2 mvmt within the same resident range
      mbgre2 <- refine(mbgre, p.est = uk64)      
      # NO ERROR #
      
      
      ## pio ##
      pio <- droplevels(pio)
      ltpio <- as.ltraj(xy = pio[,c("X", "Y")], 
                   # note Date must be POSIXct
                   date = pio$Date, 
                   # specify indiv 
                   id = pio$AnimalID)
      # identify best starting location for each indivdual
      rlocspio <- findrloc(ltpio)
      # define base model, rNSD with expanded duration parameter 
      mbpio <- mvmtClass(ltpio, p.est = timing, rloc = rlocspio$rloc) 
      # allow up to 80km2 mvmt within the same resident range
      mbpio2 <- refine(mbpio, p.est = uk64)
      # ERROR #
      
      
      
      ## sag ##
      sag <- droplevels(sag)
      ltsag <- as.ltraj(xy = sag[,c("X", "Y")], 
                   # note Date must be POSIXct
                   date = sag$Date, 
                   # specify indiv 
                   id = sag$AnimalID)
      # identify best starting location for each indivdual
      rlocssag <- findrloc(ltsag)
      # define base model, rNSD with expanded duration parameter 
      mbsag <- mvmtClass(ltsag, p.est = timing, rloc = rlocssag$rloc) 
      # allow up to 80km2 mvmt within the same resident range
      mbsag2 <- refine(mbsag, p.est = uk64)      
      
      
      
  ##  verify just madison and pioneers causing issue ##
      
      modlocs3 <- filter(modlocs, Herd != "Madison" & Herd != "Pioneers")
      modlocs3 <- droplevels(modlocs3)
      unique(modlocs3$Herd)
      lt3 <- as.ltraj(xy = modlocs3[,c("X", "Y")], 
               # note Date must be POSIXct
               date = modlocs3$Date, 
               # specify indiv 
               id = modlocs3$AnimalID)
      rlocs3 <- findrloc(lt3)
      m3base <- mvmtClass(lt3, p.est = timing, rloc = rlocs3$rloc)
      m3ref1 <- refine(m3base, p.est = uk64)
      # yes, ok, error does not occur when those popns are removed
      # so drill into mad and pio. first look at plots
      
      
      # 61150 migrant rNSD goes negative, that can't be good
      # ditto 61640
      # PM120080 may as well, just barely
      # alright, try it with these indivs removed
      
      madpio <- filter(modlocs, Herd == "Madison" | Herd == "Pioneers")
      madpio <- droplevels(madpio)
      length(unique(madpio$AnimalID))
      madpiosub <- filter(madpio, AnimalID != 61150 & AnimalID != 61640 & AnimalID != "PM120080")
      length(unique(madpiosub$AnimalID))     
      
      modlocs4 <- droplevels(madpiosub)
      lt4 <- as.ltraj(xy = modlocs4[,c("X", "Y")], 
               # note Date must be POSIXct
               date = modlocs4$Date, 
               # specify indiv 
               id = modlocs4$AnimalID)
      rlocs4 <- findrloc(lt4)
      m4base <- mvmtClass(lt4, p.est = timing, rloc = rlocs4$rloc)
      m4ref1 <- refine(m4base, p.est = uk64)
      # nope, still issue, recheck plots, looking for convergence issues
      
      # PM120094, PM120066 no res
      madpiosub2 <- filter(madpiosub, AnimalID != "PM120094" & AnimalID != "PM120066")
      madpiosub2 <- droplevels(madpiosub2)
      length(unique(madpiosub2$AnimalID))  
      modlocs5 <- droplevels(madpiosub2)
      lt5 <- as.ltraj(xy = modlocs5[,c("X", "Y")], 
               # note Date must be POSIXct
               date = modlocs5$Date, 
               # specify indiv 
               id = modlocs5$AnimalID)
      rlocs5 <- findrloc(lt5)
      m5base <- mvmtClass(lt5, p.est = timing, rloc = rlocs5$rloc)
      m5ref1 <- refine(m5base, p.est = uk64)
      # nope, still issue. guess i gotta try each individual, ughhhhhh
      
      # make sure issue still occurs with initial constraintt o save a step
      m6base <- mvmtClass(lt5, p.est = uk64, rloc = rlocs5$rloc)
      # yep ok
      
      # let's see if we can figure out try()
      try(mvmtClass(lt5, p.est = uk64, rloc = rlocs5$rloc))
      

      te <- mpindiv[2,1]
      test <- filter(madpio, AnimalID == te)
      
      
      
      madpio <- filter(modlocs, Herd == "Madison" | Herd == "Pioneers")
      madpio <- droplevels(madpio)
      mpindiv <- data.frame(AnimalID = unique(madpio$AnimalID))     
      mplt <- as.ltraj(xy = madpio[,c("X", "Y")], date = madpio$Date, id = madpio$AnimalID)
      mprloc <- findrloc(mplt)
      
      mprloc[1]
      mprloc[[1]]
      mprloc[1,1]
      mprloc[1,]
      
      str(mplt[1])
      
      
      for (i in 1:nrow(mpindiv)) {
        ind <- as.character(mpindiv[i,1])
        #ind <- droplevels(ind)
        #ilocs <- filter(madpio, AnimalID == ind)
        #ilocs <- droplevels(ilocs)
        #ilocs$AnimalID = as.factor(ilocs$AnimalID)
        #ilt <- as.ltraj(xy = ilocs[,c("X", "Y")], date = ilocs$Date, id = ilocs$AnimalID)
        #irloc <- findrloc(ilt)
        print(ind)
        try(mvmtClass(mplt[i], p.est = 64))
      }

        
      # ok. i have failed to subset indivs and create indiv ltrajs
      # i have failed to subset from within the group ltraj
      # i cannot fucking figure out how to identify problem individuals
      # ok, think, you got this. start over.

      
            
      madpio <- filter(modlocs, Herd == "Madison" | Herd == "Pioneers")
      madpio <- droplevels(madpio)
      mpindiv <- data.frame(AnimalID = unique(madpio$AnimalID))     
      mplt <- as.ltraj(xy = madpio[,c("X", "Y")], date = madpio$Date, id = madpio$AnimalID)
      mprloc <- findrloc(mplt)
      mm <- mvmtClass(mplt, p.est = uk64)
      # that worked?!
      mm2 <- mvmtClass(mplt, rloc = mprloc$rloc, p.est = uk64)
      # oh. and that didn't. 
      # so the issue was the fucking rlocs all along? fucking christ.
      View(mprloc)
      table(rlocs$rloc)
      hist(rlocs$rloc)
      
      
      
      # steps to address
      
      # 1. rerun full model, all indivs, without using rlocs
      #   1a. check whether top models change
      # 2. if top models do not change, roll on
      # 3. if top models do change, try subsetting madpio by rloc to id problem indivs
      
      
      mbase2 <- mvmtClass(lt, p.est = timing)
      mbase2ref <- refine(mbase2, p.est = uk64)
      mbase2ref2 <- refine(mbase2, p.est = ld50)
      length(which(!fullmvmt(mref2))) # 15 remaining convergence issues

      
    # identify top model for each individual #
      
      # require 2 months on summer range; require move 5km
      mtop2 <- topmvmt(mbase2ref2, omit = "mixmig", mrho = 60, mdelta = 25)
      topmods2 <- data.frame(AnimalID = modindivs, PrelimClassn = names(mtop2))
      write.csv(topmods2, file = "./rNSDresults/initialclassns-NSD.csv", row.names=F)
      
      topmodsdiff <- topmods %>%
        rename(rNSDclassn = MigClassn) %>%
        left_join(topmods2, by = "AnimalID") %>%
        rename(NSDclassn = PrelimClassn) %>%
        mutate(Samesies = ifelse(rNSDclassn == NSDclassn, 1, 0))
      any(topmodsdiff$Samesies == 0)
      any(topmodsdiff$Samesies == 1)
      length(which(topmodsdiff$Samesies == 0))
      diffs <- filter(topmodsdiff, Samesies == 0)
      # 67 differences, hm  
      # looking at them it appears the rlocs version does a better job of capturing real world
      # which means i still need to figure out the problem individuals
      
      
      
      
      
            
            
      madpio <- filter(modlocs, Herd == "Madison" | Herd == "Pioneers")
      madpio <- droplevels(madpio)
      mpindiv <- data.frame(AnimalID = unique(madpio$AnimalID))     
      mplt <- as.ltraj(xy = madpio[,c("X", "Y")], date = madpio$Date, id = madpio$AnimalID)
      mprloc <- findrloc(mplt)
      mm <- mvmtClass(mplt, p.est = uk64)
      # that worked?!
      mm2 <- mvmtClass(mplt, rloc = mprloc$rloc, p.est = uk64)
      # oh. and that didn't. 
      # so the issue was the fucking rlocs all along? fucking christ.
      View(mprloc)
      table(rlocs$rloc)
      hist(rlocs$rloc)
      
      z <- droplevels(filter(mprloc, rloc == 1))
      y <- droplevels(semi_join(madpio, z, by = c("AnimalID" = "burst")))
      x <- as.ltraj(xy = y[,c("X", "Y")], date = y$Date, id = y$AnimalID)
      w <- findrloc(x)
      
      
      rlocnums <- data.frame(num = unique(mprloc$rloc))
      
      for(i in 1:nrow(rlocnums)){
        
        paste0("grp", i)
        
        isub <- droplevels(filter(mprloc, rloc == i))
        
        lsub <- droplevels(semi_join(madpio, isub, by = c("AnimalID" = "burst")))
        ltsub <- as.ltraj(xy = lsub[,c("X", "Y")], date = lsub$Date, id = lsub$AnimalID)
        rsub <- findrloc(ltsub)
        
        try(mvmtClass(ltsub, p.est = uk64, rloc = rsub$rloc))
      }
      
    # yeah ok you prob shouldve subsetted rlocs correctly the first time
      # but this works better anyway
      
      # errors occur in rloc = 14 and rloc = 15 only
      
      # so do you change this for all of them? 
      # id prefer to constrain rlocs to <14 for all indivs when first creating it if possible
      # yep, not only possible... it's easy!
      
      
      
  #### subsetting 2 random locs per day (one day, one night) ####
      
          

      
    testlocs <- rawlocs %>%
        # just test on 2 indivs
        filter(AnimalID == 140060 | AnimalID == 140480) %>%
        # create POSIXct DateTime for ltraj object; pull just date (Day) from this
        mutate(Date = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")) %>%
        mutate(Day = as.Date(DateTime)) %>%
        # identify time of day
        mutate(Hour = as.numeric(substr(Time, 0, 2))) %>%
        mutate(TimeOfDay = ifelse(Hour >= 8 & Hour <= 20, "Day", "Night")) %>%
        # randomly select one loc per time of day (so, 2 locs per 24-hour period) per indiv
        group_by(AnimalID, Day, TimeOfDay) %>%
        sample_n(1) %>%
        ungroup()
      
      
      
  
                
  #### fixing "duplicate" datetime issue when creating ltraj object ####
      
      test <- modlocs
      test$Date <- as.POSIXct(strptime(test$DateTime, format = "%Y-%m-%d %H:%M:%S"))
  #### Convert Lat/Longs to UTMs ####
  test <- as.data.frame(spTransform(SpatialPointsDataFrame(
                            data.frame("X" = test$Longitude, 
                                       "Y" = test$Latitude), 
                            test, proj4string = latlong), utm))

  #### Create ltraj object ### 
  testlt <- as.ltraj(xy = test[,c("X", "Y")], 
                 # note Date must be POSIXct
                 date = test$Date, 
                 # specify indiv 
                 id = test$AnimalID)
  # still have issue
    
  test2 <- modlocs
  test2 <- test2[1:1000,]
  test2$Date <- as.POSIXct(test2$DateTime, format = "%Y-%m-%d %H:%M:%S")
    test2 <- as.data.frame(spTransform(SpatialPointsDataFrame(
                            data.frame("X" = test2$Longitude, 
                                       "Y" = test2$Latitude), 
                            test2, proj4string = latlong), utm))
    test2 <- droplevels(test2)
      test2lt <- as.ltraj(xy = test2[,c("X", "Y")], 
                 # note Date must be POSIXct
                 date = test2$Date, 
                 # specify indiv 
                 id = test2$AnimalID)
      
      ## identify duplicates ##
      
      test <- modlocs %>%
        group_by(AnimalID, Date) %>%
        distinct() %>%
        mutate(n = n()) %>%
        filter(n == 2)

      View(test)
      
      # I HATE DAYLIGHT SAVINGS TIME
      

         test <- modlocs
      test$Date <- as.POSIXct(strptime(test$DateTime, format = "%Y-%m-%d %H:%M:%S"))
  #### Convert Lat/Longs to UTMs ####
  test <- as.data.frame(spTransform(SpatialPointsDataFrame(
                            data.frame("X" = test$Longitude, 
                                       "Y" = test$Latitude), 
                            test, proj4string = latlong), utm))
      test <- test %>%
        filter(!is.na(Date))
        #### Create ltraj object ### 
  testlt <- as.ltraj(xy = test[,c("X", "Y")], 
                 # note Date must be POSIXct
                 date = test$Date, 
                 # specify indiv 
                 id = test$AnimalID)
  
  modlocs <- modlocs %>% filter(!is.na(Date))
  
  
  
  #### why the fuck are there still duplicate dates?! ####
  
  
        
      test <- modlocs %>%
        group_by(AnimalID, Date) %>%
        distinct() %>%
        mutate(n = n()) %>%
        filter(n == 2)

      View(test)
      
      
      
  #### infinity produced error AGAIN now that dates are fixed, uuugh ####
      
      # last time this was due to rlocs, pulling code from above to check whether that's still the case
      
      testmbase <- mvmtClass(lt, p.est = timing) # same base model, just without using rlocs
      length(which(!fullmvmt(testmbase))) # 40 convergence issues, more than with rlocs
      testmref1 <- refine(testmbase, p.est = uk64) # refine() that causes error
      # confirmed, not using rlocs solves the issue
      
      # steps to address
      
      # 1. determine whether excluding rlocs appreciably changes results
      # 2. if it does, identify individuals causing the error and manually change their rlocs
      # 3. if it does not, roll with NSD rather than rNSD
      
 # STEP 1
        
      # migrant only has to move 50 km2 (to incl short-distance migrants)
      ld50 <- pEst(u.r = 240, u.t = 150, l.d = 50)
      testmref2 <- refine(testmref1, p.est = ld50)
      length(which(!fullmvmt(testmref2))) # only 8 remaining convergence issues

      
    # identify top model for each individual #
      
      # require 2 months on summer range; require move 5km
      testmtop <- topmvmt(testmref2, omit = "mixmig", mrho = 60, mdelta = 25)
      testtopmods <- data.frame(AnimalID = modindivs, PrelimClassn = names(testmtop))
      write.csv(testtopmods, file = "./rNSDresults/test4-norlocs/initialclassns.csv", row.names=F)
      
      # read in results of a prior run 
      testold <- read.csv("./rNSDresults/test3-whereinoticedrndmlocissue/compareclassns-test1test3.csv")
      
      testcomp <- testold %>%
        rename(run1R = PrelimClassn, run2R = migrateRclassn, run1ME = NewBehav, ren2ME = behavClassn) %>%
        left_join(testtopmods, by = "AnimalID") %>%
        rename(thisrun = PrelimClassn) %>%
        mutate(run2R = ifelse(run2R == "nomad", "disperser", paste(run2R))) %>%
        mutate(diff1 = ifelse(thisrun == run1R, 0, 1)) %>%
        mutate(diff2 = ifelse(thisrun == run2R, 0, 1)) %>%
        mutate(difftot = diff1+diff2)
      testdiffs <- testcomp %>%
        filter(difftot > 0)
      write.csv(testdiffs, './rNSDresults/test4-norlocs/rsltscomparison.csv', row.names=F)
      
     ## plots ###

      num.plots <- nrow(modindivs)
      my.plots <- vector(num.plots, mode='list')
      
      for(i in 1:num.plots) {
        plot(testmref2[[i]])
        my.plots[[i]] <- recordPlot()
      }
      graphics.off()
      
      pdf('./rNSDresults/test4-norlocs/migbehav-plots-LINES.pdf', onefile = TRUE)
      for(my.plot in my.plots) {
        replayPlot(my.plot)
      }
      graphics.off()
      
      
      
      ## ok somethings up, there is no fucking way these are the same individuals in these plots
      ## plots for, e.g., 140600 and 140480 look ENTIRELY different between some of these runs
      
      
  #### checking out unclassified points ("outliers or points in transit") ####
      
      # 141000 (not likely issue)
      test <- spatmig(lt[26], testmref2[26]) # sweet, stored as list of numbers
      table(test)
      792-304-322 # 166 unclassified
      166/792 # 21 pct
      
      # 140980 (maybe issue)
      test <- spatmig(lt[25], testmref2[25]) # sweet, stored as list of numbers
      table(test)
      792-433-242 # 117 unclassified
      # ok so outliers are not n issue for her
      
      # 141150 (likely issue)
      test <- spatmig(lt[34], testmref2[34]) # sweet, stored as list of numbers
      table(test)
      792-267-210
      315/792 # 40% unclassified, could explain issue
      
      # 31113073 [77]
      test <- spatmig(lt[77], testmref2[77]) # sweet, stored as list of numbers
      table(test)
      791-449-118
      224/791 # 28%, so maybe consider >25%?
      
      
    #### rNSD issue is a thing, wah wah ####    
      
      # eg 140480, 140600. so...
      
   # STEP 2
      
      # rNSD does seem important to use; i expect it to clear up the DIFFINDIVS issue
      
      # so, start by identifying indivs causing infinity error
      # then try just iteratively reducing it by only one day until issues hopefully go away

      # define parameter causing the issue
      uk64 <- pEst(u.k = log(64))
      
      # dataframe to store error messages in
      errors <- data.frame(AnimalID = unique(modlocs$AnimalID), Err = NA)
      
      # for each individual
      for(i in 1:nrow(rlocs)) {
        
        # subset its locations
        ilocs <- droplevels(semi_join(modlocs, rlocs[i,], by = c("AnimalID" = "burst")))
        
        # make it ltraj
        ilt <- as.ltraj(xy = ilocs[,c("X", "Y")], date = ilocs$Date, id = ilocs$AnimalID)
        
        # try the model
        tryCatch(mvmtClass(ilt, p.est = uk64, rloc = rlocs[i,"rloc"]), error = function(e) {
                errors[i,"Err"] <<- conditionMessage(e)
                NULL
            })
      }
      # fuck. yes.
      
      errorindivs <- errors %>%
        filter(!is.na(Err)) %>%
        left_join(rlocs, by = c("AnimalID" = "burst"))
        

      # replace rlocs with date just prior
      rloctest <- rlocs
      rloctest$newrloc <- ifelse(rloctest$burst == 60900, rloctest$rloc-1, rloctest$rloc)
      
      rloctest <- rlocs
      rloctest$newrloc <- ifelse(rloctest$burst == errorindivs[1,1] | rloctest$burst == errorindivs[2,1] | 
                                 rloctest$burst == errorindivs[3,1], rloctest$rloc-1, rloctest$rloc)
      rloctest$DDiff = rlocs$newrloc - rlocs$rloc
        
        
               
  #### examine percent of outliers per indiv ####
      
      
        # create list to store spatmig outputs in
        pts <- list()  
        
      # for each individual
      for(i in 1:nrow(indivs)) {
        # store list of point classifications
        pts[[i]] <- spatmig(lt[i], mref2[i], graph = F)
        
        # extract length of list, ppn that's NULL (i think)
      }
      
      
    test <- pts[5]
    length(test[[1]])
    # i have a list of lists
    # each of those sublists is just a list of 1
    # and you want to extract information from that list of 1
    # test is one sublist
    test[[1]] # ok this is everything in the list
    length(test[[1]])
    lengths(test[[1]]) # great, this is the number i want
    # now how do i count things inside there?
    lengths(which(!is.na(test[[1]])))
    test[[1]][1] # still everything
    test[[1]][[1]] # still everything
    
    length(test[[1]][[1]])
    length(which(is.na(test[[1]][[1]])))
    
    
    # after running through many iterations
    # i do not think this will be informative
    test <- pts[21]
    length(which(is.na(test[[1]][[1]])))/lengths(test[[1]])
    

    