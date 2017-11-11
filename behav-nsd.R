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
  
  latlong <- CRS("+init=epsg:4326")
  stateplane <- CRS("+init=epsg:2818") #this is NAD83
  utm <- CRS("+init=epsg:3742") # NAD83(HARN)/UTMzone12N
  
  
  
  #### "Raw" data ####
  
  rawlocs <- read.csv("locs-allcows-withelevs.csv")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
      

### ### ### ### ### ###
####  |DATA PREP|  ####
### ### ### ### ### ###



  #### Pull 10 random rows if needed for little tests ####
  play <- rawlocs[sample(nrow(rawlocs), 10), ]
  head(play)   


  
  #### Subset and format raw data for use in models ####
  modlocs <- rawlocs %>%
    # only use Sapphires for trial runs
    filter(Herd == "Sapphire") %>%
    # create POSIXct DateTime for ltraj object
    mutate(Date = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")) %>%
    mutate(Day = as.Date(DateTime)) %>%
    # only use 1 year of data for each individual
    mutate(AnimalID = factor(AnimalID)) %>%
    group_by(AnimalID) %>%
    mutate(Day1 = min(as.Date(DateTime))) %>%
    ungroup() %>%
    filter(Day <= Day1+365) %>%
    mutate(AnimalID = factor(AnimalID)) %>%
    group_by(AnimalID, Day) %>%
    sample_n(1) %>%
    ungroup()
    # KRISTIN YOU STILL NEED TO 
    # subset to only incl indivs with full yr of locs

  
  
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



  #### Create dataframe to store results in ####
  results <- data.frame(Behav = c("resident", 
                                  "disperser",
                                  "migrant", 
                                  "mixmig",
                                  "nomad"))


    
    #### Create dataset of 10 "known status" individuals ####   


    # 140560 : migrant that moved furthest 
    # 140910 : migrant that moved least far
    # 140100 : resident with most core overlap
    # 141100 : resident with least core overlap
    # 140480 : resident with most HR overlap
    # 141490 : resident with least HR overlap
    # 141060 : intermediate with most HR overlap
    # 141130 : intermediate with least HR overlap
    # 140060 : intermediate that moved furthest
    # 140050 : intermediate that moved least far

    testindivs <- data.frame(
      AnimalID = c("140560", "140910", "140100",  "141100", "140480",  "141490", "141060",  "141130", "140060",  "140050"),
      Classn = c("M", "M", "R", "R", "R", "R", "I", "I", "I", "I"),
      Deets = c("Furthest", "LeastFar", "MostCore", "LeastCore", "MostHR", "LeastHR",  "MostHR", "LeastHR", "Furthest", "LeastFar"))
    testindivs$Combo = paste(testindivs$Classn, testindivs$Deets, sep = "-")
    
    testlocs <- inner_join(testindivs, modlocs, by = "AnimalID")
    unique(testlocs$AnimalID)
    str(testlocs)
    
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

    
    
    ####~GENERAL MODEL STUFF~####
        
        ##### effect of using different starting loc ####
        rlocs <- findrloc(testlt) # there is an effect for many; ponder for later
      
    
    
    ####~NSD MODELS~####
      
        #### straight nsd, no tweaks ####
        m.n1 <- mvmtClass(testlt)
        m.n1 # all mixmig
        
        
        #### tweak distance > 9km (from dist bt centroids of migrants) ####
        
            #### specified in initial model ####
            m.n2 <- mvmtClass(testlt, p.est = pEst(s.d = 9))
            m.n2 # all mixmig except 140100(strongR), now disperser
      
            #### specified for topmodel selection ####
            m.n3 <- topmvmt(m.n1, mdelta = 9000) 
            names(m.n3) # all resident
            names(m.n3[1])
            
        #### tweak distance > 2km (for comparison) ####
            
            #### specified in initial model ####
            m.n4 <- mvmtClass(testlt, p.est = pEst(s.d = 2))
            m.n4 # all mixmig except the residents, who are now mig (wtf)
            
            #### specified for topmodel selection ####
            m.n5 <- topmvmt(m.n1, mdelta = 200)
            names(m.n5) # all resident again
  
        
  
        
        
        
        

    ####~ELEVATION MODELS~####
      
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
    m.e3
    summary(m.e3)
    str(m.e3) # large list
    m.e3[1]
    names(m.e3)



    #### store everything ####             
    save.image(file = "./zOldAndMisc/nsd-prelim.RData")