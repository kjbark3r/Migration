### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
###       CLASSIFYING MIGRATORY BEHAVIOR OF ELK         ###
###              KRISTIN BARKER 2017-2018               ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


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
    
    
    
    #### Base model - no tweaks ####
    m.base <- mvmtClass(modlocs)
    m.base
    
    
    #### Refine (with negative delta value?) to fix convergence issues ####
    m.ref <- refine(m.base,  p.est = pEst(s.d = -1))
	  all(fullmvmt(m.ref))
	  
	  
	  #### Omit mixed-migrant classification
	  mod <- topmvmt(m.ref, omit = "mixmig")
