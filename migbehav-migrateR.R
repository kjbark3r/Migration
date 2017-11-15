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
    
    
    
 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
    
    
### ### ### ### ### ###
####    |MODELS|   ####
### ### ### ### ### ###     

    
    # identify best starting location for each indivdual
    rlocs <- findrloc(lt)
 
    # define base model with no tweaks
    mb <- mvmtClass(lt, rloc = rlocs$rloc)
    
    
    # refine delta to avoid convergence issues
    mr <- refine(mb, p.est = pEst(s.d = -1))
    mr
    

    # omit mixed-migrant model and require 2mo duration on range2
    mot2 <- topmvmt(mr, omit = "mixmig", mrho = 90)
    attributes(mot2) 
        

    # store results
    rslts <- data.frame(attributes(mots)) %>%
      rename(AnimalID = burst, Behav = names) 
	  
	  

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
		  


