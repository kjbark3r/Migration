### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#     CREATING HOME RANGES FOR HERDS AND INDIVIDUALS      #
#    TO ASSESS FACTORS INFLUENCING MIGRATORY BEHAVIOR     #
#                   KRISTIN BARKER                        #
#                    OCTOBER 2017                         #
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###




### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###



  #### Packages ####
  
    library(sp) # for kernel centroid estimate
    library(adehabitatHR) # for kernel centroid estimate
    library(rgdal) # for latlong/stateplane conversions
    library(gsubfn)
    library(maptools) # for writeSpatialShape
    library(ggmap)
    library(dplyr) # for joins and general awesomeness
  
  

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
  
    
  
  #### "Raw" data ####
  
  
    # Read in populations, years, and individuals of interest 
    popnyrs <- read.csv("popns-yrs.csv") %>%
      within(Year <- as.numeric(Year))
    locs <- read.csv("locs.csv") %>%
      within(Year <- as.numeric(Year))
    allcowlocs <- read.csv("locs-allcows.csv") %>%
      factor(Sex)
  
  
  #### Projections ####
  
    latlong <- CRS("+init=epsg:4326")
    stateplane <- CRS("+init=epsg:2818")
  
  
  
    
### ### ### ### ### ###
####  |DATA PREP|  ####
### ### ### ### ### ###
  
  
    
  # Define and subset info relevant for popn home ranges
  popnlocs <- allcowlocs %>%
    mutate(Year = as.numeric(substr(Date, 0, 4)),
           Month = as.numeric(substr(Date, 6, 7))) %>%
    # only keep indiv locs from popns and years of interest
    semi_join(indivs, by = "AnimalID") %>%
    semi_join(popnyrs, by = c("Herd", "Year")) %>%
    # to use locns collected during growing season (may-aug)
    filter(Month >= 05 & Month <= 08)  %>%
    # remove stored data about filtered out herds (too few indivs)
    mutate(Herd = factor(Herd))

    
  # Identify individuals of interest
  indivs <- data.frame(AnimalID = unique(locs$AnimalID))
  str(indivs)
  


  
### ### ### ### ### ##
####  |POPN HRS|  ####
### ### ### ### ### ##
    
  
  
  # get xy points; write to dataframe, to spatial data frame, to stateplane projection
  xy <- data.frame("x"=popnlocs$Long,"y"=popnlocs$Lat)
  spdf.ll <- SpatialPointsDataFrame(xy, popnlocs, proj4string = latlong)
  spdf.sp <- spTransform(spdf.ll,stateplane)
  
  
  
  # estimate mcp for each population
  mcps <- mcp(spdf.sp[,3], percent = 100) #,3 = Herd

  
  
  #export population home ranges
  writeOGR(mcps, 
           dsn = "../GIS/Shapefiles/Elk", 
           layer = "PpnGrowingSsnHRs", 
           driver = "ESRI Shapefile",
           overwrite = TRUE)

  
 
    #export individual locations
  writeOGR(spdf.sp, 
           dsn = "../GIS/Shapefiles/Elk", 
           layer = "PpnGrowingSsnLocns", 
           driver = "ESRI Shapefile",
           overwrite = TRUE)