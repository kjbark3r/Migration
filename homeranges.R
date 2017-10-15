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
  
    library(sp) # spatial
    library(rgeos) # buffer
    library(adehabitatHR) # home ranges and kernel centroids
    library(rgdal) # latlong/stateplane conversions
    library(gsubfn) # no idea, possibly unnecessary
    library(maptools) # writeSpatialShape
    library(ggmap) # in case you want to plot in R
    library(dplyr) # joins, data work, general awesomeness
  
  

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
  
    
  
  #### "Raw" data (processed in dataprep-elkdb.R) ####
  
  
    # Populations & years of interest 
    popnyrs <- read.csv("popns-yrs.csv") %>%
      within(Year <- as.numeric(Year))
    
    # All female locations
    allcowlocs <- read.csv("locs-allcows.csv") %>%
      mutate(Sex = factor(Sex))
    allindivs <-  data.frame(AnimalID = unique(allcowlocs$AnimalID))
    
    # Females to use in actual model
    locs <- read.csv("locs.csv") %>%
      within(Year <- as.numeric(Year))
    modindivs <- data.frame(AnimalID = unique(locs$AnimalID))
  
  
  #### Projections ####
  
    latlong <- CRS("+init=epsg:4326")
    stateplane <- CRS("+init=epsg:2818")
  
  
  
    
### ### ### ### ### ###
####  |DATA PREP|  ####
### ### ### ### ### ###
  
  
    
  #### Define and subset info relevant for popn home ranges ####
    
    

    
    
    
    # growing season (for covariate extractions)
    popnlocsgrow <- allcowlocs %>%
      mutate(Year = as.numeric(substr(Date, 0, 4)),
             Month = as.numeric(substr(Date, 6, 7))) %>%
      # only keep indivs from popns and years interest
      semi_join(popnyrs, by = c("Herd", "Year")) %>%
      # only locns collected during growing season (may-aug)
      filter(Month >= 05 & Month <= 08)  %>%
      # remove stored data about filtered out herds (too few indivs)
      mutate(Herd = factor(Herd))
    
    

    
    # #### * IN PROGRESS * ####
    # 
    # # winter (for density estimates)
    # ## need to define winter as including previous year's december,
    # ## and maybe november (depends on timing of population estimates)
    #     popnlocsgrowwin <- allcowlocs %>%
    #   mutate(Year = as.numeric(substr(Date, 0, 4)),
    #          Month = as.numeric(substr(Date, 6, 7))) %>%
    #   # only keep indivs from popns and years interest
    #   semi_join(popnyrs, by = c("Herd", "Year")) %>%
    #   # only locns collected during growing season (may-aug)
    #   filter(Month >= 05 & Month <= 08)  %>%
    #   # remove stored data about filtered out herds (too few indivs)
    #   mutate(Herd = factor(Herd))
    # 
    # #### *  * ####
    #     
    # 
    # 
    # #### Define and subset info relevant for indiv home ranges  ####
    # 
    #   #### * IN PROGRESS * ####
    # 
    #   ## below code loses some indivs for some reason
    #   ## you should probably figure that out
    #   ## (already started in troubleshooting-locs)
    #   indivlocs <- locs %>%
    #     mutate(Year = as.numeric(substr(Date, 0, 4)),
    #            Month = as.numeric(substr(Date, 6, 7))) %>%
    #     # only locns collected during winter (dec-feb, but captures didn't start until jan)
    #     filter(Month >= 01 & Month <= 03)  %>%
    #     # remove stored data about filtered out herds (too few indivs)
    #     mutate(Herd = factor(Herd)) %>%
    #     group_by(AnimalID) %>%
    #     filter(n() > 5)
    # 
    #   #### *  * ####

  
### ### ### ### ### ##
####  |POPN HRS|  ####
### ### ### ### ### ##
    
    
  #### Full year ####
    
    # full year, buffered (for cropping large covariate rasters)
    popnlocsall <- allcowlocs %>%
      mutate(Year = as.numeric(substr(Date, 0, 4)),
             Month = as.numeric(substr(Date, 6, 7))) %>%
      # only keep indivs from popns and years interest
      semi_join(popnyrs, by = c("Herd", "Year")) %>%
      # remove stored data about filtered out herds (too few indivs)
      mutate(Herd = factor(Herd))
    

    ## get xy points; write to dataframe, to spatial data frame, to stateplane
    xy <- data.frame("x"=popnlocsall$Long,"y"=popnlocsall$Lat)
    spdf.ll <- SpatialPointsDataFrame(xy, popnlocsall, proj4string = latlong)
    spdf.sp <- spTransform(spdf.ll,stateplane)
    
    ## export all locs of all elk in popns and yrs of interest
    writeOGR(spdf.sp,
             verbose = TRUE,
             dsn = "../GIS/Shapefiles/Elk", 
             layer = "AllElkInterestLocs", 
             driver = "ESRI Shapefile",
             overwrite_layer = TRUE)
    
    ## estimate hrs for all elk together
    popnhrsallkde <- kernelUD(spdf.sp, grid = 110)
    popnhrsoutline <- getverticeshr(popnhrsallkde, percent = 100)
    
    ## export unbuffered, holey area of all elk ranges all year
    ## because you never figured out how to deal with holes in the polygon
    writeOGR(popnhrsoutline,
             verbose = TRUE,
             dsn = "../GIS/Shapefiles/Elk", 
             layer = "AllElkPrelimHR", 
             driver = "ESRI Shapefile",
             overwrite_layer = TRUE)
    


    
  #### Growing season ####
  
    ## get xy points; write to dataframe, to spatial data frame, to stateplane
    xy <- data.frame("x"=popnlocsgrow$Long,"y"=popnlocsgrow$Lat)
    spdf.ll <- SpatialPointsDataFrame(xy, popnlocsgrow, proj4string = latlong)
    spdf.sp <- spTransform(spdf.ll,stateplane)
    
    ## estimate mcp for each population
    popnhrs <- mcp(spdf.sp[,3], percent = 100) #,3 = Herd
  
    ## export population home ranges
      writeOGR(popnhrs, 
           dsn = "../GIS/Shapefiles/Elk", 
           layer = "PpnGrowingSsnHRs", 
           driver = "ESRI Shapefile",
           overwrite = TRUE)
  
    ## export individual locations 
    writeOGR(spdf.sp, 
         dsn = "../GIS/Shapefiles/Elk", 
         layer = "PpnGrowingSsnLocns", 
         driver = "ESRI Shapefile",
         overwrite = TRUE)
    
    

  
### ### ### ### ### ###
####  |INDIV HRS|  ####
### ### ### ### ### ###
  
  
  
  #### * IN PROGRESS * ####
  #  pending above fixes  #

  #### get xy points; write to dataframe, to spatial data frame, to stateplane ####
  xy <- data.frame("x"=indivlocs$Long,"y"=indivlocs$Lat)
  spdf.ll <- SpatialPointsDataFrame(xy, indivlocs, proj4string = latlong)
  spdf.sp <- spTransform(spdf.ll,stateplane)
  
  
  #### estimate mcp for each indiv ####
  indivhrs <- mcp(spdf.sp[,1], percent = 100) #,1 = AnimalID
  
  
  
  #### export population home ranges ####
  writeOGR(indivhrs, 
           dsn = "../GIS/Shapefiles/Elk", 
           layer = "IndivWinHRs", 
           driver = "ESRI Shapefile",
           overwrite = TRUE)
  
  
  
  #### export individual locations ####
  writeOGR(spdf.sp, 
           dsn = "../GIS/Shapefiles/Elk", 
           layer = "IndivWinLocns", 
           driver = "ESRI Shapefile",
           overwrite = TRUE)
  
  #### *  * ####
  
  
### ### ###  ### ### ### 
#### ~ NEXT STEPS ~ ####
### ### ###  ### ### ### 


# create population growing season home range EXCLUSIVE of individual winter season home range
  # and store with indiv identifier, to to use in calculating what's avail on winter range
    # vs what's avail outside that area during summer