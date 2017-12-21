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
    modindivs <- read.csv("modindivs.csv")
    locs <- allcowlocs %>%
      semi_join(modindivs, by = "AnimalID")
  
  
  #### Projections ####
  
    latlong <- CRS("+init=epsg:4326")
    stateplane <- CRS("+init=epsg:2818")
  
  
  
    
### ### ### ### ### ###
####  |DATA PREP|  ####
### ### ### ### ### ###
  
  
    
  #### Define and subset info relevant for popn home ranges ####

       
    # growing season locations (for covariate extractions)
    popnlocsgrow <- allcowlocs %>%
      mutate(Year = as.numeric(substr(Date, 0, 4)),
             Month = as.numeric(substr(Date, 6, 7))) %>%
      # only keep indivs from popns and years interest
      semi_join(popnyrs, by = c("Herd", "Year")) %>%
      # only locns collected during growing season (may-aug)
      filter(Month >= 05 & Month <= 08)  %>%
      # remove stored data about filtered out herds (too few indivs)
      mutate(Herd = factor(Herd))


    # winter locations (for density estimates)
    popnlocswin <- allcowlocs %>%
        mutate(Year = as.numeric(substr(Date, 0, 4)),
               Month = as.numeric(substr(Date, 6, 7))) %>%
        # only locns collected during winter
        filter(Month == 12 | Month == 1 | Month == 2) %>%
        # map december locs to following year's winter
        mutate(Year = ifelse(Month == 12, Year + 1, Year)) %>%
        # only keep indivs from popns and years interest
        semi_join(popnyrs, by = c("Herd", "Year")) %>%
        # remove stored data about filtered out herds and indivs
        mutate(Herd = factor(Herd), AnimalID = factor(AnimalID))
    write.csv(popnlocswin, file = "popnlocs-win.csv", row.names=F)


    # winter locations with couple-month buffer (for winter covariate extraction)
    popnlocswinbuff <- allcowlocs %>%
        mutate(Year = as.numeric(substr(Date, 0, 4)),
               Month = as.numeric(substr(Date, 6, 7))) %>%
        # only locns collected during winter
        filter(Month == 12 | Month == 1 | Month == 2 | Month == 3 | Month == 4) %>%
        # map december locs to following year's winter
        mutate(Year = ifelse(Month == 12, Year + 1, Year)) %>%
        # only keep indivs from popns and years interest
        semi_join(popnyrs, by = c("Herd", "Year")) %>%
        # remove stored data about filtered out herds and indivs
        mutate(Herd = factor(Herd), AnimalID = factor(AnimalID))


    
  #### Define and subset info relevant for indiv home ranges  ####

    
    # winter locations (to create individual winter home ranges)
    indivlocswin <- locs %>%
      mutate(Year = as.numeric(substr(Date, 0, 4)),
             Month = as.numeric(substr(Date, 6, 7))) %>%
      # only locns collected during winter (dec-feb, note captures didn't start until jan)
      filter(Month == 12 | Month == 1 | Month == 2) %>%
      # map december locs to following year's winter
      mutate(Year = ifelse(Month == 12, Year + 1, Year)) %>%
      # only use locs from season of interest
      filter(Year == YearOfInterest) %>%
      # only indivs with at least 5 relocs (min required for hr estimation)
      group_by(AnimalID) %>%
      filter(n() > 5) %>%
      ungroup() %>%
      # remove stored data about filtered out indivs (too few locs)
      mutate(AnimalID = factor(AnimalID),
             Herd = factor(Herd))
    write.csv(indivlocswin, "indivlocswin.csv", row.names=F)
    

    # winter locations with couple-month buffer
    indivlocswinbuff <- locs %>%
      mutate(Year = as.numeric(substr(Date, 0, 4)),
             Month = as.numeric(substr(Date, 6, 7))) %>%
      # only locns collected during winter (dec-feb, note captures didn't start until jan)
      filter(Month == 12 | Month == 1 | Month == 2) %>%
      # map december locs to following year's winter
      mutate(Year = ifelse(Month == 12, Year + 1, Year)) %>%
      # only indivs with at least 5 relocs (min required for hr estimation)
      group_by(AnimalID) %>%
      filter(n() > 5) %>%
      ungroup() %>%
      # remove stored data about filtered out indivs (too few locs)
      mutate(AnimalID = factor(AnimalID),
             Herd = factor(Herd))
    
      
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
             dsn = "../GIS/Shapefiles/Elk", 
             layer = "AllElkInterestLocs", 
             driver = "ESRI Shapefile",
             overwrite_layer = TRUE)
   
     
    ## export locs separately per population
    
    popns <- unique(popnlocsall$Herd)
    for (i in 1:length(popns)) {
     popn <- popns[i] 
     popnlocs <- filter(popnlocsall, Herd == popn)
     # create and reproject spdf in one line 
     popn.sp <- spTransform(SpatialPointsDataFrame(data.frame("x"=popnlocs$Long,"y"=popnlocs$Lat),
                                                   popnlocs, proj4string = latlong), stateplane)
     writeOGR(popn.sp,
              dsn = "../GIS/Shapefiles/Elk/PopnHRs", 
              layer = paste0(popn, "-allcowlocs"), 
              driver = "ESRI Shapefile",
              overwrite_layer = TRUE)
    }

    
   
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
    
    
    ## estimate full yr hrs for each popn separately
    popnhrskde <- kernelUD(spdf.sp[,3], grid = 120) #,3 is Herd
    popnhrs <- getverticeshr(popnhrskde, percent = 99)
    
    ## add year of interest
    popnhrs@data <- left_join(popnhrs@data, popnyrs, by = c("id" = "Herd"))
    
    ## export full year hr for each popn
    writeOGR(popnhrs,
             verbose = TRUE,
             dsn = "../GIS/Shapefiles/Elk/PopnHRs", 
             layer = "PopnYrHRs", 
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
           dsn = "../GIS/Shapefiles/Elk/PopnHRs", 
           layer = "PpnGrowingSsnHRs", 
           driver = "ESRI Shapefile",
           overwrite = TRUE)
  
    ## export individual locations 
    writeOGR(spdf.sp, 
         dsn = "../GIS/Shapefiles/Elk", 
         layer = "PpnGrowingSsnLocns", 
         driver = "ESRI Shapefile",
         overwrite = TRUE)
    

    
  #### Winter #### 
  
    ## get xy points; write to dataframe, to spatial data frame, to stateplane
    spdf.sp <- spTransform(SpatialPointsDataFrame(data.frame("x"=popnlocswin$Longitude,
                                                             "y"=popnlocswin$Latitude),
                            popnlocswin, proj4string = latlong), stateplane)
    
 
    ## estimate mcp for each population
    popnwinmcps <- mcp(spdf.sp[,"Herd"], percent = 100,
                   unin = "m", unout = "km2")
    
    ## export population mcps
    writeOGR(popnwinmcps, 
         dsn = "../GIS/Shapefiles/Elk/PopnHRs", 
         layer = "PpnWinMCPs", 
         driver = "ESRI Shapefile",
         overwrite = TRUE)
    
 
       
    ## estimate kde for each population
    popnwinuds <- kernelUD(spdf.sp[,"Herd"], h = "href")
                             #, extent = 2) # need extent > default of 1 
    popnwinkdes <- getverticeshr(popnwinuds, percent = 95)
  

    ## export population kdes 
    writeOGR(popnwinkdes, 
         dsn = "../GIS/Shapefiles/Elk/PopnHRs", 
         layer = "PpnWinKDEs", 
         driver = "ESRI Shapefile",
         overwrite = TRUE)    
    
    
    ## store winter hr area (for density estimation)
    write.csv(popnwinmcps@data, file = "popn-winhr-areasMCP.csv", row.names = F)
    write.csv(popnwinkdes@data, file = "popn-winhr-areasKDE.csv", row.names = F)

    
    
    
    #### Winter with couple-month buffer - just locs ####
        
        ## make spatial etc etc...
        spdf.sp2 <- spTransform(SpatialPointsDataFrame(data.frame("x"=popnlocswinbuff$Longitude,
                                                                 "y"=popnlocswinbuff$Latitude),
                                popnlocswinbuff, proj4string = latlong), stateplane)
        
        ## export all locations 
        writeOGR(spdf.sp2, 
             dsn = "../GIS/Shapefiles/Elk/IndivLocs", 
             layer = "IndivWinLocs2moBuff", 
             driver = "ESRI Shapefile",
             overwrite = TRUE)    
        
        
        
        ## create population kdes of buffered winter timeframe
        popnwinbuffuds <- kernelUD(spdf.sp2[,"Herd"], h = "href")
        popnwinbuffkdes <- getverticeshr(popnwinbuffuds, percent = 99)
  
        
        ## and export them
        writeOGR(popnwinbuffkdes, 
             dsn = "../GIS/Shapefiles/Elk/PopnHRs", 
             layer = "PopnWinBuffKDEs", 
             driver = "ESRI Shapefile",
             overwrite = TRUE)    
      
        
        

    
### ### ### ### ### ###
####  |INDIV HRS|  ####
### ### ### ### ### ###
  
  
    #### Winter ####

      #### get xy points; write to dataframe, to spatial data frame, to stateplane ####
      spdf.sp <- spTransform(SpatialPointsDataFrame(data.frame("x"=indivlocswin$Longitude,"y"=indivlocswin$Latitude), 
                                                    indivlocswin, proj4string = latlong), stateplane)
      
    
      # # COMMENTED OUT BC NEED TO DEFINE WINTER BASED ON MIG MVMT PARAMS, NOT GENERIC MONTHS
      # # estimate kdes
      # indivhrswin.kde <- kernelUD(spdf.sp[,"AnimalID"], 
      #                             h = "href",
      #                             extent = 2) # need extent > default of 1 
      # indivhrswin.kde <- getverticeshr(indivhrswin.kde, percent = 95)
      # plot(indivhrswin.kde)
      
      
      # estimate mcps
      indivhrswin.mcp <- mcp(spdf.sp[,"AnimalID"], percent = 100)
      plot(indivhrswin.mcp, col = "blue")
      
      # #### export individual home ranges ####
      # writeOGR(indivhrswin.kde, 
      #          dsn = "../GIS/Shapefiles/Elk/IndivHRs", 
      #          layer = "IndivWinKDEs", 
      #          driver = "ESRI Shapefile",
      #          overwrite = TRUE)
      
        # writeOGR(indivhrswin.mcp, 
        #        dsn = "../GIS/Shapefiles/Elk/IndivHRs", 
        #        layer = "IndivWinMCPs", 
        #        driver = "ESRI Shapefile",
        #        overwrite = TRUE)
      
      
      # #### export individual locations ####
      # writeOGR(spdf.sp, 
      #          dsn = "../GIS/Shapefiles/Elk/IndivLocs", 
      #          layer = "IndivWinLocs", 
      #          driver = "ESRI Shapefile",
      #          overwrite = TRUE)
      

      
  #### Winter with couple-month buffer ####
      
        ## create population mcps of buffered winter timeframe
        ## just for cows of interest (to determine herd overlap)
        spdf.sp3 <- spTransform(SpatialPointsDataFrame(data.frame("x"=indivlocswinbuff$Longitude,
                                                                 "y"=indivlocswinbuff$Latitude),
                                indivlocswinbuff, proj4string = latlong), stateplane)
        indivwinbuffuds <- kernelUD(spdf.sp3[,"AnimalID"], h = "href", extent = 14)
        indivwinbuffkdes <- getverticeshr(indivwinbuffuds, percent = 95)
  
        
        ## and export them
        writeOGR(popnwinbuffkdes, 
             dsn = "../GIS/Shapefiles/Elk/PopnHRs", 
             layer = "PopnWinBuffKDEs", 
             driver = "ESRI Shapefile",
             overwrite = TRUE)    
    
  
  
### ### ###  ### ### ### 
#### ~ NEXT STEPS ~ ####
### ### ###  ### ### ### 


# create population growing season home range EXCLUSIVE of individual winter season home range
  # and store with indiv identifier, to to use in calculating what's avail on winter range
    # vs what's avail outside that area during summer