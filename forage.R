### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#  CALCULATING MAXIMUM "FORAGE" AND "FORAGE" VARIATION    #
#    ON POPULATION-LEVEL GROWING SEASON HOME RANGES       #
#                   KRISTIN BARKER                        #
#                    OCTOBER 2017                         #
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###




### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###



  #### Packages ####
  
    library(raster) # ...rasters...
    library(sp) # spatial
    library(rgdal) # projections; working with shps
    library(maptools) # writeSpatialShape
    library(beepr) # alarm when code finishes
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
    rm(wd_workcomp, wd_laptop, wd_worklaptop)
    

    
    #### Population data  ####
    
    
      # populations of interest (from dataprep-elkdb.R)
      popnsyrs <- read.csv("popns-yrs.csv")
      
      # add population code that matches start of tif filenames
      popdat <- popnsyrs %>%
        mutate(Pop = ifelse(Herd == "Blacktail", "bla",
                     ifelse(Herd == "Clarks Fork", "cfk",
                     ifelse(Herd == "Dome", "dom",
                     ifelse(Herd == "East Fork", "efk", 
                     ifelse(Herd == "Elkhorns", "elk",
                     ifelse(Herd == "Greeley", "grl",
                     ifelse(Herd == "HD314", "gal",
                     ifelse(Herd == "Madison", "mad",
                     ifelse(Herd == "Mill Creek", "mil",
                     ifelse(Herd == "NMadison", "nmd",
                     ifelse(Herd == "Pioneers", "pio",
                     ifelse(Herd == "Sage Creek", "sge",
                     ifelse(Herd == "Sapphire", "sap",
                     ifelse(Herd == "Silver Run", "sil",
                     ifelse(Herd == "Tobacco Roots", "tob",
                     ifelse(Herd == "West Fork", "wfk", 
                            NA))))))))))))))))) 

      
      
      # identify population codes to loop through
      popcodes <- popdat$Pop
      npops <- length(popcodes)
      
      
      
### ### ### ### ### ##
####  |PRED FOR|  ####
### ### ### ### ### ## 
      
      
    # PredFor is a measure of how predictably forage varies across the popn range during the growing season #
      
      
      # create data frame to store results in
      predfor <- data.frame(Pop = popcodes, 
                      StDevAmp = NA,
                      StDevTi = NA)

      # calculate predictability of variation in available "forage"
      for (i in 1:npops) {
        
        # for each population
        pop <- popcodes[i]
        
        # read in 6 yrs ndvi amplitude data (from rasterclip.R)
        files.amp <- list.files(
            path = "./NDVIamp",
            pattern = paste0("^", noquote(pop)),
            full.names = TRUE)
        ampstkprelim <- stack(files.amp)

        # read in 6 yrs time-integrated ndvi data (from rasterclip.R)
        files.ti <- list.files(
            path = "./NDVIti",
            pattern = paste0("^", noquote(pop)),
            full.names = TRUE)
        tistkprelim <- stack(files.ti)

        # reclassify values of 255 as NA (these represent water)
        rc <- function(x) { ifelse(x == 255, NA, x) }
        ampstk <- overlay(ampstkprelim, fun = rc)
        names(ampstk) <- names(ampstkprelim)
        tistk <- overlay(tistkprelim, fun = rc)
        names(tistk) <- names(tistkprelim)
        
        # calculate standard deviation of "forage" data for each pixel
        # of population growing season range
        ampvar <- calc(ampstk, fun = sd)
        tivar <- calc(tistk, fun = sd)
        
        # average standard deviation across entire growing season range
        asd <- cellStats(ampvar, stat = 'sd')
        tsd <- cellStats(tivar, stat = 'sd')
        
        # store results
        predfor[i, 2] <- asd
        predfor[i, 3] <- tsd
          
      }
      
      
      # format dataframe and export
      outdat <- popdat %>%
        left_join(predfor, by = "Pop") %>%
        dplyr::select(-c(nIndiv, Year))
      write.csv(outdat, file = "predfor.csv", row.names = F)
      
      beep()

    
      
      
      
### ### ### ### ### ###
####  |DELTA FOR|  ####
### ### ### ### ### ###       
      
      
    # DeltaFor is a measure of how different the forage is within and outside an elk's winter range #
      
      
      #### Data prep ####
      
          # read in individual winter home ranges
          indivhrswin <- shapefile("../GIS/Shapefiles/Elk/IndivHRs/AllWinHRs")
          
      
          # and population ranges
          popnhrs <- shapefile("../GIS/Shapefiles/Elk/PopnHRs/PopnYrHRs")
    
          
          # create dataframe of individual, herd, year of interest
          indivdatraw <- read.csv("dens-indiv.csv")
          
          
          # add popn code (for storing and retrieving popn hrs per indiv)
          indivdat <- indivdatraw %>%
            dplyr::select(-Year) %>%
            left_join(popdat, by = "Herd")
          
          
          # identify indivs
          indivlist <- unique(indivhrswin@data$id)
          nindiv <- length(indivlist)
          
          
          # full area of interest for cropping (from homeranges.R and post-hoc ArcMap buffering)
          aoiraw <- readOGR("../GIS/Shapefiles/Elk", layer = 'AreaOfInterest')
          
          
          # read in ndvi amplitude data (from https://phenology.cr.usgs.gov/get_data_250w.php)
          files.amp <- list.files(
            path = "../DatabasesEtc/Statewide/NDVIamp/",
            pattern = "tif$",
            full.names = TRUE)
          ampstk <- stack(files.amp)
          names(ampstk)
          ampstk@crs
            
          
          # read in time-integrated ndvi data  (from https://phenology.cr.usgs.gov/get_data_250w.php)
          files.ti <- list.files(
            path = "../DatabasesEtc/Statewide/NDVIti/",
            pattern = "tif$",
            full.names = TRUE)
          tistk <- stack(files.ti)
          names(tistk)
          
          
          # crop all ndvi data to area of interest (to speed processing)
          aoi <- spTransform(aoiraw, crs(ampstk)) # match aoi proj to ndvi
          ampcropprelim <- crop(ampstk, aoi) # crop (note: makes stacks to bricks
          ticropprelim <- crop(tistk, aoi)   ##       which speeds processing)
          
          
          # reclassify values of 255 as NA (these represent water)
          rc <- function(x) { ifelse(x == 255, NA, x) }
          ampcrop <- overlay(ampcropprelim, fun = rc)
          names(ampcrop) <- names(ampcropprelim)
          ticrop <- overlay(ticropprelim, fun = rc)
          names(ticrop) <- names(ticropprelim)
          
          
          # store cropped and reclassified ndvi data
          writeRaster(ampcrop, paste0("../GIS/Shapefiles/NDVI/", names(ampcrop)), 
            bylayer = TRUE, format = "GTiff", overwrite = TRUE)
          writeRaster(ticrop, paste0("../GIS/Shapefiles/NDVI/", names(ticrop)), 
            bylayer = TRUE, format = "GTiff", overwrite = TRUE)          
    
          
          # make home ranges match ndvi projection
          indivhrswin2 <- spTransform(indivhrswin, crs(ampcrop))
          popnhrs2 <- spTransform(popnhrs, crs(ampcrop))
          

          # create data frame to store results in
          deltafor <- data.frame(AnimalID = unique(indivhrswin2@data$id), 
                          MaxAmpIn = NA, MaxAmpOut = NA,
                          MaxTiIn = NA, MaxTiOut = NA)

      
          
      ##### Maximum available "forage" within each individual's winter range ####
          
          
          for (i in 1:nindiv) {
            
            # for each elk, identify year of interest and correct home range
            elk <- indivdat[i,"AnimalID"]
            yr <- indivdat[i,"Year"]
            hr <- subset(indivhrswin2, id == elk)
            
            # read in correct yrs' ndvi data 
            amp <- subset(ampcrop, paste0("amp", yr))
            ti <- subset(ticrop, paste0("ti", yr))
            
            # identify ndvi cells that are within or touching edge of home range
            ampcells <- cellFromPolygon(amp, hr, weights = TRUE)[[1]][, "cell"]
            ticells <- cellFromPolygon(ti, hr, weights = TRUE)[[1]][, "cell"]
            
            # set all other cells to NA
            amp[][-ampcells] <- NA
            ti[][-ticells] <- NA
    
            # remove NA cells
            amp1crop <- trim(amp)
            ti1crop <- trim(ti)
            
            # calculate maximum "forage" within winter range
            maxampin <- cellStats(amp1crop, stat = 'max')
            maxtiin <- cellStats(ti1crop, stat = 'max')
            
            # store results
            deltafor[i, "AnimalID"] <- elk
            deltafor[i, "MaxAmpIn"] <- maxampin
            deltafor[i, "MaxTiIn"] <- maxtiin
    
          }
          
          
          write.csv(deltafor, "deltafor-prelim.csv", row.names=F)

          
      
      ##### Maximum available "forage" outside each individual's winter range ####
          
          
          ## first create and store "forage" polygons for each popn ##

          for (i in 1:npops) {
            
            # for each population, identify year of interest and correct home range
            herd <- popdat[i,"Herd"]
            popcode <- popdat[i, "Pop"]
            yr <- popdat[i,"Year"]
            hr <- subset(popnhrs2, id == herd)
            
            # read in correct yrs' ndvi data 
            amp2 <- subset(ampcrop, paste0("amp", yr))
            ti2 <- subset(ticrop, paste0("ti", yr))
            
            # match home range projection to ndvi data
            hr2 <- spTransform(hr, crs(amp2))
            
            # identify ndvi cells that are within or touching edge of home range
            amp2cells <- cellFromPolygon(amp2, hr2, weights = TRUE)[[1]][, "cell"]
            ti2cells <- cellFromPolygon(ti2, hr2, weights = TRUE)[[1]][, "cell"]
            
            # set all other cells to NA
            amp2[][-amp2cells] <- NA
            ti2[][-ti2cells] <- NA
    
            # remove NA cells
            amp2crop <- trim(amp2)
            ti2crop <- trim(ti2)
            
            # store separately for each population
            ampname <- paste0("amp.", popcode)
            tiname <- paste0("ti.", popcode)
            
            assign(ampname, amp2crop)
            assign(tiname, ti2crop)

          }
          
          
          ## then calculate max "forage" *exclusive* of each indiv's winter hr ##
    
              for (i in 1:nindiv) {
                

                # for each elk, identify individual, herd, and herd code
                elk <- indivdat[i,"AnimalID"]
                herd <- indivdat[i, "Herd"]
                popcode <- indivdat[i, "Pop"]
                
                # identify population-range ndvi rasters
                popamp <- paste0("amp.", popcode)
                popti <- paste0("ti.", popcode)
                
                # pull rasters
                ampdat <- get(popamp)
                tidat <- get(popti)
                
                # identify indiv hr & match projection to ndvi data
                hri <- subset(indivhrswin2, id == elk)
                hri2 <- spTransform(hri, crs(ampdat))
                
                # remove indiv hr area from population range area
                exclamp <- mask(ampdat, hri2, inverse = TRUE)
                exclti <- mask(tidat, hri2, inverse = TRUE)
    
                # calculate maximum "forage" outside winter range
                maxampout <- cellStats(exclamp, stat = 'max')
                maxtiout <- cellStats(exclti, stat = 'max')
                
                # store results
                deltafor[i, "MaxAmpOut"] <- maxampout
                deltafor[i, "MaxTiOut"] <- maxtiout
          
                                
                
              }

          
      
       ## finally, calculate and store difference between max "forage" inside and outside winter HR ##
          
          deltafor$deltaAmp <- deltafor$MaxAmpOut - deltafor$MaxAmpIn
          deltafor$deltaTi <- deltafor$MaxTiOut - deltafor$MaxTiIn
          
          write.csv(deltafor, "deltafor.csv", row.names=F)
                    
          beep()
          
          save.image(file = "for.RData")
      

      
