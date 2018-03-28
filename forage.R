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
    library(beepr) # exciting alarm when code finishes
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
          indivhrswin <- shapefile("../GIS/Shapefiles/Elk/IndivHRs/AllFebHRs")
          
      
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
          
          
          # read in max ndvi data (from https://phenology.cr.usgs.gov/get_data_250w.php)
          files.maxn <- list.files(
            path = "../DatabasesEtc/Statewide/NDVImax/",
            pattern = "tif$",
            full.names = TRUE)
          maxnstk <- stack(files.maxn)
          names(maxnstk)
          maxnstk@crs

          
          # crop all ndvi data to area of interest (to speed processing)
          aoi <- spTransform(aoiraw, crs(maxnstk)) # match aoi proj to ndvi
          maxncropprelim <- crop(maxnstk, aoi) # crop

          
          
          # reclassify values of 255 as NA (these represent water)
          rc <- function(x) { ifelse(x == 255, NA, x) }
          maxncrop <- overlay(maxncropprelim, fun = rc)
          names(maxncrop) <- names(maxncropprelim)

          
          
          # store cropped and reclassified ndvi data
          writeRaster(maxncrop, paste0("../GIS/Shapefiles/NDVI/", names(maxncrop)), 
            bylayer = TRUE, format = "GTiff", overwrite = TRUE)

          
          # make home ranges match ndvi projection
          indivhrswin2 <- spTransform(indivhrswin, crs(maxncrop))
          popnhrs2 <- spTransform(popnhrs, crs(maxncrop))
          

          # create data frame to store results in
          deltafor <- data.frame(AnimalID = unique(indivhrswin2@data$id), 
                          maxNDVIin = NA, maxNDVIout = NA)


          
      ##### Maximum available "forage" within each individual's winter range ####
          
          
          for (i in 1:nindiv) {
            
            # for each elk, identify year of interest and correct home range
            elk <- indivdat[i,"AnimalID"]
            yr <- indivdat[i,"Year"]
            hr <- subset(indivhrswin2, id == elk)
            
            # read in correct yrs' ndvi data 
            maxn <- subset(maxncrop, paste0("maxn", yr))

            # identify ndvi cells that are within or touching edge of home range
            maxncells <- cellFromPolygon(maxn, hr, weights = TRUE)[[1]][, "cell"]

            # set all other cells to NA
            maxn[][-maxncells] <- NA

            # remove NA cells
            maxn1crop <- trim(maxn)

            # calculate maximum "forage" within winter range
            maxnin <- cellStats(maxn1crop, stat = 'max')

            # store results
            deltafor[i, "AnimalID"] <- elk
            deltafor[i, "maxNDVIin"] <- maxnin

          }

          
          write.csv(deltafor, "deltafor-prelim-feb.csv", row.names=F)

          
      
      ##### Maximum available "forage" outside each individual's winter range ####
          
          
          ## first create and store "forage" polygons for each popn ##

          for (i in 1:npops) {
            
            # for each population, identify year of interest and correct home range
            herd <- popdat[i,"Herd"]
            popcode <- popdat[i, "Pop"]
            yr <- popdat[i,"Year"]
            hr <- subset(popnhrs2, id == herd)
            
            # read in correct yrs' ndvi data 
            maxn2 <- subset(maxncrop, paste0("maxn", yr))

            # match home range projection to ndvi data
            hr2 <- spTransform(hr, crs(maxn2))
            
            # identify ndvi cells that are within or touching edge of home range
            maxn2cells <- cellFromPolygon(maxn2, hr2, weights = TRUE)[[1]][, "cell"]

            # set all other cells to NA
            maxn2[][-maxn2cells] <- NA

            # remove NA cells
            maxn2crop <- trim(maxn2)

            # store separately for each population
            maxnname <- paste0("maxn.", popcode)

            assign(maxnname, maxn2crop)

          }

          
          ## then calculate max "forage" *exclusive* of each indiv's winter hr ##
    
              for (i in 1:nindiv) {
                

                # for each elk, identify individual, herd, and herd code
                elk <- indivdat[i,"AnimalID"]
                herd <- indivdat[i, "Herd"]
                popcode <- indivdat[i, "Pop"]
                
                # identify population-range ndvi rasters
                popmaxn <- paste0("maxn.", popcode)

                # pull rasters
                maxndat <- get(popmaxn)

                # identify indiv hr & match projection to ndvi data
                hri <- subset(indivhrswin2, id == elk)
                hri2 <- spTransform(hri, crs(maxndat))
                
                # remove indiv hr area from population range area
                exclmaxn <- mask(maxndat, hri2, inverse = TRUE)

                # calculate maximum "forage" outside winter range
                maxnout <- cellStats(exclmaxn, stat = 'max')

                # store results
                deltafor[i, "maxNDVIout"] <- maxnout
       
                
              }

          
      
       ## finally, calculate and store difference between max "forage" inside and outside winter HR ##
          
          deltafor$deltaNDVI <- deltafor$maxNDVIout - deltafor$maxNDVIin

          write.csv(deltafor, "deltafor-feb.csv", row.names=F)
                    
          beep()
          
          save.image(file = "for.RData")
      

      
