### ### ### ### ### ### ### ### ### ### ### ### ### #
#       IDENTIFYING PRIVATE AND PUBLIC LAND         #
#  ON WINTER HOME RANGES OF HERDS AND INDIVIDUALS   #
#                   KRISTIN BARKER                  #
#                    JANUARY 2018                   #
### ### ### ### ### ### ### ### ### ### ### ### ### #




### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###



  #### Packages ####
  
    library(raster) # ...rasters...
    library(sp) # spatial
    library(rgeos) # gIntersection (clip spatial opjs)
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
  
      
      
    #### Spatial data ####
      

      
      # buffered popn winter ranges of (to crop cadastral to)
      hrs <- shapefile("../GIS/Shapefiles/Elk/IndivHRs/AllWinHRsMergedBuff")
      


      # identify relevant cadastral files
       files.cad <- list.files(
            path = "../DatabasesEtc/Statewide/LandOwnership",
            pattern = ".shp$",
            full.names = TRUE)
       
       
      # cadastral for each year, clipped to speed processing       
       for(i in 1:length(files.cad)) {  
         
         # for each cadastral
         cadname <- files.cad[i]
         cadyr <- substr(cadname, nchar(cadname)-9, nchar(cadname)-8)
         outname <- paste0("cad", cadyr, "mt")
         
         # read in as spatialpolygonsdataframe
         cadi <- shapefile(cadname)
         
         # transform home range spdf to match cadastral projection
         hrs.crs <- spTransform(hrs, crs(cadi)) # i'm sure there's a better way...
         
         # clip cadastral to home ranges
         clip <- raster::intersect(cadi, hrs.crs) 
         
         # store
         assign(outname, clip)
         
         # export, jic
         writeOGR(clip, dsn = "../GIS/Shapefiles/Land",
           layer = outname,
           driver = "ESRI Shapefile",
           overwrite_layer = TRUE)      


       }
       
       #beep("sword")
       #save.image(file = "ownership.RData")

   
       
   #### After this I used arcmap to:

    # hand-digitize relevant areas missing from wyoming
      # join wyoming data to each shapefile
        # [saved with "wy" appended to file name]
    # merge indivwinHRs in each popn [to speed processing and
      # to avoid wasting time on land use classns outside HRs]
       
              
       # clean up objects that are no longer needed since arcmap work
       rm(list = ls()[grep("cad", ls())], i, clip, outname)
       
     
       
       
         
### ### ### ### ### ### ### ### ### #
####  |MERGING OWNERSHIP TYPES|  ####
### ### ### ### ### ### ### ### ### #
       
 
       
      #### Standardize column names ####
       
       
        ## identify and prep files ##
       
          # identify cropped land ownership files to use
          cadlist <- list.files(path = "../GIS/Shapefiles/Land",
            pattern = "popcad.+shp$",full.names = TRUE)
       
           # read in and store each file (naming convention = "cad"+yr)
           for (i in 1:length(cadlist)) {
             inname <- cadlist[i]
             outname <- substr(inname, nchar(inname)-8, nchar(inname)-4)
             assign(outname, shapefile(inname))
           }
    
         
       
       ## standardize column names across files ##
           
           # for pre-2012 cadastrals
           oldcadlista <- c("cad08", "cad10", "cad11")
           for (i in 1:length(oldcadlista)) {
            cadi <- get(oldcadlista[i]) 
            dati <- data.frame(
              myClassn = NA,
              owner = cadi@data$OWNR_NAM1,
              descr = cadi@data$GENDESCRIP,
              propType = cadi@data$PROPTYPE,
              grazAcre = cadi@data$GRAZING_AC,
              irrigAcre = cadi@data$IRRIG_ACRE,
              hayAcre = cadi@data$WILD_HAY_A)
            cadi@data = dati
            assign(paste0(oldcadlista[i], "upd"), cadi)
           }
           
           # for 2012 and later cadastrals
           oldcadlistb <- c("cad12", "cad13", "cad14", "cad16")
           for (i in 1:length(oldcadlistb)) {
            cadi <- get(oldcadlistb[i]) 
            dati <- data.frame(
              myClassn = NA,
              owner = cadi@data$OwnerName,
              descr = NA,
              propType = cadi@data$PROPTYPE,
              grazAcre = cadi@data$GrazingAcr,
              irrigAcre = cadi@data$IrrigatedA,
              hayAcre = cadi@data$WildHayAcr)
            cadi@data = dati
            assign(paste0(oldcadlistb[i], "upd"), cadi)
           }


        
           
           
           
    #### Classify AGR and obvious DEV land ####
        
        # using the files with standardized column names
        for (i in 1:length(cadlist)) {
          
          # identify the file
          cadi <- get(cadlist[i])
          
          # and the year (to name output file at end)
          yr <- substr(cadlist[i], 4, 5)
          
          # update land use classification per parcel as follows:
          cadi@data <- mutate(cadi@data,
            # if any grazing, irrigated, or wild hay acreage, class "agricultural"
            myClassn = ifelse(grazAcre > 0 | irrigAcre > 0 | hayAcre > 0, "AGR", 
             # exempt properties, mining claims, and NAs require manual classn
             ifelse(grepl("exempt", propType), "UNK",
               # so call them "unknown" for now
               ifelse(grepl("mining", propType), "UNK",
                 # all property types other than the above are "developed"
                 ifelse(is.na(propType), "UNK", "DEV")))))
          
          # store as r object
          assign(paste0("lu", yr), cadi)
          
          # and export to arcmap to check out the unknowns
          writeOGR(cadi, dsn = "../GIS/Shapefiles/Land",
            layer = paste0("lu", yr),
            driver = "ESRI Shapefile",
            overwrite_layer = TRUE)      
       }

        
           
           
           