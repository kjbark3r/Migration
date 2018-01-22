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
         # hrs.crs <- spTransform(hrs, crs(cadi)) # you already ran this once
         
         # clip cadastral to home ranges
         clip <- raster::intersect(cadi, hrs.crs) # this crashed r last time
         
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

       
       
       
### ### ### ### ### ### ### ### ### #
####  |MERGING OWNERSHIP TYPES|  ####
### ### ### ### ### ### ### ### ### #         
        
  # cleanup in attempt to free some memory
       rm(cadi, cadname, cadyr, clip, outname, i, files.cad)
       # yeah that did nothing
       
       colnames(cad08mt@data)
       colnames(cad09mt@data)
       colnames(cad10mt@data)
       colnames(cad11mt@data)
       colnames(cad12mt@data) # gendescrip dies here
       colnames(cad08mt@data)
       colnames(cad08mt@data)
       colnames(cad08mt@data)
       
       
       # identify all unique OwnerNames starting in 2012
       # ultimately reassign all other than matching what 08-10 GENDESCRIP has to pvt
       
       descrip <- list(unique(cad08mt@data$GENDESCRIP))
       descrip
       
       c12 <- data.frame(Nms = unique(cad12mt@data$OwnerName))
       c08dat <- cad08mt@data
       
       # check out property types for classification
       write.csv(unique(cad08mt@data$PROPTYPE), "./zOldAndMisc/proptypes1.csv", row.names=F)
       write.csv(unique(cad16mt@data$PropType), "./zOldAndMisc/proptypes2.csv", row.names=F)
       
       # create df of just GENDESCRIP, OWNR_NAM1
       descrips <- cad11mt@data %>%
         dplyr::select(GENDESCRIP, OWNR_NAM1) %>%
         rename(OwnerName = OWNR_NAM1) %>%
         distinct()

       
       # join to 2012 data and see what's still missing
       test12 <- left_join(cad12mt@data, descrips, by = "OwnerName")
       nas <- filter(test12, is.na(GENDESCRIP)) %>%
         dplyr::select(OwnerName, GENDESCRIP) %>%
         distinct()
       ## see data prep notes for list of things to grep and fix, others are pvt or NA
    
 
       
       
### ### ### ### ### ### ### ### #
####  |OWNERSHIP PER INDIV|  ####
### ### ### ### ### ### ### ### # 
       
       # read in winter hr of each individual
       indivhrs <- shapefile("../GIS/Shapefiles/Elk/IndivHRs/AllWinHRs")
       
       # index these by @data$id (=AnimalID)
       