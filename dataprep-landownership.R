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
       
          # identify cropped land ownership files created in arcmap
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


        
           
    #### First classify AGR and obvious DEV land ####
        
        # using the files with standardized column names  
        cadlist <- grep("upd", objects(), value = TRUE)   

        for (i in 1:length(cadlist)) {
          
          # identify the file
          cadi <- get(cadlist[i])
          
          # and the year (to name output file at end)
          yr <- substr(cadlist[i], 4, 5)
          
          # update land use classification per parcel as follows:
          cadi@data <- mutate(cadi@data, myClassn = 
            # "agricultural" if any acreage of irrigated
            ifelse(irrigAcre > 0, "AGR", 
            # other exempt properties, mining claims, and NAs require manual classification
            ifelse(grepl("[Ee]xempt", propType), "UNK",
               # so call them "unknown" for now
               ifelse(grepl("[Mm]ining", propType), "UNK",
                 # all property types other than the above are "developed"
                 ifelse(is.na(propType), "UNK", "DEV")))))    

          
          # store as r object
          assign(paste0("lu", yr), cadi)
   
       }

        
        
    #### Then use info from older cadastrals to classify some of the remaining unknowns ####
        
           
        # make df of unknown classifications that incls ownership and descrip info from old cadastrals   
        olddescr <- rbind(data.frame(lu08@data), data.frame(lu10@data), data.frame(lu11@data)) %>%
              dplyr::select(owner, descr, myClassn) %>%
              filter(myClassn == "UNK") %>%
              # remove USA owner bc has multiple possible descrips (can be NPS, BLM, etc)   
              filter(owner != "UNITED STATES OF AMERICA" & !is.na(owner)) %>%
              # define correct descrips for USFS and MT Dept of Transportation
              mutate(descr = ifelse(owner == "USDA FOREST SERVICE", "USFS",
                ifelse(owner == "MONTANA DEPARTMENT OF TRANSPOR", "MT State", paste(descr)))) %>%
              # remove duplicate entries
              distinct() %>%
              mutate(owner = as.character(owner), descr = as.character(descr))
           
        

        # make df of just descrs and classns 
        known <- data.frame(descr = unique(olddescr$descr), myClassn = NA) %>%
          # classify obvious public and developed lands as such & classify water NA
          mutate(myClassn = ifelse(descr == "Public" | descr == "USFS" | 
                                   descr == "BLM" | descr == "NPS", "PUB", 
                            ifelse(descr == "Private" | descr == "Utl Ease" | 
                                   descr == "RgtOfWay", "DEV", 
                            ifelse(descr == "Water", "NA", "UNK"))),
            descr = as.character(descr)) 
        
        
        # make df of owners and updated classns
        classns <- olddescr %>%
          dplyr::select(-myClassn) %>%
          left_join(known, by = "descr") %>%
          rename(newClassn = myClassn) %>%
          dplyr::select(-descr)
        
        # and another of just owners and descrips
        ownrs <- dplyr::select(olddescr, -myClassn)
        

        # add descrips to newer cadastrals that didn't have them
        newcadlist <- c("lu12", "lu13", "lu14", "lu16")
        for (i in 1:length(newcadlist)) {
          lui <- get(newcadlist[i])
          lui@data <- lui@data %>%
            mutate(owner = as.character(owner)) %>%
            dplyr::select(-descr) %>%
            left_join(ownrs, by = "owner")
          assign(newcadlist[i], lui)
        }        
        
       
        # gave in and manually made csv of updated classns based on ownership
        mandesc <- read.csv("manualdescrips.csv") %>%
          rename(manClassn = myClassn) %>%
          mutate(manClassn = as.character(manClassn))
        
         
        # add myClassns from updated descrips
        lulist <- grep("^lu[[:digit:]]", objects(), value = TRUE)  
        
        
        # if myClassn is UNK or NA, update with new classns from descrips df
        for (i in 1:length(lulist)) {
          
         lui <- get(lulist[i])
         
         lui@data <- lui@data %>%
           left_join(classns, by = "owner") %>%
           mutate(newClassn = ifelse(is.na(newClassn), "UNK", newClassn)) %>%
           mutate(myClassn = ifelse(myClassn == "UNK", newClassn, myClassn)) %>%
           left_join(mandesc, by = "descr") %>%
           mutate(manClassn = ifelse(is.na(manClassn), "UNK", manClassn)) %>%
           mutate(myClassn = ifelse(myClassn == "UNK", manClassn, myClassn)) %>%
           dplyr::select(-c(newClassn, manClassn)) %>%
           mutate(temp = ifelse(is.na(owner), myClassn, ifelse(owner == "BLM", "PUB", myClassn))) %>%
           rename(Classn = temp) %>%
           dplyr::select(-myClassn)

         
         assign(paste0("new", lulist[i]), lui)
         
         
          # and export to arcmap to check out remaining unknowns
          writeOGR(lui, dsn = "../GIS/Shapefiles/Land",
            layer = paste0("lu", substr(lulist[i],3,4)),
            driver = "ESRI Shapefile",
            overwrite_layer = TRUE)
         
        }
           

        
      #### make owner match Water or RgtOfWay descrs
        
        # remove and read back in in "lu" shps (bc i just manually updated WY owners in arcmap, not my smartest plan)
        rm(lu08, lu10, lu11, lu12, lu13, lu14, lu16, newlu08, newlu10, newlu11, newlu12, newlu13, newlu14, newlu16)
       
 
        # identify land ownership files updated in arcmap
        newlulist <- list.files(path = "../GIS/Shapefiles/Land",
          pattern = "lu.+shp$",full.names = TRUE)
     
         # fix water and rightofway owners; re-export
         for (i in 1:length(newlulist)) {
           
           lui <- shapefile(newlulist[i])
           lui@data$owner = ifelse(is.na(lui@data$descr), lui@data$owner,
             ifelse(lui@data$descr == "Water", "Water",
             ifelse(lui@data$descr == "RgtOfWay", "RgtOfWay", lui@data$owner)))
           

           
          assign(paste0("new", lulist[i]), lui)
         
         
          # and export to arcmap to check out remaining unknowns
          writeOGR(lui, dsn = "../GIS/Shapefiles/Land",
            layer = paste0("lu", substr(lulist[i],3,4)),
            driver = "ESRI Shapefile",
            overwrite_layer = TRUE)
         }

   
       
        
       save.image(file = "landuse.RData")
       

        

  
           
