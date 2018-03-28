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
    library(rgeos) # clip polygons
    library(adehabitatHR) # home ranges and kernel centroids
    library(rgdal) # latlong/stateplane conversions
    library(raster) # shapefile()
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
  
    
  
  #### "Raw" data  ####

    # winter home range for each individual (from homeranges.R)
    indivhrsprelim <- shapefile("../GIS/Shapefiles/Elk/IndivHRs/AllFebHRs") 

    # list of land use files
    lulist <- list.files(path = "../GIS/Shapefiles/Land", pattern = "lu.+shp$",full.names = TRUE)

    # read in and store each land use file (naming convention = "lu"+yr)
     for (i in 1:length(lulist)) {
       inname <- lulist[i]
       outname <- substr(inname, nchar(inname)-7, nchar(inname)-4)
       assign(outname, shapefile(inname))
     }
    
    
        
  #### Data prep  ####   
    
    
    # dataframe of individuals, herds, and years of interest
    indivdat <- indivhrsprelim@data %>% 
      rename(AnimalID = id, HRarea = area, YOI = Year)

    
    # dataframe to store new data in
    moddat <- indivdat %>%
      mutate(nOwn = NA, acreAg = NA) 

    
    # match land use and home range projections
    indivhrs <- spTransform(indivhrsprelim, crs(lu08)) # match aoi proj to ndvi
    
    
    
  
### ### ### ### ### ### ### ### #
####  |OWNERSHIP PER INDIV|  ####
### ### ### ### ### ### ### ### # 
       
    
     # for each individual
     for (i in 1:nrow(moddat)) {
       
       # identify elk and year of interest
       elk <- moddat[i,"AnimalID"]
       yoi <- moddat[i, "YOI"]
       
       # pull last 2 digits of year to identify correct land use file
       yrsub <- ifelse(yoi == 2015, 14, # no cadastral data for 2015, use 2014
         ifelse(yoi == 2006, "08", substr(yoi, 3, 4))) # and none for 2006, use 2008

       # pull correct winter home range
       hr <- subset(indivhrs, id == elk)
       
       # pull land use file from that year
       lui <- get(paste0("lu", yrsub))
       
       # remove leading or trailing spaces from landowner names
       lui@data$owner <- trimws(lui@data$owner)
       
       # clip land use to indiv winter range
       luclip <- raster::intersect(lui, hr)

       # calculate and store number unique landowners on winter range
       moddat[i, "nOwn"] <- length(unique(luclip@data$owner))
       
       # calculate and store acres of irrigated ag on winter range
       moddat[i, "acreAg"] <- sum(luclip@data$irrigAcre)
       
     }
     

    
    # calculate ownership density and proportion irrigated ag
    moddat <- moddat %>%
      mutate(densOwn = nOwn/HRarea, 
             ppnAg = (0.004047*acreAg)/HRarea,
             irrig = ifelse(ppnAg > 0, 1, 0))
    
    summary(moddat$densOwn)
    summary(moddat$ppnAg)
    length(which(moddat$irrig == 1))
    
    # export updated model data file
    write.csv(moddat, "human-covariates-feb.csv", row.names = F)
    