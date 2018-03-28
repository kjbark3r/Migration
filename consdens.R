### ### ### ### ### ### ### ### ### ### ### ### ### ##
#    ESTIMATING CONSPECIFIC DENSITY DURING WINTER    #
#                   KRISTIN BARKER                   #
#                    NOVEMBER 2017                   #
### ### ### ### ### ### ### ### ### ### ### ### ### ##




### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###



    #### Packages ####
    
        library(sp) # spatial, over
        library(rgeos) # buffer, centroid
        #library(adehabitatHR) # home ranges and kernel centroids
        library(rgdal) # latlong/stateplane conversions; readOGR
        #library(gsubfn) # no idea, possibly unnecessary
        library(maptools) # writeSpatialShape
        library(raster) # intersect
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
    
    
    
    #### Projections ####
    
        latlong <- CRS("+init=epsg:4326")
        stateplane <- CRS("+init=epsg:2818")
    


    #### Population year info ####
    
        popnyrs <- read.csv("popns-yrs.csv")
        
        
    #### Winter HR polygons (HRs created in homeranges.R; popnHRs combined and processed in ArcMap) ####
        
        windens <- readOGR("../GIS/Shapefiles/Elk/PopnHRs", layer ='WinHRcombosFeb')
        windens <- spTransform(windens, stateplane)
        winindiv <- readOGR("../GIS/Shapefiles/Elk/IndivHRs", layer ='AllFebHRs')
        winindiv <- spTransform(winindiv, stateplane)
        plot(windens); plot(winindiv, add = T)
        
    #### extract density values associated with each individual based on area occupied during winter ####
        
        # density data (calculated in ar
        
        # determine where indivs of interest were located during winter
        indivlocswin <- read.csv("indivlocsfeb.csv")
        spdf.sp <- spTransform(SpatialPointsDataFrame(data.frame("x"=indivlocswin$Longitude,"y"=indivlocswin$Latitude), 
                                                    indivlocswin, proj4string = latlong), stateplane)
        indivhrs <- mcp(spdf.sp[,"AnimalID"], percent = 100)
        

        windens@data <- dplyr::select(windens@data, consDens)
        
        # sanity check
        plot(windens, col = "blue"); plot(indivhrs, add = T)
        
        # make density a raster so you can extract values from it
        dens <- raster(extent(windens))
        newres <- res(dens)/100 # need finer resolution than default
        dens <- raster(extent(windens), res = newres)
        rast <- rasterize(windens, dens, field = windens@data$consDens)
        plot(rast)

        
        # extract density values per indiv
        ext <- extract(rast, indivhrs, fun = unique)
        
        # remove NAs
        ext <- lapply(ext, function(x) x[!is.na(x)])
        
        # add animalid 
        names(ext) <- indivhrs@data$id
        
        # dataframe
        df <- data.frame(unlist(ext))
        df$AnimalID <- row.names(df)
        df <- rename(df, Dens = unlist.ext.)
        
        # add herd and year info
        popnyrs <- read.csv("popns-yrs.csv")
        densdat <- popnhrs@data %>%
          left_join(popnyrs, by = c("id" = "Herd"))
        
       # herd per indiv
                  
        # fix estimates for elk in shared blacktail/sagecreek area
            # because slightly diff estimates in diff years
            # and you didn't want to deal with overlapping polygons
        # and for elk in shared cfkN/silver run area (BRUC 15064 and 15099)
            # because they had (a negligible amount of) locs in cfkS also
            # so above code couldn't correctly estimate their density
          indivdat <- indivlocswin %>%
            dplyr::select(AnimalID, Herd) %>%
            distinct() %>%
            left_join(popnyrs, by = "Herd") %>%
            dplyr::select(-nIndiv) %>%
            left_join(df, by = "AnimalID") %>%
            mutate(Dens = ifelse((Herd == "Blacktail" | Herd == "Sage Creek") & Year == 2012,
              6003/1915, Dens)) %>% # 6003 = combined count in 2012. 1915 = area (km2)
            mutate(Dens = ifelse(AnimalID == "BRUC15064" | AnimalID == 'BRUC15099', 1.59649, Dens)) 
          write.csv(indivdat, "dens-indiv-feb.csv", row.names = F)
          
         herddat <- data.frame(popnhrs@data)
         write.csv(herddat, "dens-group-feb.csv", row.names = F)
            
      
 
        
        
