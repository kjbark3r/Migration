### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#  CLIPPING LARGE COVARIATE RASTERS TO AREA OF INTEREST   #
#                   KRISTIN BARKER                        #
#                    OCTOBER 2017                         #
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###




### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###



#### Packages ####

    library(raster) # ...rasters...
    library(sp) # spatial
#    library(rgeos) # buffer
#    library(adehabitatHR) # home ranges and kernel centroids
    library(rgdal) # projections; working with shps
#    library(gsubfn) # no idea, possibly unnecessary
    library(maptools) # writeSpatialShape
#    library(ggmap) # in case you want to plot in R
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

#### Projections ####
    
    latlong <- CRS("+init=epsg:4326")
    stateplane <- CRS("+init=epsg:2818")


#### "Raw" data  ####

    
    # populations and years of interest (from dataprep-elkdb.R)
    popnyr <- read.csv("popns-yrs.csv")

    
    # full area of interest (from homeranges.R and post-hoc ArcMap buffering)
    aoiraw <- readOGR("../GIS/Shapefiles/Elk", layer = 'AreaOfInterest')
    
    
    # growing season home range for each population (from homeranges.R)
    popnhrs <- readOGR("../GIS/Shapefiles/Elk/PopnHRs", layer = 'PpnGrowingSsnHRs')
    
    
    # NDVI amplitude (from https://phenology.cr.usgs.gov/get_data_250w.php)
    files.amp <- list.files(
        path = "./NDVIamp/",
        pattern = "tif$",
        full.names = TRUE)
    ampstk <- stack(files.amp)
    names(ampstk)
    ampstk@crs
    
    # time-integrated NDVI (from https://phenology.cr.usgs.gov/get_data_250w.php)
    files.ti <- list.files(
      path = "./NDVIti/",
      pattern = "tif$",
      full.names = TRUE)
    tistk <- stack(files.ti)
    names(tistk)
    tistk@crs

    

#### Tweaked data  ####
    
    
    # population codes and year indices
    popdat <- popnyr %>%
      select(-nIndiv) %>%
      mutate(yrIndex = Year-2000, # yrindex 1 is 2001
             Pop = ifelse(Herd == "Blacktail", "bla",
                   ifelse(Herd == "Border", "bor",
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
    
    # verify home ranges are in same order as popdat
    popdat$Herd
    popnhrs@data$id

    
    
### ### ### ### ### ### ### ### ##
####  |REMOTELY SENSED DATA|  ####
### ### ### ### ### ### ### ### ##

    
    # crop all ndvi data to area of interest (to speed processing)
    aoi <- spTransform(aoiraw, crs(ampstk)) # match aoi proj to ndvi
    ampcrop <- crop(ampstk, aoi) # crop (note: makes stacks to bricks
    ticrop <- crop(tistk, aoi)   ##       which speeds processing)
    ampcropsp <- projectRaster(ampcrop, crs = crs(popnhrs)) # make ndvi
    ticropsp <- projectRaster(ticrop, crs = crs(popnhrs)) ## stateplane


    # remove unnecessary items from workspace to save some memory
    rm(ampcrop, ticrop, aoiraw, files.amp, files.ti)
    
    
    # set output paths
    ampout <- "./NDVIamp/processed/"
    tiout <- "./NDVIti/processed/"    
    
    

    for(i in 1:nrow(popdat)) {
      
      # for each population
      pop <- popdat$Pop[i]
      
      # identify growing season home range polygon
      hr <- popnhrs[popnhrs$id[i],]
      
      # identify year of interest
      yr <- popdat$yrIndex[i]
      
      # identify all 5 years prior to year of interest
      yrs <- c(yr-5, yr-4, yr-3, yr-2, yr-1, yr)
      
      # identify NDVIamp and tiNDVI layers for each of those years
      ampsub <- subset(ampcropsp, yrs)
      tisub <- subset(ticropsp, yrs)
      
      # create empty rasterstacks to store results in
      ampstk <- stack()
      tistk <- stack()
      
      
      for (j in 1:length(yrs)) {
       
        # for each relevant year
        ampuncut <- ampsub[[j]]
        tiuncut <- tisub[[j]]
        
        # identify ndvi cells that are within or touching edge of home range
        cells <- cellFromPolygon(ampuncut, hr, weights = TRUE)[[1]][, "cell"]
        
        # set all other cells to NA
        ampuncut[][-cells] <- NA
        tiuncut[][-cells] <- NA
        
        # remove NA cells
        amp <- trim(ampuncut)
        ti <- trim(tiuncut)
        
        # combine results in stack
        ampstk <- stack(ampstk, amp)
        tistk <- stack(tistk, ti)

      }
      
      
      # store growing season HR, NDVIamp, and tiNDVI relevant to each popn
      
      writeOGR(hr, 
               dsn = "../GIS/Shapefiles/Elk/PopnHRs",
               layer = paste0(pop, "-HRgrow", yr+2000),
               driver = "ESRI Shapefile", overwrite = TRUE)
      
      writeRaster(ampstk,
                  filename = paste0(ampout, pop, "-", names(ampstk)),
                  bylayer = TRUE, format = "GTiff", overwrite = TRUE)
      
      writeRaster(tistk,
                  filename = paste0(tiout, pop, "-", names(tistk)),
                  bylayer = TRUE, format = "GTiff", overwrite = TRUE)
    
  }
   
 