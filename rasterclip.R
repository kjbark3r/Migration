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
    library(rgeos) # buffer
    library(adehabitatHR) # home ranges and kernel centroids
    library(rgdal) # projections; working with shps
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


#### Projections ####
    
    latlong <- CRS("+init=epsg:4326")
    stateplane <- CRS("+init=epsg:2818")


#### "Raw" data  ####

    
    # populations and years of interest (from dataprep-elkdb.R)
    popnyr <- read.csv("popns-yrs.csv")

    
    # list years of interest
    yrs <- list(unique(popnyr$Year))
    
    # full area of interest (from homeranges.R and post-hoc ArcMap buffering)
    aoiraw <- readOGR("../GIS/Shapefiles/Elk", layer = 'AreaOfInterest')
    
    
    # growing season home range for each population (from homeranges.R)
    popnhrs <- readOGR("../GIS/Shapefiles/Elk", layer = 'PpnGrowingSsnHRs')
    
    
    # NDVI amplitude
    files.amp <- list.files(
        path = "./NDVIamp/",
        pattern = "tif$",
        full.names = TRUE)
    ampstk <- stack(files.amp)
    names(ampstk)
    ampstk@crs
    
    # time-integrated NDVI
    files.ti <- list.files(
      path = "./NDVIti/",
      pattern = "tif$",
      full.names = TRUE)
    tistk <- stack(files.ti)
    names(tistk)
    tistk@crs


### ### ### ### ### ### ### ### ##
####  |REMOTELY SENSED DATA|  ####
### ### ### ### ### ### ### ### ##

    
    # clip to area of interest 
    aoi <- spTransform(aoiraw, crs(ampstk)) # match projections
    ampcrop <- crop(ampstk, aoi) # crop (note: makes stacks to bricks
    ticrop <- crop(tistk, aoi)   #       which speeds processing)
    
    # create lists and info for loop(?) prep
    
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
    

    
    
    for(i in 1:nrow(popdat)) {
      
      # for each population
      pop <- popdat$Pop[i]
      
      # identify growing season home range polygon
      hr <- popnhrs@polygons[[i]]@Polygons[[1]]
      
      # identify year of interest
      yr <- popdat$yrIndex[i]
      
      # identify all 5 years prior to year of interest
      yrs <- c(yr-5, yr-4, yr-3, yr-2, yr-1, yr)
      
      # identify NDVIamp and tiNDVI layers for each of those years
      amp <- subset(ampcrop, yrs)
      ti <- subset(ticrop, yrs)
      
      # crop layers to population growing season range
      ampc <- crop(amp, hr)
      tic <- crop(ti, hr)
      # CURRENT ISSUE: YOUR SUBSETTED HR POLYGON IS JUST A POLYGON
      # NOT A SPATIAL POLYGON
      # SO THIS CROPPING STEP CAN'T WORK
      
      # store cropped layers as bricks
      #brick(x = , filename = paste0())
      
    }
    
    
    
    
    
#### old code ####



# # crop one raster to another
# kud99 <- getverticeshr(kud.all, percent = 99) # gives outline of home range
# lc.crop <- crop(lc, kud99) # lc is a landcover raster – needs to be same projection
# library(raster) # mask is in this library
# lc.crop <- mask(lc, kud99) # magically gets rid of NAs or something
# plot(lc.crop)


#### revisit nearest neighbor vs bilinear ####

# ndvi - NEED EVERY RASTER FROM EVERY TIME PD IN EVERY YR
