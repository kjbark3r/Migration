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
    

    
    #### Population data  ####.
    
    
      # populations of interest (from dataprep-elkdb.R)
      popnsyrs <- read.csv("popns-yrs.csv")
      
      # add population code that matches start of tif filenames
      popdat <- popnsyrs %>%
        mutate(Pop = ifelse(Herd == "Blacktail", "bla",
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

      
      
      # identify population codes to loop through
      popcodes <- popdat$Pop
      npops <- length(popcodes)
      
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
            path = "./NDVIamp/processed",
            pattern = paste0("^", noquote(pop)),
            full.names = TRUE)
        ampstk <- stack(files.amp)
        names(ampstk)
        
        # read in 6 yrs time-integrated ndvi data (from rasterclip.R)
        files.ti <- list.files(
            path = "./NDVIti/processed",
            pattern = paste0("^", noquote(pop)),
            full.names = TRUE)
        tistk <- stack(files.ti)
        names(tistk)
        
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

      
