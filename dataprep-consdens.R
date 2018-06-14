### ### ### ### ### ### ### ### ### ### ### ### ### ##
#     PROCESSING COUNT DATA FROM MONTANA BIOLOGISTS  #
#    TO ASSESS CONSPECIFIC DENSITY DURING WINTER     #
#                   KRISTIN BARKER                   #
#                    NOVEMBER 2017                   #
### ### ### ### ### ### ### ### ### ### ### ### ### ##




### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###



    #### Packages ####
    
        library(sp) # spatial
        library(rgeos) # buffer
        library(adehabitatHR) # home ranges and kernel centroids
        library(rgdal) # latlong/stateplane conversions
        library(gsubfn) # no idea, possibly unnecessary
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
      
      

### ### ### ### ### ### ### ### ### ### ###
####  |COMBINING DEAN'S POINT COUNTS|  ####
### ### ### ### ### ### ### ### ### ### ###
 

    ### ### ### ### #
    ####  DATA   ####
    ### ### ### ### #
    
            
    
        #### All count data (to update with Gravellys) ####
            
           ests <- read.csv("../DatabasesEtc/Statewide/Elk/mt-elk-popn-ests-toupdate.csv")
        
        
        #### Count data from Gravellys and Tobacco Roots ####
        
            grav <- read.csv("../DatabasesEtc/Statewide/Elk/PopnEsts-RawFromBios/gravelly-rawdata.csv") %>%
              filter(Species == "elk") %>%
              rename(Group = Winter.Herd) %>%
              select(Year, Lat, Long, Group, Total)
            tobac <- read.csv("../DatabasesEtc/Statewide/Elk/PopnEsts-RawFromBios/tobaccoroots-rawdata.csv") %>%
              filter(Species == "elk") %>%
              select(Year, Lat, Long, Total)
            
        
        #### Population winter home ranges ####
        
            hrs.all <- readOGR("../GIS/Shapefiles/Elk/PopnHRs", layer ='PpnWinBuffKDEs')
            unique(hrs.all$id) # correct; removed hd314 because can't estimate indiv winHR (late capture)
            
            mad.hr <- hrs.all[hrs.all$id == "Madison",]
            mad.hr@data$id <- factor(mad.hr@data$id)
            
            bla.hr <- hrs.all[hrs.all$id == "Blacktail",]
            bla.hr@data$id <- factor(bla.hr@data$id)
            
            sge.hr <- hrs.all[hrs.all$id == "Sage Creek",]
            sge.hr@data$id <- factor(sge.hr@data$id)
            
            tob.hr <- hrs.all[hrs.all$id == "Tobacco Roots",]
            tob.hr@data$id <- factor(tob.hr@data$id)
            
        
            
        #### All count data ####
            grav.06 <- subset(grav, Year == 2006)
            grav.sp <- spTransform(SpatialPointsDataFrame(data.frame("x"=grav.06$Long,"y"=grav.06$Lat),
                                               grav.06, proj4string = latlong), mad.hr@proj4string)   

            
            

            
    ### ### ### ### #
    ####  WORK   ####
    ### ### ### ### #
            
      
            
      #### Madison ####
    
        mad.yr <- popnyrs[popnyrs$Herd == "Madison","Year"]
        mad.dat <- subset(grav, Year == mad.yr)
        mad.yrlocs <- spTransform(SpatialPointsDataFrame(data.frame("x"=mad.dat$Long,"y"=mad.dat$Lat),
                                           mad.dat, proj4string = latlong), mad.hr@proj4string)
        plot(mad.hr, main = "Madison 2006"); plot(mad.yrlocs, add=T)
        mad.locs <- mad.yrlocs[complete.cases(over(mad.yrlocs, mad.hr)), ] 
        mad.locs@data$Total <- as.numeric(mad.locs@data$Total)
        plot(mad.locs, add = T, col = "red")
        plot(grav.sp, add = T, col = "blue")
        # MADISON - Me, Dean #
        sum(mad.locs@data$Total); ests[ests$Herd == "Madison", 3]
        # sum from plotted points does not match dean's estimate
        # but does match closely his estimate of west madison (exclusive of east madison)
        
        
        # export shp of all recorded counts
        writeOGR(mad.yrlocs, dsn = "../GIS/Shapefiles/Elk", layer = "countLocs2006", 
                 driver = "ESRI Shapefile", overwrite_layer = TRUE)
    
    #### Blacktail ####
    
        bla.yr <- popnyrs[popnyrs$Herd == "Blacktail","Year"]
        bla.dat <- subset(grav, Year == bla.yr)
        bla.yrlocs <- spTransform(SpatialPointsDataFrame(data.frame("x"=bla.dat$Long,"y"=bla.dat$Lat),
                                                         bla.dat, proj4string = latlong), bla.hr@proj4string)
        plot(bla.hr, main = "Blacktail 2011"); plot(bla.yrlocs, add=T)
        # expect dean's estimate to be higher than this count (same reason as madison)
        bla.locs <- bla.yrlocs[complete.cases(over(bla.yrlocs, bla.hr)), ] 
        bla.locs@data$Total <- as.numeric(bla.locs@data$Total)
        plot(bla.locs, add = T, col = "red")
        # BLACKTAIL  - Me, Dean #
        sum(bla.locs@data$Total) ; ests[ests$Herd == "Blacktail", 3] 
        
        
        # export shp of all recorded counts
        writeOGR(bla.yrlocs, dsn = "../GIS/Shapefiles/Elk", layer = "countLocs2011", 
                 driver = "ESRI Shapefile", overwrite_layer = TRUE)
        
    
    #### Tobacco Roots ####
    
        tob.yr <- popnyrs[popnyrs$Herd == "Tobacco Roots","Year"]
        tob.dat <- subset(tobac, Year == tob.yr)
        tob.yrlocs <- spTransform(SpatialPointsDataFrame(data.frame("x"=tob.dat$Long,"y"=tob.dat$Lat),
                                                         tob.dat, proj4string = latlong), tob.hr@proj4string)
        plot(tob.hr, main = "Tobacco Roots 2014"); plot(tob.yrlocs, add=T)
        tob.locs <- tob.yrlocs[complete.cases(over(tob.yrlocs, tob.hr)), ] 
        tob.locs@data$Total <- as.numeric(tob.locs@data$Total)
        plot(tob.locs, add = T, col = "red")
        # TOB ROOTS  - Me, Dean #
        sum(tob.locs@data$Total); ests[ests$Herd == "Tobacco Roots", 3]
        
        
        # export shp of all recorded counts
        writeOGR(tob.yrlocs, dsn = "../GIS/Shapefiles/Elk", layer = "countLocs2014", 
                 driver = "ESRI Shapefile", overwrite_layer = TRUE)
        
    
    
    
    #### Sage Creek ####
    
        # no count available; average previous and subsequent year (yr of interest = 2012)
        sag.yr1 <- 2011
        sag.dat1 <- subset(grav, Year == sag.yr1)
        sag.yrlocs1 <- spTransform(SpatialPointsDataFrame(data.frame("x"=sag.dat1$Long,"y"=sag.dat1$Lat),
                                           sag.dat1, proj4string = latlong), mad.hr@proj4string)
        
        sag.yr2 <- 2013
        sag.dat2 <- subset(grav, Year == sag.yr2)
        sag.yrlocs2 <- spTransform(SpatialPointsDataFrame(data.frame("x"=sag.dat2$Long,"y"=sag.dat2$Lat),
                                           sag.dat2, proj4string = latlong), mad.hr@proj4string)
        
        # export shp of all recorded counts
        writeOGR(sag.yrlocs1, dsn = "../GIS/Shapefiles/Elk", layer = "countLocs2011", 
                 driver = "ESRI Shapefile", overwrite_layer = TRUE)
        writeOGR(sag.yrlocs2, dsn = "../GIS/Shapefiles/Elk", layer = "countLocs2013", 
                 driver = "ESRI Shapefile", overwrite_layer = TRUE)
        
        
        
        
        
### ### ### ### ### ###  ### ### ### ### ### ### ### ### ###
####  |ASSESSING OVERLAP TO DETERMINE COMBINED GROUPS|  ####
### ### ### ### ### ###  ### ### ### ### ### ### ### ### ### 
        

        
        
    ### ### ### ### #
    ####  DATA   ####
    ### ### ### ### #
        
      
      #### All winter locations from herds of interest    
      
          
          # per herd (incls indivs with too few locs to estimate indiv HR)
          popnlocswin <- read.csv("popnlocs-win.csv")
        
          
          # per indiv (only indivs with enough locs to estimate indiv HR)
          indivlocswin <- read.csv("indivlocswin.csv")
          
          # store unique individuals and individuals+herd
          indivlist <- data.frame(AnimalID = unique(indivlocswin$AnimalID))
          indivherd <- indivlocswin %>%
            select(AnimalID, Herd) %>%
            distinct()
          

        
        
    ### ### ### ### #
    ####  WORK   ####
    ### ### ### ### #    
        
          
      #### Conservative winter range of each herd ####
          
          ## get xy points; write to dataframe, to spatial data frame, to stateplane
          spdf <- SpatialPointsDataFrame(data.frame("x"=popnlocswin$Longitude,
                          "y"=popnlocswin$Latitude), popnlocswin, proj4string = latlong)
          spdf.sp <- spTransform(spdf, stateplane)
      
           
          ## estimate kde for each population
          popnwinuds <- kernelUD(spdf.sp[,"Herd"], h = "href") 
          popnwinkdes <- getverticeshr(popnwinuds, percent = 95)
          plot(popnwinkdes)
                
                
                
      #### Conservative winter range of each individual ####
          
          
          ## get xy points; write to dataframe, to spatial data frame, to stateplane
          spdf.i <- SpatialPointsDataFrame(data.frame("x"=indivlocswin$Longitude,
                          "y"=indivlocswin$Latitude), indivlocswin, proj4string = latlong)
          spdf.sp.i <- spTransform(spdf.i, stateplane)
      
           
          ## estimate kde for each individual
          indivwinuds <- kernelUD(spdf.sp.i[,"AnimalID"], h = "href")
          indivwinkdes <- getverticeshr(indivwinuds, percent = 95)
          plot(indivwinkdes, add = T, col = "blue")
                
          
          # add Herd data back in
          indivwinkdes@data <- indivwinkdes@data %>%
            rename(AnimalID = id) %>%
            left_join(indivherd, by = "AnimalID")
          head(indivwinkdes@data)
          
          

      #### Assessing space sharing (for combining/splitting group counts ####   
          
          
          sharedat <- data.frame(Herd = as.character(), 
                                 n = as.integer(), 
                                 nInt = as.integer(), 
                                 ppnInt = as.numeric(), 
                                 intWith = as.character())
          options(stringsAsFactors = FALSE)
          
          
          ## AMONG POPULATIONS ##
          
          
              ## Sage Creek indivs in Blacktail
          
                  # subset sage creek individuals
                  sagi <- indivwinkdes[indivwinkdes@data$Herd == "Sage Creek",]
                  sagi@data$AnimalID <- factor(sagi@data$AnimalID)
                  sagilist <- factor(unique(sagi@data$AnimalID))
              
                  # determine which indivs used blacktail winter range
                  int.sag <- intersect(sagi, popnwinkdes[popnwinkdes@data$id == "Blacktail",])
                  
                  
                  # store pathetically
                  sharedat[1,1] <- "Sage Creek"
                  sharedat[1,2] <- nrow(sagi@data)
                  sharedat[1,3] <- nrow(int.sag@data)
                  sharedat[1,4] <- nrow(int.sag@data)/nrow(sagi@data)
                  sharedat[1,5] <- "Blacktail"
              
              
              ## Blacktail indivs in Sage Creek
          
                  # subset blacktail individuals
                  blai <- indivwinkdes[indivwinkdes@data$Herd == "Blacktail",]
                  blai@data$AnimalID <- factor(blai@data$AnimalID)
                  blailist <- factor(unique(blai@data$AnimalID))
              
                  # determine which indivs used blacktail winter range
                  int.bla <- intersect(blai, popnwinkdes[popnwinkdes@data$id == "Sage Creek",])
                  
                  ## visualize
                  plot(popnwinkdes[popnwinkdes@data$id == "Sage Creek",])
                  plot(int.sag, add = T, col = "blue")
                  plot(sagi, add = T, col = "red")
                  plot(blai, add = T, col = "green")
                  
                  
                  # store pathetically
                  sharedat[2,1] <- "Blacktail"
                  sharedat[2,2] <- nrow(blai@data)
                  sharedat[2,3] <- nrow(int.bla@data)
                  sharedat[2,4] <- nrow(int.bla@data)/nrow(blai@data)
                  sharedat[2,5] <- "Sage Creek"
                  
                  
                  
              ## Clarks Fork indivs in Silver Run
          
                  # subset clarks fork individuals
                  cfki <- indivwinkdes[indivwinkdes@data$Herd == "Clarks Fork",]
                  cfki@data$AnimalID <- factor(cfki@data$AnimalID)
                  cfkilist <- factor(unique(cfki@data$AnimalID))
              
                  # determine which indivs used silver run winter range
                  int.cfk <- intersect(cfki, popnwinkdes[popnwinkdes@data$id == "Silver Run",])

                  # store pathetically
                  sharedat[3,1] <- "Clarks Fork"
                  sharedat[3,2] <- nrow(cfki@data)
                  sharedat[3,3] <- nrow(int.cfk@data)
                  sharedat[3,4] <- nrow(int.cfk@data)/nrow(cfki@data)
                  sharedat[3,5] <- "Silver Run"                  
          
                  
                  
              ## Silver Run indivs in Clarks Fork
          
                  # subset silver run individuals
                  sili <- indivwinkdes[indivwinkdes@data$Herd == "Silver Run",]
                  sili@data$AnimalID <- factor(sili@data$AnimalID)
                  sililist <- factor(unique(sili@data$AnimalID))
              
                  # determine which indivs used clarks fork winter range
                  int.sil <- intersect(sili, popnwinkdes[popnwinkdes@data$id == "Clarks Fork",])

                  # store pathetically
                  sharedat[4,1] <- "Silver Run"
                  sharedat[4,2] <- nrow(sili@data)
                  sharedat[4,3] <- nrow(int.sil@data)
                  sharedat[4,4] <- nrow(int.sil@data)/nrow(sili@data)
                  sharedat[4,5] <- "Clarks Fork"        
                  
                  
              ## Dome indivs in HD314
                  
                  # use subsequent year's winter HR for HD314 (late capture; no locs recorded during first winter)
                  allcowlocs <- read.csv("locs-allcows.csv") %>%
                    mutate(Sex = factor(Sex))
                  hdlocswin <- allcowlocs %>%
                    filter(Herd == "HD314") %>%
                    mutate(Year = as.numeric(substr(Date, 0, 4)),
                           Month = as.numeric(substr(Date, 6, 7))) %>%
                    # only locns collected during winter
                    filter(Month == 12 | Month == 1 | Month == 2) %>%
                    # map december locs to following year's winter
                    mutate(Year = ifelse(Month == 12, Year + 1, Year)) %>%
                    # remove stored data about filtered out herds and indivs
                    mutate(Herd = factor(Herd), AnimalID = factor(AnimalID))
                  
                  
                  ## get xy points; write to dataframe, to spatial data frame, to stateplane
                  spdf <- SpatialPointsDataFrame(data.frame("x"=hdlocswin$Longitude,
                                  "y"=hdlocswin$Latitude), hdlocswin, proj4string = latlong)
                  spdf.sp <- spTransform(spdf, stateplane)
              
                   
                  ## estimate kde for each population
                  hdwinuds <- kernelUD(spdf.sp[,"Herd"], h = "href")
                                           #, extent = 2) # need extent > default of 1 
                  hdwinkdes <- getverticeshr(hdwinuds, percent = 95)
                  plot(hdwinkdes)
                  
          
                  # subset dome individuals
                  domi <- indivwinkdes[indivwinkdes@data$Herd == "Dome",]
                  domi@data$AnimalID <- factor(domi@data$AnimalID)

                  # determine which indivs used clarks fork winter range
                  int.dom <- intersect(domi, hdwinkdes)
                  domilist <- factor(unique(int.dom$AnimalID))

                  # store pathetically
                  sharedat[5,1] <- "Dome"
                  sharedat[5,2] <- nrow(domi@data)
                  sharedat[5,3] <- nrow(int.dom@data)
                  sharedat[5,4] <- nrow(int.dom@data)/nrow(domi@data)
                  sharedat[5,5] <- "HD314"  
                  
                  # visualize
                  plot(domi, main = "Dome")
                  for (i in 1:length(domilist)) {
                    elk = as.character(domilist[i])
                    plot(domi[domi@data$AnimalID == elk,], add = T, border = "green", lwd=3)
                    
                  }
                        
          ## WITHIN POPULATIONS ##
        
        
                  # elkhorns 
                      
                      # visually determined using arcmap
                      sharedat[6,1] <- "Elkhorns123"
                      sharedat[6,2] <- NA
                      sharedat[6,3] <- 0
                      sharedat[6,4] <- 0
                      sharedat[6,5] <- "Elkhorns123"        
                  
                  # NMadison
                      nmde <- readOGR("../GIS/Shapefiles/Elk/PopnHRs", layer ='nmdE')
                      nmde <- spTransform(nmde, indivwinkdes@proj4string)
                      nmdw <- readOGR("../GIS/Shapefiles/Elk/PopnHRs", layer ='nmdW')
                      nmdw <- spTransform(nmdw, indivwinkdes@proj4string)
                      
                      # subset nmadison individuals
                      nmdi <- indivwinkdes[indivwinkdes@data$Herd == "NMadison",]
                      nmdi@data$AnimalID <- factor(nmdi@data$AnimalID)

                  
                      # determine which indivs used eastern and western ranges
                      int.nmde <- intersect(nmdi, nmde)
                      int.nmdw <- intersect(nmdi, nmdw)
                      
                      # determine which indivs used BOTH eastern and western ranges
                      int.nmd <- inner_join(int.nmde@data, int.nmdw@data, by = "AnimalID")
                      nmdilist <- factor(unique(int.nmd$AnimalID))


                      # store pathetically
                      sharedat[7,1] <- "NMadison"
                      sharedat[7,2] <- nrow(nmdi@data)
                      sharedat[7,3] <- nrow(int.nmd)
                      sharedat[7,4] <- nrow(int.nmd)/nrow(nmdi@data)
                      sharedat[7,5] <- "NMadison"    
                      
                      # visualize
                      plot(nmdi, main = "NMadison")
                      for (i in 1:length(nmdilist)) {
                        elk = as.character(nmdilist[i])
                        plot(nmdi[nmdi@data$AnimalID == elk,], add = T, border = "green", lwd=3)
                        
                      }
                      

                  
                  
                  # Pioneers
                      
                      pion <- readOGR("../GIS/Shapefiles/Elk/PopnHRs", layer ='pioN')
                      pion <- spTransform(pion, indivwinkdes@proj4string)
                      pios <- readOGR("../GIS/Shapefiles/Elk/PopnHRs", layer ='pioS')
                      pios <- spTransform(pios, indivwinkdes@proj4string)
                      
                      # subset pioneer individuals
                      pioi <- indivwinkdes[indivwinkdes@data$Herd == "Pioneers",]
                      pioi@data$AnimalID <- factor(pioi@data$AnimalID)

                      # determine which indivs used northern and southern ranges
                      int.pion <- intersect(pioi, pion)
                      int.pios <- intersect(pioi, pios)
                      
                      # determine which indivs used BOTH northern and southern ranges
                      int.pio <- inner_join(int.pion@data, int.pios@data, by = "AnimalID")
                      pioilist <- factor(unique(int.pio$AnimalID))

                      # store pathetically
                      sharedat[8,1] <- "Pioneers"
                      sharedat[8,2] <- nrow(pioi@data)
                      sharedat[8,3] <- nrow(int.pio)
                      sharedat[8,4] <- nrow(int.pio)/nrow(pioi@data)
                      sharedat[8,5] <- "Pioneers" 
                      
                      # visualize
                      plot(pioi, main = "Pioneers")
                      for (i in 1:length(pioilist)) {
                        elk = as.character(pioilist[i])
                        plot(pioi[pioi@data$AnimalID == elk,], add = T, border = "green", lwd=3)
                        
                      }


                  
                  
                  # Sapphires
                      
                                            
                      sapn <- readOGR("../GIS/Shapefiles/Elk/PopnHRs", layer ='sapN')
                      sapn <- spTransform(sapn, indivwinkdes@proj4string)
                      saps <- readOGR("../GIS/Shapefiles/Elk/PopnHRs", layer ='sapS')
                      saps <- spTransform(saps, indivwinkdes@proj4string)
                      
                      # subset sapphire individuals
                      sapi <- indivwinkdes[indivwinkdes@data$Herd == "Sapphire",]
                      sapi@data$AnimalID <- factor(sapi@data$AnimalID)

                      
                      # determine which indivs used northern and southern ranges
                      int.sapn <- intersect(sapi, sapn)
                      int.saps <- intersect(sapi, saps)
                      
                      # determine which indivs used BOTH northern and southern ranges
                      int.sap <- inner_join(int.sapn@data, int.saps@data, by = "AnimalID")
                      sapilist <- factor(unique(int.sap$AnimalID))

                      # store pathetically
                      sharedat[9,1] <- "Sapphires"
                      sharedat[9,2] <- nrow(sapi@data)
                      sharedat[9,3] <- nrow(int.sap)
                      sharedat[9,4] <- nrow(int.sap)/nrow(sapi@data)
                      sharedat[9,5] <- "Sapphires" 
                      
                      # visualize
                      plot(sapi, main = "Sapphires")
                      for (i in 1:length(sapilist)) {
                        elk = as.character(sapilist[i])
                        plot(sapi[sapi@data$AnimalID == elk,], add = T, border = "green", lwd=3)
                        
                      }
                      
                      write.csv(sharedat, "overlap-data.csv", row.names=F)
                      