### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# QUANTIFYING MIGRATORY BEHAVIOR ALONG A CONTINUUM USING  #
#  A MODIFIED RANGE SHIFT INDEX (Gurarie et al. 2017)     #
#                   KRISTIN BARKER                        #
#                      FEB 2018                           #
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###




### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###



  #### Packages ####
  
    library(sp) # spatial
    library(rgeos) # centroids
    library(adehabitatHR) # home ranges and kernel centroids
    #library(rgdal) # latlong/stateplane conversions
    library(raster) # shapefile()
    #library(gsubfn) # no idea, possibly unnecessary
    #library(maptools) # writeSpatialShape
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
    
    
      
  #### "Raw" data (processed in dataprep-elkdb.R and migbehav-migrateR.R) ####
  
  
    # Populations & years of interest 
    popnyrs <- read.csv("popns-yrs.csv") %>%
      within(Year <- as.numeric(Year))
    
    
    # All female locations
    allcowlocs <- read.csv("locs-allcows.csv") %>%
      mutate(Sex = factor(Sex))
    allindivs <-  data.frame(AnimalID = unique(allcowlocs$AnimalID))
    
    
    # Females to use in actual model
    modindivs <- read.csv("modindivs.csv")
    locs <- allcowlocs %>%
      semi_join(modindivs, by = "AnimalID")
    
    
    # winter HRs (from homeranges.R)
    indivhrswin <- shapefile("../GIS/Shapefiles/Elk/IndivHRs/AllWinHRs") 
    hrs.win <- spTransform(indivhrswin, stateplane) # need flat projection
    hrs.win@data$id = as.factor(hrs.win@data$id)

    
    # indivs of interest for reals
    indivlist <- data.frame(AnimalID = indivhrswin@data$id)
    
    # locs only for indivs in popns and yrs of interest 
    ioi <- locs %>% 
      semi_join(popnyrs, by = c("Herd", "Year")) %>%
      semi_join(indivlist, by = "AnimalID")
    ioi <- droplevels(ioi)
    
    # winter home range area and mig classn 
    moddat <- read.csv("moddat.csv") 
    
   
    
    
### ### ### ### ### ### ##
####  |RANGING AREA|  ####
### ### ### ### ### ### ##
    
    


    
    
    
    
### ### ### ### ### ### ### ###
####  |DIST BT CENTROIDS|  ####
### ### ### ### ### ### ### ###
    
    
    #### estimate summer home ranges ####
    
      ## prep dataframe
      popnlocssum <- allcowlocs %>%
        # only keep indivs from popns and years interest
        semi_join(indivlist, by = "AnimalID") %>%
        # and only locns collected during summer (jun-aug)
        filter(Month >= 06 & Month <= 08) 
    
      # remove stored factor data
      popnlocssum <- droplevels(popnlocssum)
      
      # estimate home ranges
      xy.sum <- data.frame("x"=popnlocssum$Longitude, "y"=popnlocssum$Latitude)
      spdf.sum <- SpatialPointsDataFrame(xy.sum, popnlocssum, proj4string = latlong)
      spdat.sum <- spTransform(spdf.sum, stateplane)
      kud.sum <- kernelUD(spdat.sum[,"AnimalID"], h = "href")
      hrs.sum <- getverticeshr(kud.sum, percent = 95)

      
    
    #### calculate distance between summer and winter centroids ####    
      
      # extract centroids of summer ranges
      cnts.sum <- gCentroid(hrs.sum, byid = TRUE)
      cnts.sum.df <- data.frame(cnts.sum@coords) %>%
        tibble::rownames_to_column() %>%
        rename(AnimalID = rowname, sumX = x, sumY = y) 
        
      # extract centroids of winter ranges      
      cnts.win <- gCentroid(hrs.win, byid = TRUE) 
      cnts.win.df <- data.frame(cnts.win@coords) %>%
        mutate(AnimalID = hrs.win@data$id) %>%
        rename(winX = x, winY = y)
      
      # calculate dist between centroids
      centroids <- left_join(cnts.sum.df, cnts.win.df, by = "AnimalID") %>%
        group_by(AnimalID) %>%
        summarise(DistX = abs(sumX-winX),
                  DistY = abs(sumY-winY)) %>%
        ungroup() %>%
        mutate(Dist = sqrt(DistX^2+DistY^2)) %>%
        dplyr::select(AnimalID, Dist)      
      
      
    
      
    
### ### ### ### ### ### ### ###
####  |RANGE SHIFT INDEX|  ####
### ### ### ### ### ### ### ###      
    
      
      rsi <- centroids %>%
        left_join(areas.yr, by = "AnimalID") %>%
        mutate(RSI = Dist / (2* (sqrt(area/pi)))) %>%
        arrange(RSI) %>%
        mutate(testRank = row_number()) %>%
        left_join(moddat, by = "AnimalID")
      summary(rsi)
      write.csv(rsi, file = "rsi.csv", row.names = F)
      
      
    
### ### ### ### ### ### 
####  |TEST MODS|  ####
### ### ### ### ### ###         
     
      
      # view relationships
      
       dat.cor <- rsi %>%
        dplyr::select(RSI, predFor, deltaFor, Dens, Old, densOwn, irrig, Behav)
      source("pairs-panels.R")
      pairs.panels(dat.cor)
      

      library(lme4)
      library(AICcmodavg)

      
      #### define a priori models ####
      m1 <- glmer(RSI ~ predFor + (1|Herd), family = Gamma(link = "inverse"), dat = rsi)
      m2 <- lmer(RSI ~ predFor + deltaFor + (1|Herd), dat = rsi, REML = FALSE)
      m3 <- lmer(RSI ~ predFor + deltaFor + deltaFor:predFor + (1|Herd), dat = rsi, REML = FALSE)
      m4 <- lmer(RSI ~ predFor + deltaFor + Dens + (1|Herd), dat = rsi, REML = FALSE)
      m5 <- lmer(RSI ~ predFor + deltaFor + Dens + deltaFor:Dens + (1|Herd), dat = rsi, REML = FALSE)
      m6 <- lmer(RSI ~ predFor + deltaFor + Old + (1|Herd), dat = rsi, REML = FALSE)
      m7 <- lmer(RSI ~ predFor + deltaFor + Old + deltaFor:Old + (1|Herd), dat = rsi, REML = FALSE)
      m8 <- lmer(RSI ~ predFor + deltaFor + Old + Dens + (1|Herd), dat = rsi, REML = FALSE)
      m9 <- lmer(RSI ~ predFor + deltaFor + Old + Dens + Old:Dens + (1|Herd), dat = rsi, REML = FALSE)
      m10 <- lmer(RSI ~ densOwn + (1|Herd), dat = rsi, REML = FALSE)
      m11 <- lmer(RSI ~ predFor + deltaFor + densOwn + (1|Herd), dat = rsi, REML = FALSE)
      m12 <- lmer(RSI ~ predFor + deltaFor + densOwn + deltaFor:predFor + (1|Herd), dat = rsi, REML = FALSE)
      m13 <- lmer(RSI ~ predFor + deltaFor + densOwn + deltaFor:densOwn + (1|Herd), dat = rsi, REML = FALSE)
      m14 <- lmer(RSI ~ densOwn + Old + densOwn:Old + (1|Herd), dat = rsi, REML = FALSE)
      m15 <- lmer(RSI ~ predFor + deltaFor + densOwn + Old + densOwn:Old + (1|Herd), dat = rsi, REML = FALSE)
      m16 <- lmer(RSI ~ irrig + (1|Herd), dat = rsi, REML = FALSE)
      m17 <- lmer(RSI ~ densOwn + irrig + (1|Herd), dat = rsi, REML = FALSE)
      m18 <- lmer(RSI ~ predFor + deltaFor + irrig   + (1|Herd), dat = rsi, REML = FALSE)
      m19 <- lmer(RSI ~ predFor + deltaFor + irrig + deltaFor:irrig + (1|Herd), dat = rsi, REML = FALSE)  
      m20 <- lmer(RSI ~ predFor + deltaFor + irrig + predFor:irrig + (1|Herd), dat = rsi, REML = FALSE)
      m21 <- lmer(RSI ~ irrig + Dens + irrig:Dens + (1|Herd), dat = rsi, REML = FALSE)
      m22 <- lmer(RSI ~ predFor + irrig + Dens + irrig:Dens + (1|Herd), dat = rsi, REML = FALSE)  
      m23 <- lmer(RSI ~ predFor + irrig + Old + Dens + Old:Dens + (1|Herd), dat = rsi, REML = FALSE)  
      m24 <- lmer(RSI ~ densOwn + irrig + Dens + irrig:Dens + (1|Herd), dat = rsi, REML = FALSE)
      m25 <- lmer(RSI ~ densOwn + irrig + Dens + densOwn:Dens + (1|Herd), dat = rsi, REML = FALSE) 
      m26 <- lmer(RSI ~ predFor + irrig + densOwn + irrig:densOwn + (1|Herd), dat = rsi, REML = FALSE) 
      
      # compete with AICc #
      mods <- list()
      modnms <- paste0("m", rep(1:26))
      for (i in 1:length(modnms)) { mods[[i]] <- get(modnms[[i]]) }
      aictab(cand.set = mods, modnames = modnms)
      aictab <- data.frame(aictab(cand.set = mods, modnames = modnms))
      
      # look at contenders
      summary(m6)
      summary(m7)
      summary(m8)
      
      library(car)
      Anova(m6) # if you want pvals and such
      
      rsi <- read.csv("rsi.csv")
      gamma <- fitdistr(rsi$RSI, "gamma")
      qqp(rsi$RSI, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])
      qqp(rsi$RSI, "lnorm")

      plot(RSI ~ testRank, dat = rsi, col = rsi$Behav)
      
      install.packages("drc")
      library(drc)
      
      mm <- drm(RSI ~ predFor, data = rsi, fct = MM.2())
      any(is.na(rsi$RSI))
      any(is.na(rsi$predFor))
      str(rsi)
      

      
      
      
      
   