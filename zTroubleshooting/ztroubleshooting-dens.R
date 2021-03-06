### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
###           conspecific density
###   based on animl's geographic location
###   rather than population she's assigned to
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


#### i think this was early attempt? ####

        cent <- gCentroid(winindiv)
        test <- over(cent, windens)
        
        windensrast <- rasterize(windens, raster(extent(windens)))
        plot(windensrast)
          
        

        #### try again ####
        pu <- shapefile("../GIS/Shapefiles/Elk/IndivHRs/IndivWinMCPs")
        pu <- spTransform(pu, crs(windens))
        test <- extract(pu, windensrast, small = TRUE)
      
        
        hrs <- getverticeshr(kud) #create hr polygons from the kdes
cnts <- gCentroid(hrs, byid = TRUE) #extract centroids
      

### ### ### ### ### ### ### ### ### ### ##


 #### Projections ####
    
        latlong <- CRS("+init=epsg:4326")
        stateplane <- CRS("+init=epsg:2818")
    

   #### Winter HR polygons (HRs created in homeranges.R; popnHRs combined and processed in ArcMap) ####
        
        windens <- shapefile("../GIS/Shapefiles/Elk/PopnHRs/WinHRcombos")
        winindiv <- shapefile("../GIS/Shapefiles/Elk/IndivHRs/IndivWinMCPs")
        
        windens <- spTransform(windens, stateplane)
        winindiv <- spTransform(winindiv, stateplane)
        plot(windens, col = "blue"); plot(winindiv, add = T)
        
        windens2 <- windens
        windens2@data <- dplyr::select(windens2@data, Density)
        
        # it seems i can't extract data from polygons
        # so i guess i have to make my density data into a raster
        # current issue is that the grid size is too big
        # so make it finer, duh
        
        # blank raster to store data in
        blank <- raster(extent(windens))
        rast <- rasterize(windens2, blank)
        plot(rast)
        res(blank)
        newres <- res(blank)/100
        blank <- raster(extent(windens), res = newres)
        rast <- rasterize(windens2, blank)
        plot(rast)
        # damn i'm good
        plot(winindiv, add = T)
        
        
        
        
        
       ### ### ###  ### ### ### ### ### ### ### ### ### 
        ################### ok so... for real now #####
        
        
        
        #### Projections ####
    
        latlong <- CRS("+init=epsg:4326")
        stateplane <- CRS("+init=epsg:2818")
        
        # determine where indivs of interest were located during winter
        indivlocswin <- read.csv("indivlocswin.csv")
        spdf.sp <- spTransform(SpatialPointsDataFrame(data.frame("x"=indivlocswin$Longitude,"y"=indivlocswin$Latitude), 
                                                    indivlocswin, proj4string = latlong), stateplane)
        indivhrs <- mcp(spdf.sp[,"AnimalID"], percent = 100)
        
        # determine winter density
        popnhrs <- shapefile("../GIS/Shapefiles/Elk/PopnHRs/WinHRcombos")
        popnhrs <- spTransform(popnhrs, stateplane)
        windens <- popnhrs
        windens@data <- dplyr::select(windens@data, Density)
        
        # sanity check
        plot(windens, col = "blue"); plot(indivhrs, add = T)
        
        # make density a raster so you can extract values from it
        dens <- raster(extent(windens))
        newres <- res(dens)/100 # need finer resolution than default
        dens <- raster(extent(windens), res = newres)
        rast <- rasterize(windens, dens, field = windens@data$Density)
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
          indivdat <- indivlocswin %>%
            dplyr::select(AnimalID, Herd) %>%
            distinct() %>%
            left_join(popnyrs, by = "Herd") %>%
            dplyr::select(-nIndiv) %>%
            left_join(df, by = "AnimalID") %>%
            mutate(Dens = ifelse((Herd == "Blacktail" | Herd == "Sage Creek") & Year == 2012,
              6003/1915, Dens)) # 6003 = combined count in 2012. 1915 = area (km2)
          write.csv(indivdat, "dens-indiv.csv", row.names = F)
          
          
  #### home range extent issue ####
          
          library(adehabitatHR)
          library(raster)
          setwd( "C:\\Users\\kristin\\Documents\\Migration")
          detections <- read.csv("indivlocswin.csv")
              latlong <- CRS("+init=epsg:4326")
    stateplane <- CRS("+init=epsg:2818")
    detections <- spTransform(SpatialPointsDataFrame(data.frame("x"=detections$Longitude,
                                                             "y"=detections$Latitude),
                            detections, proj4string = latlong), stateplane)
    
          kud=kernelUD(detections[,1],h="href", grid=40, kern=c("bivnorm")) 
          t1 <- getverticeshr(kud, percent = 95)
          wholearea <- shapefile("..\\GIS\\Shapefiles\\Elk\\PopnHRs\\WinHRcombos")
          extent(wholearea)
          test <- kernelUD(detections[,"AnimalID"], h = "href", extent = extent(wholearea))
          # newp
          
          xmin(wholearea)
          
          x <- seq(xmin(wholearea),xmax(wholearea),by=30)  # where resolution is the pixel size you desire
y <- seq(ymin(wholearea),ymax(wholearea),by=30)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
# memory error
install.packages("snowfall")
library(snowfall)
sfParallel()
sfInit(parallel=TRUE, cpus=4)
memory.limit(size = NA)
memory.limit(size = 7500000)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE
class(xy)

kud=kernelUD(detections[,1],h="href", grid=xy, kern=c("bivnorm")) 
t1 <- getverticeshr(kud, percent = 95)        





  #### starting over with the home range thing ####

# ran just the top of homeranges.R     
modindivs <- read.csv("modindivs.csv")
indivlocswin <- read.csv("indivlocswin.csv")

# may be best to estimate separately for each herd?

# test run with just one, ill do silver run bc its small

latlong <- CRS("+init=epsg:4326")
stateplane <- CRS("+init=epsg:2818")


sil <- filter(indivlocswin, Herd == "Silver Run")
length(unique(sil$AnimalID))


sil <- sil %>%
  group_by(AnimalID) %>%
  filter(n() > 5) %>%
  ungroup()
sil <- droplevels(sil)

spdf <- spTransform(SpatialPointsDataFrame(data.frame("x"=sil$Longitude, "y"=sil$Latitude),
                        sil, proj4string = latlong), stateplane)

kud <- kernelUD(spdf[,"AnimalID"], h="href") 
t1 <- getverticeshr(kud, percent = 95)
plot(t1)

# that worked. i'ma try looping thru all herds and creating these separately

# first see how to combine spatialpolygonsdfs

hd <- filter(indivlocswin, Herd == "HD314")
length(unique(hd$AnimalID))


hd <- hd %>%
  group_by(AnimalID) %>%
  filter(n() > 5) %>%
  ungroup()
hd <- droplevels(hd)

spdf2 <- spTransform(SpatialPointsDataFrame(data.frame("x"=hd$Longitude, "y"=hd$Latitude),
                        hd, proj4string = latlong), stateplane)

kud2 <- kernelUD(spdf2[,"AnimalID"], h="href") 
t2 <- getverticeshr(kud2, percent = 95)
plot(t2)

test <- rbind(t1, t2)
plot(test)
# sweeet

# but how to do it starting with a blank one?
t3 <- SpatialPolygons(list())
t4 <- data.frame()
t5 <- SpatialPolygonsDataFrame(t3, t4)
test3 <- rbind(t5, test2)

# eh screw it, we'll do a raster stack instead

library(raster)
test <- raster(t1)
test1 <- raster()
test2 <- stack(test1, test)
plot(test)

# fiiiine, shps


herds <- unique(popnyrs$Herd) 
herdstouse <- herds[-13] # sapphire throws error; will do separately


for (i in 1:length(herdstouse)) {
 herd <- herdstouse[i]
 dat <- filter(indivlocswin, Herd == herd) %>%
   group_by(AnimalID) %>%
   filter(n() > 5) %>%
   ungroup()
 dat <- droplevels(dat)
 xy <- data.frame("x"=dat$Longitude, "y"=dat$Latitude)
 spdf <- SpatialPointsDataFrame(xy, dat, proj4string = latlong)
 spdat <- spTransform(spdf, stateplane)
 kud <- kernelUD(spdat[,"AnimalID"], h = "href")
 kdes <- getverticeshr(kud, percent = 95)
 writeOGR(kdes, 
     dsn = "../GIS/Shapefiles/Elk/IndivHRs", 
     layer = paste("win", herd, sep = "-"), 
     driver = "ESRI Shapefile",
     overwrite = TRUE) 
}



#### quantifying effects of potential inaccuracies in population estimates due to sightability diffs ####

#### Packages ###

library(sp) # spatial, over
library(rgeos) # buffer, centroid
library(rgdal) # latlong/stateplane conversions; readOGR
library(maptools) # writeSpatialShape
library(raster) # intersect
library(dplyr) # joins, data work, general awesomeness


#### Working directory ###

wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\Migration"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\Migration"
wd_worklaptop <- "C:\\Users\\kristin\\Documents\\Migration"
wd_UCB <- "C:\\Users\\Kristin\\Box Sync\\Documents\\PreUCB\\Migration"
if (file.exists(wd_workcomp)) {setwd(wd_workcomp)
} else {
  if(file.exists(wd_laptop)) {setwd(wd_laptop)
  } else {
    if(file.exists(wd_UCB)) {setwd(wd_UCB)
    } else {setwd(wd_worklaptop)
    }
  }
}

rm(wd_workcomp, wd_laptop, wd_worklaptop)

#### Projections ###

latlong <- CRS("+init=epsg:4326")
stateplane <- CRS("+init=epsg:2818")



#### Population year info ###

popnyrs <- read.csv("popns-yrs.csv")


#### Winter HR polygons (HRs created in homeranges.R; popnHRs combined and processed in ArcMap) ###

windens <- readOGR("E:\\UMT_2018\\Documents\\GIS\\Shapefiles\\Elk\\PopnHRs", layer ='WinHRcombosFeb')

sight <- windens@data %>%
  arrange(popnEst) %>%
  mutate(rank = row_number(),
         pct80 = popnEst * 1.8,
         pct60 = popnEst * 1.6)

## ok... what am i doing here... i'm curious whether any of the population estimates of one herd
## fall within the range of popnEst - pct60 of any other herd

for(i in 1:nrow(sight)) {
  
  herd = sight[i, "id"]
  est = sight[1, "popnEst"]
  
  print(any(est >= sight$popnEst & est <= sight$pct60 & herd != sight$id))
  #print(any(est >= sight$popnEst & est <= sight$pct80 & herd != sight$id))  
  
  
}


# oh actually makes more sense to look at how it would affect density estimates specifically, duh

windens <- readOGR("E:\\UMT_2018\\Documents\\GIS\\Shapefiles\\Elk\\PopnHRs", layer ='WinHRcombosFeb')

# calculate densities if sightability had been 60% or 80%
dens <- windens@data %>%
  arrange(consDens) %>%
  mutate(id = as.character(id),
         rank = row_number(),
         pct80 = popnEst * (5/4),
         pct60 = popnEst * (5/3),
         dens80 = pct80/area,
         dens60 = pct60/area,
         diff80 = dens80 - consDens,
         diff60 = dens60 - consDens) %>%
  dplyr::select(id, rank, consDens, dens80, dens60, diff80, diff60)


# create herd name abbreviations
abbvs <- c("TobRt", "ElkH", "Sap", "SilRn", "Pio", "Blk", "Grly", "WFk", "EFk", "NMad", "Dom", "MlCk", "CkFk", "Mad")


# for each herd,
for(i in 1:nrow(dens)) {
  
  # start with original data
  tmpdat <- dens  
  
  # replace the pct60 and pct80 estimates with the actual estimate for that herd
  tmpdat[i, "dens80"] = tmpdat[i, "consDens"]
  tmpdat[i, "dens60"] = tmpdat[i, "consDens"] 
  

  # add new column of updated ranks under these sightability changes
  dens[paste0("r80", abbvs[i])] <- rank(tmpdat$dens80)  
  dens[paste0("r60", abbvs[i])] <- rank(tmpdat$dens60)    

}

write.csv(dens, file = "densitySightability.csv")

# duh, you're replacing them iteratively, need to return tot he original dataframe each time




## useless

dat60 <- matrix(ncol = nrow(sight)+1)
colnames(dat60) <- c("orig", "TobRt", "ElkH", "Sap", "SilRn", "Pio", "Blk", "Grly", "WFk", "EFk", "NMad", "Dom", "MlCk", "CkFk", "Mad")
dat60$orig <- sight$rank
dat60 <- data.frame(dat60)

newranks80 <- rank(tmpdat$dens80)
newranks60 <- rank(tmpdat$dens60)

# 3. create vector of updated ranks
# 2. determine how the ranks would change if compared a correct herd estimate to underestimated herds
assign(paste0("r80", abbvs[i]), rank(tmpdat$dens80))
assign(paste0("r60", abbvs[i]), rank(tmpdat$dens60))  

mutate(tmpdat, assign(paste0("r80", abbvs[i])), rank(tmpdat$dens80))





#### sightability take 2, now with canopy cover ####


  library(raster) # ...rasters...
  library(sp) # spatial
  library(rgdal) # projections; working with shps
  library(maptools) # writeSpatialShape
  library(beepr) # exciting alarm when code finishes
  library(dplyr) # joins, data work, general awesomeness
  
  setwd("C:\\Users\\Kristin\\Box Sync\\Documents\\PreUCB\\Migration")
  
  
  ### Projections ###
  
  latlong <- CRS("+init=epsg:4326")
  stateplane <- CRS("+init=epsg:2818")
  
  
  
  ### Population year info ###
  
  popnyrs <- read.csv("popns-yrs.csv")
  
  
  ### Winter HR polygons ###
  
  windens <- readOGR("E:\\UMT_2018\\Documents\\GIS\\Shapefiles\\Elk\\PopnHRs", layer ='WinHRcombosFeb')
  windens <- spTransform(windens, stateplane)

  ### cover from my hero owen ###
  
  covRaw <- raster("E:\\Tree Cover\\perc_tree_2011.tif")
  covProj <- projectRaster(covRaw, crs = crs(windens))
  covClip <- raster::intersect(covProj, windens)
  plot(covClip)
  plot(windens, add = T)
  writeRaster(covClip, filename = "treeCover.tif", format = "GTiff")
  covExt <- extract(covClip, windens)

  
  covMean <- lapply(covExt, mean)
  par(mfrow = c(2, 7))
  lapply(covExt, hist)
  par(mfrow = c(1,1))
  plot(covClip); plot(windens, color = "id", add = T)

  
  covExtLog <- lapply(covExt, log)
  lapply(covExtLog, hist)
  
  ## normal enough; now test for diffs between them ##
  ## if diffs, determine for which heds
  ## and see whether that would matter given density diffs you calcd earlier in xls
  
  
  ## actually may need to make sure no >100 cov vals first, duh
  lapply(covExt, max)
  # oops, yep, how to handle...
  # use arcmap to see which values
  # mk they're all just water, force to 0
  
  waterFix <- function(vals) {
  
    vals <- ifelse(vals > 100, 0, vals)
    
  }
  
  test <- c(0, 5, 1000)
  test <- waterFix(test)
  # oh holy fuck, it actually worked
  
  covExtFixed <- lapply(covExt, waterFix)
  covLog <- lapply(covExtFixed, log)
  # oops need to add small value
  covLog <- lapply(covExtFixed, function(x) x = log(x+0.01))
  
  # fwiw i finally figured out this isn't listed by the factor level
  # it's just in order of unique herd id
  names(covLog) <- unique(windens@data$id)
  # booyah
  
  test <- unlist(covLog)

  
  
  
  
  
  
  ## nixable, maybe ####
  
  par(mfrow = c(2, 7))
  lapply(covLog, hist)
  par(mfrow = c(1,1))
  # noice.
  
  
  # save win shp jic since you cleverly left your hd in the lab
  writeOGR(windens, dsn = ".", layer = "windens", 
           driver = "ESRI Shapefile", overwrite_layer = TRUE)
  
  # hm how do i know which element of the list maps to which polygon?
  # ah it must be the factor levels in windens
  
  ext <- extract(covClip, windens, df = TRUE)

  herdIDs <- data.frame(Herd = unique(windens@data$id), 
                        ID = as.numeric(windens@data$id))
  
  dat <- ext %>%
    mutate(cov = waterFix(perc_tree_2011),
           covLog = log(cov)) %>%
    left_join(herdIDs, by = "ID")
  
  datClean <- dat[!is.na(dat),]
  hm <- dat[is.na(dat),]

  # i think the herds are mapped incorrectly
  # because clarks fork has lots of rows but their HR is super small
  # so if you get significant results from the anova
  # you need to fix that so you can determine where the diffs are
  # but real quick in case they're not diff and it doesn't matter
  
  am <- aov(cov ~ Herd, data = datClean)
  anova(am)
  
  
  
  #### starting over cleaner (post-explosion) ####
  
    library(raster) # ...rasters...
    library(sp) # spatial
    library(rgdal) # projections; working with shps
    library(maptools) # writeSpatialShape
    library(beepr) # exciting alarm when code finishes
    library(dplyr) # joins, data work, general awesomeness
    
    setwd("C:\\Users\\Kristin\\Box Sync\\Documents\\PreUCB\\Migration")
  
  
  ### Projections ###
  
    latlong <- CRS("+init=epsg:4326")
    stateplane <- CRS("+init=epsg:2818")
  
  
  ### Population year info ###
  
   popnyrs <- read.csv("popns-yrs.csv")
  
  
  ### Winter HR polygons ###
  
    windens <- readOGR("E:\\UMT_2018\\Documents\\GIS\\Shapefiles\\Elk\\PopnHRs", layer ='WinHRcombosFeb')
    windens <- spTransform(windens, stateplane)
    
  ### cover from my hero owen ###
  
    covRaw <- raster("E:\\Tree Cover\\perc_tree_2011.tif")
    covProj <- projectRaster(covRaw, crs = crs(windens))
    covClip <- raster::intersect(covProj, windens)
    
  ### extract cover per herd ###
  
   covExt <- extract(covClip, windens)
  
  ### make values >100 == 0 (these are water) ###
  
    waterFix <- function(vals) vals <- ifelse(vals > 100, 0, vals)
    covExtFixed <- lapply(covExt, waterFix) # update values
    names(covExtFixed) <- unique(windens@data$id) # map herds to values
    covDat <- utils::stack(covExtFixed) # make it a dataframe
    
  
  ### calculate summary stats ###
    
    cover <- covDat %>%
      rename(Herd = ind) %>%
      group_by(Herd) %>%
      summarise(
        meanCV = mean(values),
        medCov = median(values),
        iqrCov = IQR(values),
        ppnCov50 = round(length(which(values > 50))/n(), 4)
      )
    length(which(cover$ppnCov50 > 0.01))
    write.csv(cover, "sightability-cover.csv", row.names = F)
  
  ## CALCULATE: ##
  
  # 1. median and IQR of cover for each herd winter range
  # 2. percent of each herd winter range with >50% cover
  
  # 
  
  ppn <- function(x) length(which(x>50))/length(x)
  
  


  
  covSmry <- data.frame(
    
  )

      
  
  
         
  ## cut
      
      # add small value to 0s and take log to make data appx normal
      
      covLog <- lapply(covExtFixed, function(x) x = log(x+0.01))
      
      # fwiw i finally figured out this isn't listed by the factor level
      # it's just in order of unique herd id
      names(covLog) <- unique(windens@data$id)
      
      ## save point for when you explode it again ###
      save.image("sightPrelim.RData")
      
      ### make covLog into a dataframe with names as row
      
      dat <- utils::stack(covLog) # YAY!!
      colnames(dat) <- c("covLog", "Herd")
      
      
      
      ## let's not get carried away here. First just look at median cover per herd.
  
  
      ### test for diffs between herds
      anova(aov(covLog ~ Herd, data = dat)) # y
      
      
      ### identify which herds are diff from the others
      
      herds <- unique(dat$Herd)
      
      # here's where you'd initialize a df to store results in, i'm sure they're all significant
      
      for(i in 1:length(unique(dat$Herd))-1) {
        h1 <- as.character(herds[i])
        h2 <- as.character(herds[i+1])
        
        tdat <- filter(dat, Herd == h1 | Herd == h2)
        
        t.test(covLog ~ Herd, dat = tdat)$p.value
        
      }
      
      ## uh
      
      
      sumCov <- lapply(covExtFixed, median)
      lapply(covExtFixed, mean)
      lapply(covExtFixed, max)
      
      test <- c(0, 10, 57)
      
      
      ppn50 <- data.frame(lapply(covExtFixed, ppn))
      lapply(ppn50, max)
      
      