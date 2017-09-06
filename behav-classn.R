### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#    CLASSIFYING MIGRATORY BEHAVIOR OF INDIVIDUALS        #
# BASED ON VOLUME INTERSECTION OF SEASONAL USE AREAS      #
#                   KRISTIN BARKER                        #
#                   SEPTEMBER 2017                        #
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###




#### packages ####

  library(RODBC) #possibly unnecessary; for connecting to access
  library(sp) #for kernel centroid estimate
  library(adehabitatHR) #for kernel centroid estimate
  library(rgdal) #for latlong/stateplane conversions
  library(gsubfn)
  library(maptools) #for writeSpatialShape
  library(dplyr) #for joins




#### working directories and database connection ####

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
  # if(file.exists(wd_worklaptop)) {
  #   channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
  #                                dbq=C:/Users/kristin/Documents/DatabasesEtc/Statewide/MtElkDatabase.accdb")
  # } else {  cat("Maybe you shouldn't have been so lazy when you made this code") }
  rm(wd_workcomp, wd_laptop, wd_worklaptop)


  
#### define projections ####
  
  latlong <- CRS("+init=epsg:4326")
  stateplane <- CRS("+init=epsg:2818")

  


  
#### calculate HR area and volume intersection for each individual #### 
  
  
  
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
####                 WORK IN PROGRESS                  ####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

## for each indivyr
  ## make a spdf of relevant locs
  ## calc 2 UDSs, one per Season [kernelUD}]
  ## calc area of each UD **specify unin and unout [getverticeshr]
  ## convert above data to as.data.frame (to store areas - may need to rename cols)
  ## calc 95% VI
  ## calc 50% VI [kerneloverlapHR]
  ## store: indiv, year, km2Win, km2Sum, VI95, VI50
  
  
    