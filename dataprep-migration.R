### ### ### ### ### ### ### ### ### ### ### ### ### ###
#  PREPPING DATA FOR INCLUSION IN BEHAVIOR MODELS     #
#                   KRISTIN BARKER                    #
#                      FEB 2018                       #
### ### ### ### ### ### ### ### ### ### ### ### ### ###




### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###



  #### Packages ####
  
    library(foreign) # unimportant?
    library(ggplot2) # pretty plots
    library(gridExtra) # >1 plot per display
    library(reshape2) # jitter etc for data viz
    library(ordinal) # ordinal response w random effect
    library(AICcmodavg) # model comparison
    library(MuMIn) # model averaging
    library(dplyr) # data manipulation
    
    
  
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
    

    

### ### ### ###  ### ### #
####   | DATA PREP |  ####
### ### ### ###  ### ### #
    
    
  #### "Raw" data  ####
    
    deltafor <- read.csv("deltafor.csv") # from forage.R
    predfor <- read.csv("predfor.csv") # from forage.R
    dens <- read.csv("dens-indiv.csv") # from consdens.R
    age <- read.csv("indiv-dat.csv") # from dataprep-elkdb.R
    behav <- read.csv("mig-behav.csv") # from migbehav-migrateR.R
    behavpop <- read.csv("behav-per-popn.csv") # from migbehav-migrateR.R
    popndens <- read.csv("dens-group.csv") # from consdens.R
    hum <- read.csv("human-covariates.csv") # from landuse.R
 
       
  #### Model data  ####
    
    
    # data per indiv
    dat <- age %>%
      # start with age, this incls all captured elk
      dplyr::select(AnimalID, Old) %>%
      # add behavior and only retain indivs incld in that analysis
      inner_join(behav, by = "AnimalID") %>%
      dplyr::select(AnimalID, Behav, Old) %>%
      # add conspecific density and forage covariates
      left_join(dens, by = "AnimalID") %>%
      left_join(predfor, by = "Herd") %>%
      # only keep columns of interest
      dplyr::select(AnimalID, Behav, Old, Dens, Herd, StDevAmp, StDevTi) %>%
      # make predFor indicate how predictably forage varies (not how unpredictably)
      rename(PredForAmp = StDevAmp, PredForTi = StDevTi) %>%
      mutate(PredForAmp = -1*PredForAmp, PredForTi = -1*PredForTi) %>%
      left_join(deltafor, by = "AnimalID") %>%
      # remove extraneous columns
      dplyr::select(-c(maxNDVIin, maxNDVIout, Herd)) %>%
      # make column name more intuitive
      rename(deltaFor = deltaNDVI) %>%
      # add human-related covariates and remove extraneous columns
      left_join(hum, by = "AnimalID") %>%
      # rm 2 indivs with locs too late to estimate winHR
      filter(!is.na(Herd)) %>%
      # rm stored info from dropped indivs
      droplevels()

    

    # data per popn, incl age
    popold <- dat %>% 
      dplyr::select(Herd, Old) %>% 
      filter(!is.na(Herd), !is.na(Old)) %>%
      group_by(Herd) %>% 
      summarise(ppnOld = sum(Old)/n())
    popdat <- dat %>%
      dplyr::select(Herd, Dens, PredForTi) %>%
      filter(!is.na(Herd)) %>%
      left_join(behavpop, by = "Herd") %>%
      left_join(popold, by = "Herd") %>%
      distinct() %>%
      # rm "clarks fk" density data from indivs that used other areas
      filter((Herd == "Clarks Fork" & Dens > 2) | Herd != "Clarks Fork") %>%
      arrange(Herd)

     

    
### ### ### ### ##
#### |MODELS| ####
### ### ### ### ##
       

       
    #### Choose remotely-sensed metric to use for forage variability ####
    
       
      ## format behavior as ordinal 
      dat$behavO <- factor(dat$Behav, levels = c("resident", "other", "migrant"), ordered = TRUE)
       
      ## compare model using NDVI amplitude vs. time-integrated NDVI
      mods.for <- list()
      mods.for.nms <- c("PredAmp", "PredTi")
      mods.for[[1]] <- clmm(behavO ~ PredForAmp + deltaFor + (1|Herd), data = dat)
      mods.for[[2]] <- clmm(behavO ~ PredForTi + deltaFor + (1|Herd), data = dat)
      aictab(cand.set = mods.for, modnames = mods.for.nms)
      # within 1 deltaAIC; have no evidence to support one over other
      # predAmp slightly better (AICcWt = 0.61) so just rolling with that one
      
      
      # check whether relationship changes when deltaFor included in model
      mods.for <- list()
      mods.for.nms <- c("PredAmp", "PredTi")
      mods.for[[1]] <- clmm(behavO ~ PredForAmp + deltaFor + (1|Herd), data = dat)
      mods.for[[2]] <- clmm(behavO ~ PredForTi + deltaFor + (1|Herd), data = dat)
      aictab(cand.set = mods.for, modnames = mods.for.nms)
      # nope
      
      
      dat$predFor = dat$PredForAmp

      
         
      
      
### ### ### ### ### ##     
#### |STORE DATA| ####      
### ### ### ### ### ##
      
      
      # all indiv data (including "NA" age)
      write.csv(dat, "moddat.csv", row.names=F)
      
      # herd-level summaries
      write.csv(popdat, "pop-summaries.csv", row.names=F)
      