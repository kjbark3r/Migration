### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#  ASSESSING RELATIVE INFLUENCE OF FORAGE, HUMANS, AND    #
#    INTRINSIC FACTORS ON MIGRATORY BEHAVIOR OF ELK       #
#                   KRISTIN BARKER                        #
#                 DEC 2017 - FOREVER                      #
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###




### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###



  #### Packages ####
  
    library(foreign)
    library(nnet) # multinom
    library(ggplot2) # pretty plots
    library(gridExtra) # >1 plot per display
    library(reshape2)
    library(ordinal) # ordinal response
    library(AICcmodavg) # model comparison
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
    

    

### ### ### ###  ### ### ### ### ###
####  | DATA PREP - run once |  ####
### ### ### ###  ### ### ### ### ###
    
    
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
      filter(!is.na(Herd))
    dat <- droplevels(dat) # rm stored info from dropped indivs
    write.csv(dat, "moddat.csv", row.names=F)


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
    write.csv(popdat, "pop-summaries.csv", row.names=F)
    
    


### ### ### ### ### ### ### ##
#### |DATA SUMMARIES ETC| ####
### ### ### ### ### ### ### ##    
    

    ## read in data (if already run once) ##
    dat <- read.csv("moddat.csv")
    popdat <- read.csv("pop-summaries.csv")
    
    
    ## format behavior as ordinal ##
    dat$behavO <- factor(dat$Behav, levels = c("resident", "other", "migrant"), ordered = TRUE)
    
    
    #### General looks at indiv covariates ####

    
      # number of elk >10yrs old
      length(which(dat$Old == 1))
      
      # number of migrants
      length(which(dat$Behav == "migrant"))
      length(which(dat$Behav == "migrant"))/nrow(dat)
      
      # summary tables of binary covariates
      lapply(dat[, c("Behav", "Old", "irrig")], table)
      
      # three-way contingency table
      ftable(xtabs(~ Old + Behav + irrig, data = dat))
  
      
   #### General plots of univariate relationships ####
      
      # 
      ggplot(dat, aes(x = Behav, y = deltaFor)) +
        geom_boxplot(size = 0.75) +
        geom_jitter(alpha = 0.5) +
        facet_grid(Old ~ irrig, margins = TRUE) 

      
      
      
    #### Summarizing covariates ####  
      
      
      # deltafor
      
      
      # predfor
      
      
      # consdens
      
      
      # age
      
      
      #densown
      
      
      # ppnAg
      
      
 
    
    #### Testing for differences in covariates bt behavior types ####   


      
      # dens
        
        aov.d <- aov(Dens ~ Behav, data = dat)
        summary(aov.d)
        TukeyHSD(aov.d)
        


      # deltafor

        aov.df <- aov(deltaFor ~ Behav, data = dat)
        summary(aov.df)
        TukeyHSD(aov.df)  
              
        
      # predfor

        aov.pf <- aov(PredForTi ~ Behav, data = dat)
        summary(aov.pf)
        TukeyHSD(aov.pf)  
        
        
      # age

       agetab <- matrix(c(4,61,37,163,2,47), 2, 3, 
         dimnames = list(age = c("nOld", "nYoung"),
                         behav = c("resident", "migrant", "other")))
       fisher.test(agetab)
 
          

### ### ### ### ##
#### |MODELS| ####
### ### ### ### ##
       
       
       
    #### Choosing remotely-sensed metric to use for forage variability ####
       
      mods.for <- list()
      mods.for.nms <- c("PredAmp", "PredTi")
      mods.for[[1]] <- clmm(behavO ~ PredForAmp + (1|Herd), data = dat)
      mods.for[[2]] <- clmm(behavO ~ PredForTi + (1|Herd), data = dat)
      aictab(cand.set = mods.for, modnames = mods.for.nms)
      aicres <- data.frame(aictab(cand.set = mods.for, modnames = mods.for.nms))
      # within 1 deltaAIC; have no evidence to support one over other
      # predAmp slightly better (AICcWt = 0.61) so just rolling with that one
      
      dat$predFor = dat$PredForAmp
      
      
      

  
    #### Models ####
      
      
      # remove indivs for whom we have no age estimation #
      moddat <- filter(dat, !is.na(Old))
      
      
      # check for collinearity #
      dat.cor <- moddat %>%
        dplyr::select(Behav, predFor, deltaFor, Dens, Old, densOwn, ppnAg)
      source("pairs-panels.R")
      pairs.panels(dat.cor)
      # all < 0.60
      
      
      # define models #
      fmh <- clmm(behavO ~ predFor + deltaFor + (1|Herd), data = moddat)
      consdens <- clmm(behavO ~ predFor + deltaFor + Dens + deltaFor*Dens + (1|Herd), data = moddat)
      old <- clmm(behavO ~ predFor + deltaFor + Old + deltaFor*Old + (1|Herd), data = moddat)
      humshld <- clmm(behavO ~ densOwn + (1|Herd), data = moddat)
      agrsub <- clmm(behavO ~ ppnAg + (1|Herd), data = moddat)
      
      
      # look at CIs of coefficients to assess hypothesis support within topics #
      confint(fmh)
      confint(consdens)
      confint(old)
      confint(humshld)
      confint(agrsub)
      
      # look at model summaries #
      summary(fmh)
      summary(consdens)
      summary(old)
      summary(humshld)
      summary(agrsub)      
      
      
      # compete with AICc #
      mods <- list()
      modnms <- c("Forage", "Density", "Old", "HumanShld", "AgrSub")
      mods[[1]] <- fmh
      mods[[2]] <- consdens
      mods[[3]] <- old
      mods[[4]] <- humshld
      mods[[5]] <- agrsub
      aictab(cand.set = mods, modnames = modnms)
      aicres <- data.frame(aictab(cand.set = mods, modnames = modnms))
      
      
      # compete with BIC #
      bicres <- data.frame(BIC(fmh, consdens, old, humshld, agrsub))
      
      

    #### Assess proportional odds assumption ####