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
    # remove indivs for whom we have no age estimation #
    moddat <- filter(dat, !is.na(Old))
    
    
    ## format behavior as ordinal ##
    dat$behavO <- factor(dat$Behav, levels = c("resident", "other", "migrant"), ordered = TRUE)
    moddat$behavO <- factor(moddat$Behav, levels = c("resident", "other", "migrant"), ordered = TRUE)
    
    
    #### General looks at indiv covariates ####

    
      # number of elk >10yrs old
      length(which(moddat$Old == 1))
      
      # number of migrants
      length(which(moddat$Behav == "migrant"))
      length(which(moddat$Behav == "migrant"))/nrow(dat)
      
      # summary tables of binary covariates
      lapply(moddat[, c("Behav", "Old", "irrig")], table)
      
      # three-way contingency table
      ftable(xtabs(~ Old + Behav + irrig, data = moddat))
      
      
  
      
   #### General plots of univariate relationships ####
      
      # deltaFor, split by old & irrig
      ggplot(moddat, aes(x = Behav, y = deltaFor)) +
        geom_boxplot(size = 0.75) +
        geom_jitter(alpha = 0.5) +
        facet_grid(Old ~ irrig, margins = TRUE) 
      
      # predFor, split by old & irrig
      ggplot(moddat, aes(x = Behav, y = predFor)) +
        geom_boxplot(size = 0.75) +
        geom_jitter(alpha = 0.5) +
        facet_grid(Old ~ irrig, margins = TRUE) 
      
      #
      ggplot(popdat, aes(x = ppnOld, y = ppnMig, color = Herd)) +
        labs(x = "", y = "Proportion Migrant") +
        geom_smooth(color = "black") +
        geom_point()
        
      
      
    #### Summarizing covariates ####  
      
      
      # deltafor
      
      
      # predfor
      
      
      # consdens
      
      
      # age
      
      
      #densown
      
      
      # ppnAg
      
      
 
    
    #### Testing for differences in covariates bt behavior types ####   


      
      # dens
        
        aov.d <- aov(Dens ~ Behav, data = moddat)
        summary(aov.d)
        TukeyHSD(aov.d)
        


      # deltafor

        aov.df <- aov(deltaFor ~ Behav, data = moddat)
        summary(aov.df)
        TukeyHSD(aov.df)  
              
        
      # predfor

        aov.pf <- aov(PredForTi ~ Behav, data = moddat)
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
       
       
       
    #### Choose remotely-sensed metric to use for forage variability ####
       
      mods.for <- list()
      mods.for.nms <- c("PredAmp", "PredTi")
      mods.for[[1]] <- clmm(behavO ~ PredForAmp + (1|Herd), data = dat)
      mods.for[[2]] <- clmm(behavO ~ PredForTi + (1|Herd), data = dat)
      aictab(cand.set = mods.for, modnames = mods.for.nms)
      # within 1 deltaAIC; have no evidence to support one over other
      # predAmp slightly better (AICcWt = 0.61) so just rolling with that one
      
      dat$predFor = dat$PredForAmp
      moddat$predFor = moddat$PredForAmp
      
 
          
    #### check covariates for collinearity & assess univariate relationships ####
      
      dat.cor <- moddat %>%
        dplyr::select(behavO, predFor, deltaFor, betFor, Dens, Old, densOwn, ppnAg, irrig)
      source("pairs-panels.R")
      pairs.panels(dat.cor)
      # all < 0.60
      
      
      
    #### univariate models ####
      
      # df to store results of each model      
      dat.uni <- data.frame(cov = NA, coeff = NA, dirEffect = NA, spt95 = NA,
                            spt90 = NA, spt80 = NA, spt75 = NA)  
      
      
      # define each model
      pf <- clmm(behavO ~ predFor + (1|Herd), data = moddat)
      df <- clmm(behavO ~ deltaFor + (1|Herd), data = moddat)
      dn <- clmm(behavO ~ Dens + (1|Herd), data = moddat)
      ol <- clmm(behavO ~ Old + (1|Herd), data = moddat)
      ow <- clmm(behavO ~ densOwn + (1|Herd), data = moddat)
      ag <- clmm(behavO ~ ppnAg + (1|Herd), data = moddat)
      ay <- clmm(behavO ~ irrig + (1|Herd), data = moddat)
      
      #predFor 
      d.pf <- data.frame(coef(summary(pf))) %>%
        tibble::rownames_to_column() %>%
        rename(cov = rowname, coeff = Estimate, se = "Std..Error") %>%
        filter(!grepl("other", cov)) %>%
        mutate(CI95low = coeff - (1.96*se), CI95high = coeff + (1.96*se),
               CI90low = coeff - (1.645*se), CI90high = coeff + (1.645*se),
               CI80low = coeff - (1.28*se), CI80high = coeff + (1.28*se),
               CI75low = coeff - (1.15*se), CI75high = coeff + (1.15*se),
               CI70low = coeff - (1.04*se), CI70high = coeff + (1.04*se)) %>%
        mutate(spt95 = ifelse(CI95low < 0 & CI95high > 0, 0, 1),
               spt90 = ifelse(CI90low < 0 & CI90high > 0, 0, 1),
               spt80 = ifelse(CI80low < 0 & CI80high > 0, 0, 1),
               spt75 = ifelse(CI75low < 0 & CI75high > 0, 0, 1),
               spt70 = ifelse(CI70low < 0 & CI70high > 0, 0, 1)) %>%
        mutate(dirEffect = ifelse(coeff > 0, "pos", "neg")) %>%
        dplyr::select(cov, coeff, dirEffect, spt95, spt90, spt80, spt75)
      
      # deltaFor
      d.df <- data.frame(coef(summary(df))) %>%
        tibble::rownames_to_column() %>%
        rename(cov = rowname, coeff = Estimate, se = "Std..Error") %>%
        filter(!grepl("other", cov)) %>%
        mutate(CI95low = coeff - (1.96*se), CI95high = coeff + (1.96*se),
               CI90low = coeff - (1.645*se), CI90high = coeff + (1.645*se),
               CI80low = coeff - (1.28*se), CI80high = coeff + (1.28*se),
               CI75low = coeff - (1.15*se), CI75high = coeff + (1.15*se),
               CI70low = coeff - (1.04*se), CI70high = coeff + (1.04*se)) %>%
        mutate(spt95 = ifelse(CI95low < 0 & CI95high > 0, 0, 1),
               spt90 = ifelse(CI90low < 0 & CI90high > 0, 0, 1),
               spt80 = ifelse(CI80low < 0 & CI80high > 0, 0, 1),
               spt75 = ifelse(CI75low < 0 & CI75high > 0, 0, 1),
               spt70 = ifelse(CI70low < 0 & CI70high > 0, 0, 1)) %>%
        mutate(dirEffect = ifelse(coeff > 0, "pos", "neg")) %>%
        dplyr::select(cov, coeff, dirEffect, spt95, spt90, spt80, spt75)
       
      # conspecific density     
      d.dn <- data.frame(coef(summary(dn))) %>%
        tibble::rownames_to_column() %>%
        rename(cov = rowname, coeff = Estimate, se = "Std..Error") %>%
        filter(!grepl("other", cov)) %>%
        mutate(CI95low = coeff - (1.96*se), CI95high = coeff + (1.96*se),
               CI90low = coeff - (1.645*se), CI90high = coeff + (1.645*se),
               CI80low = coeff - (1.28*se), CI80high = coeff + (1.28*se),
               CI75low = coeff - (1.15*se), CI75high = coeff + (1.15*se),
               CI70low = coeff - (1.04*se), CI70high = coeff + (1.04*se)) %>%
        mutate(spt95 = ifelse(CI95low < 0 & CI95high > 0, 0, 1),
               spt90 = ifelse(CI90low < 0 & CI90high > 0, 0, 1),
               spt80 = ifelse(CI80low < 0 & CI80high > 0, 0, 1),
               spt75 = ifelse(CI75low < 0 & CI75high > 0, 0, 1),
               spt70 = ifelse(CI70low < 0 & CI70high > 0, 0, 1)) %>%
        mutate(dirEffect = ifelse(coeff > 0, "pos", "neg")) %>%
        dplyr::select(cov, coeff, dirEffect, spt95, spt90, spt80, spt75)
      
      # old
      d.ol <- data.frame(coef(summary(ol))) %>%
        tibble::rownames_to_column() %>%
        rename(cov = rowname, coeff = Estimate, se = "Std..Error") %>%
        filter(!grepl("other", cov)) %>%
        mutate(CI95low = coeff - (1.96*se), CI95high = coeff + (1.96*se),
               CI90low = coeff - (1.645*se), CI90high = coeff + (1.645*se),
               CI80low = coeff - (1.28*se), CI80high = coeff + (1.28*se),
               CI75low = coeff - (1.15*se), CI75high = coeff + (1.15*se),
               CI70low = coeff - (1.04*se), CI70high = coeff + (1.04*se)) %>%
        mutate(spt95 = ifelse(CI95low < 0 & CI95high > 0, 0, 1),
               spt90 = ifelse(CI90low < 0 & CI90high > 0, 0, 1),
               spt80 = ifelse(CI80low < 0 & CI80high > 0, 0, 1),
               spt75 = ifelse(CI75low < 0 & CI75high > 0, 0, 1),
               spt70 = ifelse(CI70low < 0 & CI70high > 0, 0, 1)) %>%
        mutate(dirEffect = ifelse(coeff > 0, "pos", "neg")) %>%
        dplyr::select(cov, coeff, dirEffect, spt95, spt90, spt80, spt75)
      
      # ownership density
      d.ow <- data.frame(coef(summary(ow))) %>%
        tibble::rownames_to_column() %>%
        rename(cov = rowname, coeff = Estimate, se = "Std..Error") %>%
        filter(!grepl("other", cov)) %>%
        mutate(CI95low = coeff - (1.96*se), CI95high = coeff + (1.96*se),
               CI90low = coeff - (1.645*se), CI90high = coeff + (1.645*se),
               CI80low = coeff - (1.28*se), CI80high = coeff + (1.28*se),
               CI75low = coeff - (1.15*se), CI75high = coeff + (1.15*se),
               CI70low = coeff - (1.04*se), CI70high = coeff + (1.04*se)) %>%
        mutate(spt95 = ifelse(CI95low < 0 & CI95high > 0, 0, 1),
               spt90 = ifelse(CI90low < 0 & CI90high > 0, 0, 1),
               spt80 = ifelse(CI80low < 0 & CI80high > 0, 0, 1),
               spt75 = ifelse(CI75low < 0 & CI75high > 0, 0, 1),
               spt70 = ifelse(CI70low < 0 & CI70high > 0, 0, 1)) %>%
        mutate(dirEffect = ifelse(coeff > 0, "pos", "neg")) %>%
        dplyr::select(cov, coeff, dirEffect, spt95, spt90, spt80, spt75)      
      
      # ppnAg
      d.ag <- data.frame(coef(summary(ag))) %>%
        tibble::rownames_to_column() %>%
        rename(cov = rowname, coeff = Estimate, se = "Std..Error") %>%
        filter(!grepl("other", cov)) %>%
        mutate(CI95low = coeff - (1.96*se), CI95high = coeff + (1.96*se),
               CI90low = coeff - (1.645*se), CI90high = coeff + (1.645*se),
               CI80low = coeff - (1.28*se), CI80high = coeff + (1.28*se),
               CI75low = coeff - (1.15*se), CI75high = coeff + (1.15*se),
               CI70low = coeff - (1.04*se), CI70high = coeff + (1.04*se)) %>%
        mutate(spt95 = ifelse(CI95low < 0 & CI95high > 0, 0, 1),
               spt90 = ifelse(CI90low < 0 & CI90high > 0, 0, 1),
               spt80 = ifelse(CI80low < 0 & CI80high > 0, 0, 1),
               spt75 = ifelse(CI75low < 0 & CI75high > 0, 0, 1),
               spt70 = ifelse(CI70low < 0 & CI70high > 0, 0, 1)) %>%
        mutate(dirEffect = ifelse(coeff > 0, "pos", "neg")) %>%
        dplyr::select(cov, coeff, dirEffect, spt95, spt90, spt80, spt75) 
      
      # irrig (y/n)
      d.ay <- data.frame(coef(summary(ay))) %>%
        tibble::rownames_to_column() %>%
        rename(cov = rowname, coeff = Estimate, se = "Std..Error") %>%
        filter(!grepl("other", cov)) %>%
        mutate(CI95low = coeff - (1.96*se), CI95high = coeff + (1.96*se),
               CI90low = coeff - (1.645*se), CI90high = coeff + (1.645*se),
               CI80low = coeff - (1.28*se), CI80high = coeff + (1.28*se),
               CI75low = coeff - (1.15*se), CI75high = coeff + (1.15*se),
               CI70low = coeff - (1.04*se), CI70high = coeff + (1.04*se)) %>%
        mutate(spt95 = ifelse(CI95low < 0 & CI95high > 0, 0, 1),
               spt90 = ifelse(CI90low < 0 & CI90high > 0, 0, 1),
               spt80 = ifelse(CI80low < 0 & CI80high > 0, 0, 1),
               spt75 = ifelse(CI75low < 0 & CI75high > 0, 0, 1),
               spt70 = ifelse(CI70low < 0 & CI70high > 0, 0, 1)) %>%
        mutate(dirEffect = ifelse(coeff > 0, "pos", "neg")) %>%
        dplyr::select(cov, coeff, dirEffect, spt95, spt90, spt80, spt75) 
      
      # data frame summarizing all results      
      univariate <- bind_rows(d.pf, d.df, d.dn, d.ol, d.ow, d.ag, d.ay)
      
      
      # forage with binary 0/1 for whether forage better off winter range
      test <- data.frame(coef(summary(for3))) %>%
        tibble::rownames_to_column() %>%
        rename(cov = rowname, coeff = Estimate, se = "Std..Error") %>%
        filter(!grepl("other", cov)) %>%
        mutate(CI95low = coeff - (1.96*se), CI95high = coeff + (1.96*se),
               CI90low = coeff - (1.645*se), CI90high = coeff + (1.645*se),
               CI80low = coeff - (1.28*se), CI80high = coeff + (1.28*se),
               CI75low = coeff - (1.15*se), CI75high = coeff + (1.15*se),
               CI70low = coeff - (1.04*se), CI70high = coeff + (1.04*se)) %>%
        mutate(spt95 = ifelse(CI95low < 0 & CI95high > 0, 0, 1),
               spt90 = ifelse(CI90low < 0 & CI90high > 0, 0, 1),
               spt80 = ifelse(CI80low < 0 & CI80high > 0, 0, 1),
               spt75 = ifelse(CI75low < 0 & CI75high > 0, 0, 1),
               spt70 = ifelse(CI70low < 0 & CI70high > 0, 0, 1)) %>%
        mutate(dirEffect = ifelse(coeff > 0, "pos", "neg")) %>%
        dplyr::select(cov, coeff, dirEffect, spt95, spt90, spt80, spt75) 
      

    #### standardized forage covariates (to assess relative influence of each) ####
      
      # need transformation before scaling?
      par(mfrow=c(3,1))
      hist(moddat$predFor)
      hist(moddat$deltaFor)
      hist(log(moddat$deltaFor)) # predFor is ok; deltaFor needs log transformation
      hist(log(moddat$deltaFor)); hist(log(moddat$deltaFor+0.01)); hist(log(moddat$deltaFor+0.05))
      length(which(moddat$deltaFor == 0))/nrow(moddat)
      
      # standardize each
      test <- moddat %>%
        mutate(predForStd = predFor-(mean(predFor)/sd(predFor)),
          deltaForLog = log(deltaFor + 1),
          deltaForStd = deltaForLog - (mean(deltaForLog)/sd(deltaForLog)))
      summary(test$deltaForStd); summary(test$predForStd)
      par(mfrow=c(2,1)); hist(test$predForStd); hist(test$deltaForStd)
      
      # cutting losses and moving on for now; can't get standardization of log to work right
      
      
    #### assessing possible deltaFor covariates (choose 1 as baseline for other models that use forage) #### 
      
      # create new binary covariate for whether forage is better outside winter range or not
      moddat$betFor = ifelse(moddat$deltaFor > 0, 1, 0)
      
      # define models
      for1 <- clmm(behavO ~ predFor + deltaFor + (1|Herd), data = moddat)
      for2 <- clmm(behavO ~ predFor + deltaFor + I(predFor*deltaFor) + (1|Herd), data = moddat)
      for3 <- clmm(behavO ~ predFor + betFor + (1|Herd), data = moddat)
      for4 <- clmm(behavO ~ predFor + betFor + I(predFor*betFor) + (1|Herd), data = moddat)

      # compete with AICc #
      mods <- list()
      modnms <- c("plusDelta", "intrxnDelta", "plusBet", "intrxnBet")
      mods[[1]] <- for1
      mods[[2]] <- for2
      mods[[3]] <- for3
      mods[[4]] <- for4
      aictab(cand.set = mods, modnames = modnms)
      # plus the binary covariate and interaction with the binary covariate are indistinguishable
      # (deltaAICc = 1.96 with slightly better support for plus), so i don't think intrxn necessary
 
      
      
      

      
      
      

