### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#  ASSESSING RELATIVE INFLUENCE OF FORAGE, HUMANS, AND    #
#    INTRINSIC FACTORS ON MIGRATORY BEHAVIOR OF ELK       #
#                   KRISTIN BARKER                        #
#                    DECEMBER 2017                         #
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
    

    
  #### "Raw" data  ####
    
    deltafor <- read.csv("deltafor.csv") # from forage.R
    predfor <- read.csv("predfor.csv") # from forage.R
    dens <- read.csv("dens-indiv.csv") # from consdens.R
    age <- read.csv("indiv-dat.csv") # from dataprep-elkdb.R
    behav <- read.csv("mig-behav.csv") # from migbehav-migrateR.R
    behavpop <- read.csv("behav-per-popn.csv") # from migbehav-migrateR.R
    popndens <- read.csv("dens-group.csv") # feom consdens.R
    
  #### Model data  ####
    
    
    dat <- age %>%
      dplyr::select(AnimalID, Old) %>%
      inner_join(behav, by = "AnimalID") %>%
      dplyr::select(AnimalID, Behav, Old) %>%
      left_join(dens, by = "AnimalID") %>%
      left_join(predfor, by = "Herd") %>%
      dplyr::select(AnimalID, Behav, Old, Dens, Herd, StDevAmp, StDevTi) %>%
      rename(PredForAmp = StDevAmp, PredForTi = StDevTi) %>%
      left_join(deltafor, by = "AnimalID") %>%
      dplyr::select(-c(MaxAmpIn, MaxAmpOut, MaxTiIn, MaxTiOut)) %>%
      filter(!is.na(Herd)) # rm 2 indivs with locs too late to estimate winHR


    popold <- dat %>% 
      dplyr::select(Herd, Old) %>% 
      filter(!is.na(Herd), !is.na(Old)) %>%
      group_by(Herd) %>% 
      summarise(ppnOld = sum(Old)/n())
    
    
    popdat <- dat %>%
      dplyr::select(Herd, Dens, PredForTi) %>%
      filter(!is.na(Herd)) %>%
      # below line is what should be removed
      # you're just hackily filling in cfk density for now
      #mutate(Dens = ifelse(is.na(Dens), 21.83360, Dens)) %>%
      left_join(behavpop, by = "Herd") %>%
      left_join(popold, by = "Herd") %>%
      distinct() %>%
      arrange(Herd)
    write.csv(popdat, "pop-summaries.csv", row.names=F)
    
    
  #### DATA SUMMARIES ETC ####


    # number of elk >10yrs old
    length(which(dat$Old == 1))
    
    # number of migrants
    length(which(dat$Behav == "migrant"))

 
    # histograms of covariate distributions
    par(mfrow=c(2,2))
    hist(dat$PredForAmp, main = "Variation in Forage (NDVI amp)")
    hist(dat$PredForTi, main = "Variation in Forage (time-int NDVI)")
    hist(dat$deltaAmp, main = "DeltaFor (NDVI amp)")
    hist(dat$deltaTi, main = "DeltaFor (time-int NDVI)")

    
    
    # plots of popn data and ppn mig
    m1 <- ggplot(popdat, aes(x = ppnOld, y = ppnMig)) +
      labs(x = "", y = "Proportion Migrant") +
      geom_smooth(color = "black") +
      geom_point()
    m2 <- ggplot(popdat, aes(x = Dens, y = ppnMig)) +
      labs(x = "", y = "") +
      geom_smooth(color = "black") +
      geom_point()
    m3 <- ggplot(popdat, aes(x = PredForTi, y = ppnMig)) +
      labs(x = "", y = "") +
      geom_smooth(color = "black") +
      geom_point()
    i1 <- ggplot(popdat, aes(x = ppnOld, y = ppnOth)) +
      labs(x = "", y = "Proportion Intermediate") +
      geom_smooth(color = "black") +
      geom_point()
    i2 <- ggplot(popdat, aes(x = Dens, y = ppnOth)) +
      labs(x = "", y = "") +
      geom_smooth(color = "black") +
      geom_point()
    i3 <- ggplot(popdat, aes(x = PredForTi, y = ppnOth)) +
      labs(x = "", y = "") +
      geom_smooth(color = "black") +
      geom_point()
    r1 <- ggplot(popdat, aes(x = ppnOld, y = ppnRes)) +
      labs(x = "Proportion >10 yrs old", y = "Proportion Resident") +
      geom_smooth(color = "black") +
      geom_point()
    r2 <- ggplot(popdat, aes(x = Dens, y = ppnRes)) +
      labs(x = "Conspecific density (elk/km^2)", y = "") +
      geom_smooth(color = "black") +
      geom_point()
    r3 <- ggplot(popdat, aes(x = PredForTi, y = ppnRes)) +
      labs(x = "Forage Unpredictability (5-yr sd)", y = "") +
      geom_smooth(color = "black") +
      geom_point()
    grid.arrange(m1, m2, m3, i1, i2, i3, r1, r2, r3, ncol = 3, nrow = 3)
    
    
    # actually forage is the only interesting one; just do that
    mf <- ggplot(popdat, aes(x = PredForTi, y = ppnMig)) +
      labs(x = "", y = "Proportion Migrant") +
      geom_smooth(color = "black") +
      geom_point() +
      theme(axis.title = element_text(size = 16))
    ifor <- ggplot(popdat, aes(x = PredForTi, y = ppnOth)) +
      labs(x = "", y = "Proportion Intermediate") +
      geom_smooth(color = "black") +
      geom_point()+
      theme(axis.title = element_text(size = 16))
    rf <- ggplot(popdat, aes(x = PredForTi, y = ppnRes)) +
      labs(x = "Forage Unpredictability (5-yr sd)", y = "Proportion Resident") +
      geom_smooth(color = "black") +
      geom_point()+
      theme(axis.title = element_text(size = 16))
    grid.arrange(mf, ifor, rf, ncol = 1)


    
  #### WORKING THROUGH STATS.IDRE.UCLA.EDU EXAMPLE ####
    
    # find-replace "testdat" to "dat" first, then "test" to "mod" or whatever
    
    testdat <- dat
    unique(testdat$Behav)
    
    # rescale ndvi data to make them comparable
    testdat$rsPredForAmp <- scale(testdat$PredForAmp, center = TRUE, scale = TRUE)
    testdat$rsDeltaAmp <- scale(testdat$deltaAmp, center = TRUE, scale = TRUE)
    testdat$rsPredForTi <- scale(testdat$PredForTi, center = TRUE, scale = TRUE)
    testdat$rsDeltaTi <- scale(testdat$deltaTi, center = TRUE, scale = TRUE)
    
    
    # summary tables of relationships between each covariate and the response
    with(testdat, table(Old, Behav))
    with(testdat, table(Herd, Behav))
    with(testdat, do.call(rbind, tapply(deltaAmp, Behav, function(x) c(M = mean(x, na.rm = T), SD = sd(x, na.rm = T)))))
    with(testdat, do.call(rbind, tapply(deltaTi, Behav, function(x) c(M = mean(x, na.rm = T), SD = sd(x, na.rm = T)))))
    with(testdat, do.call(rbind, tapply(PredForAmp, Behav, function(x) c(M = mean(x, na.rm = T), SD = sd(x, na.rm = T)))))
    with(testdat, do.call(rbind, tapply(PredForTi, Behav, function(x) c(M = mean(x, na.rm = T), SD = sd(x, na.rm = T)))))    
    
    # set resident as reference level
    testdat$Behav <- relevel(testdat$Behav, ref = "resident")
    
    # m1: predfor (higher numbers mean more variable and therefore less predictable)
    test <- multinom(Behav ~ PredForAmp, data = testdat)
    summary(test)
        # a one-unit increase in PredForAmp (i.e., in unpredictable forage variation)
        # is assocated with a dec in log-odds of being resident
        # in the amount of 1.32+-0.25SE beta(resmig) [need to better understand that beta]
    
    # calculate z-scores 
    testz <- summary(test)$coefficients/summary(test)$standard.errors
    testz
    
    # assess covariate significance using 2-tailed z test (a type of Wald test I think?)
    testp <- (1 - pnorm(abs(testz), 0, 1)) * 2
    testp
    
    # exponentiate coeffs to assess relative risk (e.g. Pr(mig)/Pr(res))
    exp(coef(test))
      # the relative risk ratio for a one-unit increase in standard deviation of forage
      # is 0.27 for migrating rather than remaining resident (and 1.01 for being intermediate)
    
    # calculate predicted probabilities for each outcome level
    head(testpp <- fitted(test))
      
    
    
  #### INITIAL PRELIM FORAGE MODEL COMPARISON ####      
    
    # looking at all possible forage models for funzies
    mods.for <- list()
    modnms <- c("PredAmp", "DeltaAmp", "AllForAmp", "PredTi", "DeltaTi", "AllForTi")
    mods.for[[1]] <- multinom(Behav ~ rsPredForAmp, data = testdat)
    mods.for[[2]] <- multinom(Behav ~ rsDeltaAmp, data = testdat)
    mods.for[[3]] <- multinom(Behav ~ rsPredForAmp + rsDeltaAmp, data = testdat)
    mods.for[[4]] <- multinom(Behav ~ rsPredForTi, data = testdat)
    mods.for[[5]] <- multinom(Behav ~ rsDeltaTi, data = testdat)
    mods.for[[6]] <- multinom(Behav ~ rsPredForTi + rsDeltaTi, data = testdat)
    aictab(cand.set = mods.for, modnames = modnms)
    aicres <- data.frame(aictab(cand.set = mods.for, modnames = modnms))
    write.csv(aicres, file = "prelim-forage-aic.csv", row.names = F)
    
    
    # for actual "competition" only consider full models
    mods.for2 <- list()
    modnms2 <- c("Amp", "Ti")
    mods.for2[[1]] <- multinom(Behav ~ rsPredForAmp + rsDeltaAmp, data = testdat)
    mods.for2[[2]] <- multinom(Behav ~ rsPredForTi + rsDeltaTi, data = testdat)
    aictab(cand.set = mods.for2, modnames = modnms2)
    aicres2 <- data.frame(aictab(cand.set = mods.for2, modnames = modnms2))
    write.csv(aicres2, file = "amp-vs-ti.csv", row.names = F) # time-integrated ftw
    
    # look at univariate and multivariate ti models
    mods.for3 <- list()
    modnms3 <- c("PredTi", "DeltaTi", "AllForTi")
    mods.for3[[1]] <- multinom(Behav ~ rsPredForTi, data = testdat)
    mods.for3[[2]] <- multinom(Behav ~ rsDeltaTi, data = testdat)
    mods.for3[[3]] <- multinom(Behav ~ rsPredForTi + rsDeltaTi, data = testdat)
    aictab(cand.set = mods.for3, modnames = modnms3)
    aicres3 <- data.frame(aictab(cand.set = mods.for3, modnames = modnms3))
    write.csv(aicres3, file = "tis-aic.csv", row.names = F)
    
    # results of full ti model
    modfor <- multinom(Behav ~ PredForTi + deltaTi, data = testdat)
    summary(modfor)
    modforz <- summary(modfor)$coefficients/summary(modfor)$standard.errors
    modforp <- (1 - pnorm(abs(modforz), 0, 1)) * 2
    modforp
    
    # predicted probabilities
    pp <- fitted(modfor)
    head(pp)
    
    
    
    
    
  #### TRYING MCMCGLMM ####
    
    library(MCMCglmm)
    
    MCMCglmm(Behav ~ PredForTi, random=~Herd, family = "multinomial", data=dat,
      nitt = 1000, burnin = 10, thin = 1, verbose = T)
    # Error in if (nJ ? 1) { missing value where TRUE/FALSE needed
    
    multinom(Behav ~ rsPredForTi + rsDeltaTi, data = testdat)
      
      
      data(PlodiaPO)
      View(PlodiaPO)
      
  
      
  #### TRYING MCLOGIT ####
    
    library(mclogit)
      
      # check out help example
      data(Transport)
      
      # make resident the baseline level
      dat$Behav <- relevel(dat$Behav, ref = "resident")
          
      # first just try the same base model with no random effect
      summary(mblogit(Behav ~ PredForTi + deltaTi, data = dat))
      
      # compare to multinom output
      summary(multinom(Behav ~ PredForTi + deltaTi, data = dat))
      
      # cool, numbers are the same
      
      # now add random effect of herd
      summary(mblogit(Behav ~ PredForTi + deltaTi, data = dat, random = ~1|Herd))
      #didn't converge but did give output
      
      
      ### KRISTIN YOU LEFT OFF MODELING HERE ####
      ## mclogit could be promising for a quick solution
      ## although it has convergence issues you need to figure out
      ## specifically it "cannot find an appropriate step size"
      ## but it's just a warning not an error so not sure how big an issue
      
    
   

      
  #### other section ####  
    # test for diffs in dens, deltafor, and predfor bt behavs
    
        # summary df of means and sd's   
      
          sumbehav <- dat %>%
            group_by(Behav) %>%
            summarise(densMean = mean(Dens, na.rm=T), densSD = sd(Dens, na.rm=T),
              dfMean = mean(deltaTi, na.rm=T), dfSD = sd(deltaTi, na.rm=T),
              pfMean = mean(PredForTi, na.rm=T), pfSD = sd(PredForTi, na.rm=T),
              nOld = sum(Old, na.rm=T), nYoung = n()-nOld, nTot = n(),
              ppnOld = nOld/nTot, ppnYoung = nYoung/nTot)
          write.csv(sumbehav, "summarytable-bybehav.csv", row.names=F)
        
        
        # dens
          
          aov.d <- aov(Dens ~ Behav, data = dat)
          summary(aov.d)
          TukeyHSD(aov.d)
          
              # mod.dens <- glm(Dens ~ Behav, data = dat)
              # mod.df <- glm(deltaTi ~ Behav, data = dat)
              # mod.pf <- glm(PredForTi ~ Behav, data = dat)
              # 
              # summary(mod.dens)
              # summary(mod.df)
              # summary(mod.pf)  
          

        # deltafor

          aov.df <- aov(deltaTi ~ Behav, data = dat)
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
   
            
    

    #### plotting forage per indiv ####
         
         hist(deltafor$MaxTiIn)
         hist(deltafor$deltaTi)
         
         

     
    
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
    
  #### OLDER CODE ####  
    
    # m1: predictability of variation in forage
    testm1 <- multinom(Behav ~ PredForAmp, data = testdat)
    summary(testm1)
    testzm1 <- summary(testm1)$coefficients/summary(testm1)$standard.errors
    testpm1 <- (1 - pnorm(abs(testzm1), 0, 1)) * 2
    testpm1
    AIC(testm1)
    
    # m2: diff between forage outside and inside winter range
    testm2 <- multinom(Behav ~ deltaAmp, data = testdat)
    summary(testm2)
    testzm2 <- summary(testm2)$coefficients/summary(testm2)$standard.errors
    testpm2 <- (1 - pnorm(abs(testzm2), 0, 1)) * 2
    testpm2
    
    # m3: full forage (both variation and difference)
    testm3 <- multinom(Behav ~ PredForAmp + deltaAmp, data = testdat)
    summary(testm3)
    testzm3 <- summary(testm3)$coefficients/summary(testm3)$standard.errors
    testpm3 <- (1 - pnorm(abs(testzm3), 0, 1)) * 2
    testpm3
    
    
    # m4: conspecific density
    testm4 <- multinom(Behav ~ Dens, data = testdat)
    summary(testm4)
    testzm4 <- summary(testm4)$coefficients/summary(testm4)$standard.errors
    testpm4 <- (1 - pnorm(abs(testzm4), 0, 1)) * 2
    testpm4
    
    # m5: animal age
    testm5 <- multinom(Behav ~ Age, data = testdat)
    
    