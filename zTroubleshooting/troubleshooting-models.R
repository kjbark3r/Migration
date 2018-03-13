# figuring out actual modeling

# kristinbarker spr 2018




### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###



  #### Packages ####
  
    library(foreign)
    library(nnet) # multinom
    library(ggplot2) # pretty plots
    library(gridExtra) # >1 plot per display
    library(reshape2) # no idea, from idre stats ex
    library(ordinal) # ordinal response
    library(VGAM) # non-proportional odds
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
    popndens <- read.csv("dens-group.csv") # from consdens.R
    
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
      dplyr::select(-c(maxNDVIin, maxNDVIout)) %>%
      rename(deltaFor = deltaNDVI) %>%
      filter(!is.na(Herd)) # rm 2 indivs with locs too late to estimate winHR


    # rescale ndvi data to make them comparable
    dat$rsPredForAmp <- scale(dat$PredForAmp, center = TRUE, scale = TRUE)
    dat$rsPredForTi <- scale(dat$PredForTi, center = TRUE, scale = TRUE)
    dat$rsDeltaFor <- scale(dat$deltaFor, center = TRUE, scale = TRUE)
    
    
    # set behav as either an ordered or an unordered factor (resident is baseline for both)
    dat$behavN <- relevel(dat$Behav, ref = "resident")
    dat$behavO <- factor(dat$Behav, levels = c("resident", "other", "migrant"), ordered = TRUE)

    
    
### ### ### ### ### ### ### ### ### ### #
#### | STATS.IDRE NOMINAL LOGIT EX | ####      
### ### ### ### ### ### ### ### ### ### #      
    
    
        # find-replace "testdat" to "dat" first, then "test" to "mod" or whatever
    
    testdat <- dat
    unique(testdat$Behav)
    
    # rescale ndvi data to make them comparable
    testdat$rsPredForAmp <- scale(testdat$PredForAmp, center = TRUE, scale = TRUE)
    testdat$rsPredForTi <- scale(testdat$PredForTi, center = TRUE, scale = TRUE)
    testdat$rsDeltaFor <- scale(testdat$deltaFor, center = TRUE, scale = TRUE)
    
    
    # summary tables of relationships between each covariate and the response
    with(testdat, table(Old, Behav))
    with(testdat, table(Herd, Behav))
    with(testdat, do.call(rbind, tapply(deltaFor, Behav, function(x) c(M = mean(x, na.rm = T), SD = sd(x, na.rm = T)))))
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
    
    
          
    # make resident the baseline level
    testdat$Behav <- relevel(testdat$Behav, ref = "resident")
    
    
    # looking at all possible forage models for funzies
    mods.for <- list()
    modnms <- c("PredAmp", "DeltaFor", "AllForAmp", "PredTi", "AllForTi")
    mods.for[[1]] <- multinom(Behav ~ rsPredForAmp, data = testdat)
    mods.for[[2]] <- multinom(Behav ~ rsDeltaFor, data = testdat)
    mods.for[[3]] <- multinom(Behav ~ rsPredForAmp + rsDeltaFor, data = testdat)
    mods.for[[4]] <- multinom(Behav ~ rsPredForTi, data = testdat)
    mods.for[[5]] <- multinom(Behav ~ rsPredForTi + rsDeltaFor, data = testdat)
    aictab(cand.set = mods.for, modnames = modnms)
    aicres <- data.frame(aictab(cand.set = mods.for, modnames = modnms))
    write.csv(aicres, file = "prelim-forage-aic.csv", row.names = F)
    
    
    # for actual "competition" only consider full models
    mods.for2 <- list()
    modnms2 <- c("Amp", "Ti")
    mods.for2[[1]] <- multinom(Behav ~ rsPredForAmp + rsDeltaFor, data = testdat)
    mods.for2[[2]] <- multinom(Behav ~ rsPredForTi + rsDeltaFor, data = testdat)
    aictab(cand.set = mods.for2, modnames = modnms2)
    aicres2 <- data.frame(aictab(cand.set = mods.for2, modnames = modnms2))
    write.csv(aicres2, file = "amp-vs-ti.csv", row.names = F) # time-integrated ftw
    
    # look at univariate and multivariate ti models
    mods.for3 <- list()
    modnms3 <- c("PredTi", "DeltaFor", "AllForTi")
    mods.for3[[1]] <- multinom(Behav ~ rsPredForTi, data = testdat)
    mods.for3[[2]] <- multinom(Behav ~ rsDeltaFor, data = testdat)
    mods.for3[[3]] <- multinom(Behav ~ rsPredForTi + rsDeltaFor, data = testdat)
    aictab(cand.set = mods.for3, modnames = modnms3)
    aicres3 <- data.frame(aictab(cand.set = mods.for3, modnames = modnms3))
    write.csv(aicres3, file = "tis-aic.csv", row.names = F)
    
    # results of full ti model
    modfor <- multinom(Behav ~ PredForTi + deltaFor, data = testdat)
    summary(modfor)
    modfor
    modforz <- summary(modfor)$coefficients/summary(modfor)$standard.errors
    modforp <- (1 - pnorm(abs(modforz), 0, 1)) * 2
    modforp
    
    # predicted probabilities
    pp <- fitted(modfor)
    head(pp)
    
    
    
### ### ### ### ### ### ### ### ### ### ### ###
#### | MESSING AROUND WITH PACKAGES | ####      
### ### ### ### ### ### ### ### ### ### ### ###       
    
    
        
    
  #### TRYING MCMCGLMM ####
    
    library(MCMCglmm)
    
    MCMCglmm(behavO ~ PredForTi, random=~Herd, family = "multinomial", data=dat,
      nitt = 1000, burnin = 10, thin = 1, verbose = T)
    # Error in if (nJ ? 1) { missing value where TRUE/FALSE needed
    
    multinom(Behav ~ rsPredForTi + rsDeltaFor, data = testdat)
      
      
      data(PlodiaPO)
      View(PlodiaPO)
      
  
      
  #### TRYING MCLOGIT ####
    
    library(mclogit)
      
      # check out help example
      data(Transport)
      
      # make resident the baseline level
      olddat$Behav <- relevel(olddat$Behav, ref = "resident")
      olddat <- droplevels(olddat)
          
      # first just try the same base model with no random effect
      summary(mblogit(Behav ~ PredForTi + deltaFor, data = dat))
      
      # compare to multinom output
      summary(multinom(Behav ~ PredForTi + deltaFor, data = dat))
      
      # cool, numbers are the same
      
      # now add random effect of herd
      summary(mblogit(Behav ~ predFor + deltaFor, data = dat, random = ~1|Herd),
        control = list(maxit = 150))
      # this somehow works
      
      mblogit(Behav ~ predFor + deltaFor, data = dat, random = ~1|Herd) # converged
      mblogit(Behav ~ predFor + deltaFor, data = olddat, random = ~1|Herd) # didn't converge
      mblogit(Behav ~ predFor + deltaFor, data = dat, random = ~1|Herd, control = mclogit.control(maxit = 50)) # works
      mblogit(Behav ~ predFor + deltaFor, data = olddat, random = ~1|Herd, control = mclogit.control(maxit = 150)) # didn't converge
      mblogit(Behav ~ predFor + deltaFor, data = dat, random = ~1|Herd, subset = !is.na(Old)) # didn't converge
      mblogit(Behav ~ predFor + deltaFor, data = olddat, random = ~1|Herd, control = mclogit.control(maxit = 999))
      mblogit(Behav ~ predFor, data = olddat) # works
      mblogit(Behav ~ deltaFor, data = olddat) # works
      mblogit(Behav ~ predFor + deltaFor, data = olddat) # works
      mblogit(Behav ~ predFor + deltaFor, random = ~1|Herd, data = olddat) # fails. it's the random effect.  


      
      tm1 <- mblogit(Behav ~ deltaFor, dat = olddat)
      tm2 <- mblogit(Behav ~ predFor + deltaFor, dat = olddat)
      tm3 <- mblogit(Behav ~ predFor + deltaFor + deltaFor:predFor, dat = olddat)
      tm4 <- mblogit(Behav ~ predFor + deltaFor + Dens, dat = olddat)
      tm5 <- mblogit(Behav ~ predFor + deltaFor + Dens + deltaFor:Dens, dat = olddat)
      tm6 <- mblogit(Behav ~ predFor + deltaFor + Old, dat = olddat)
      tm7 <- mblogit(Behav ~ predFor + deltaFor + Old + deltaFor:Old, dat = olddat)
      tm8 <- mblogit(Behav ~ predFor + deltaFor + Old + Dens, dat = olddat)
      tm9 <- mblogit(Behav ~ predFor + deltaFor + Old + Dens + Old:Dens, dat = olddat)
      tm10 <- mblogit(Behav ~ densOwn, dat = olddat)
      tm11 <- mblogit(Behav ~ predFor + deltaFor + densOwn, dat = olddat)
      tm12 <- mblogit(Behav ~ predFor + deltaFor + densOwn + deltaFor:predFor, dat = olddat)
      tm13 <- mblogit(Behav ~ predFor + deltaFor + densOwn + deltaFor:densOwn, dat = olddat)
      tm14 <- mblogit(Behav ~ densOwn + Old + densOwn:Old, dat = olddat)
      tm15 <- mblogit(Behav ~ predFor + deltaFor + densOwn + Old + densOwn:Old, dat = olddat)
      tm16 <- mblogit(Behav ~ irrig, dat = olddat)
      tm17 <- mblogit(Behav ~ densOwn + irrig, dat = olddat)
      tm18 <- mblogit(Behav ~ predFor + deltaFor + irrig  , dat = olddat)
      tm19 <- mblogit(Behav ~ predFor + deltaFor + irrig + deltaFor:irrig, dat = olddat)  
      tm20 <- mblogit(Behav ~ predFor + deltaFor + irrig + predFor:irrig, dat = olddat)
      tm21 <- mblogit(Behav ~ irrig + Dens + irrig:Dens, dat = olddat)
      tm22 <- mblogit(Behav ~ predFor + irrig + Dens + irrig:Dens, dat = olddat)  
      tm23 <- mblogit(Behav ~ predFor + irrig + Old + Dens + Old:Dens, dat = olddat)  
      tm24 <- mblogit(Behav ~ densOwn + irrig + Dens + irrig:Dens, dat = olddat)
      tm25 <- mblogit(Behav ~ densOwn + irrig + Dens + densOwn:Dens, dat = olddat) 
      tm26 <- mblogit(Behav ~ predFor + irrig + densOwn + irrig:densOwn, dat = olddat) 
      
      
      # compete with AICc #
      tmods <- list()
      tmodnms <- paste0("tm", rep(1:26))
      for (i in 1:length(tmodnms)) { tmods[[i]] <- get(tmodnms[[i]]) }
      # oh of fucking course aic doesn't work with these models
      
      


      
      
      install.packages("mixcat")
      
      library(mixcat)
      npmlt(dat$Behav ~ dat$deltaFor)
      npmlt(dat$Behav ~ dat$predFor) # christ, it can't even do this right
      npmlt(dat$Behav ~ dat$deltaFor, random = ~1+dat$Herd)
      npmlt(dat$Behav ~ dat$predFor + dat$deltaFor, random = ~1+dat$Herd)

      
      
      
      
    
### ### ### ### ### ### ### ### ### ### ### ###
#### | TRYING DIFFERENT MODELING OPTIONS | ####      
### ### ### ### ### ### ### ### ### ### ### ###   
  
    
  #### Nominal ####
    
    
    ## a) multinom from nlm package ####
    

      ## with an interaction ##
      mint <- multinom(Behav ~ rsPredForTi + rsDeltaFor + Old + rsDeltaFor*Old, data = dat)
  

  #### Ordinal ####   
    
    ## a) clm from ordinal package ####

        # goal 1: make one simple model work
        ord <- clm(behavO ~ rsPredForTi + rsDeltaFor, data = dat)
        ord
        summary(ord)
            
        # compare to nominal model    
        nom <- multinom(behavN ~ rsPredForTi + rsDeltaFor, data = dat)
        summary(nom)
    
    ## b) vglm with acat from VGAM package (adjacent-category) ####
        
        # i think this allows betas to differ bt groups
        adj <- vglm(behavO ~ rsPredForTi + rsDeltaFor, family = acat, data = dat)
        adj
        summary(adj)
        # yes it does, but i don't think it supports random effects
        
        #adyr <- vglm(behavO ~ rsPredForTi + rsDeltaFor + (1|Herd), family = acat, data = dat)
        # yeah nope
        
        # fixed effect of herd
        adjherd <- vglm(behavO ~ Herd, acat, data = dat)
        summary(adjherd)
        
    ## c) clmm from ordinal package, allows mixed effects ####    
    
        # try adding mixed effect to cumulative link
        me <- clmm(behavO ~ rsPredForTi + rsDeltaFor + (1|Herd), data = dat, Hess = TRUE)
        summary(me)
        me
        str(me)
        me$dims$df.residual # residual degrees of freedom
        

 
       
    
## ### ### ### ### ### ### ### ### ### ### ### ### ##
#### | IDENTIFYING MOST APPROPRIATE MODEL TYPE | ####      
## ### ### ### ### ### ### ### ### ### ### ### ### ##
    
    
    
    #### Proportional odds assumption met? ####
    
        ## ordinal model with proportional odds
        po <- vglm(behavO ~ rsPredForTi, 
          family = cumulative(parallel = TRUE), 
          data = dat)
        
    
        ## ordinal model without proportional odds
        npo <- vglm(behavO ~ rsPredForTi, 
          family = cumulative, 
          data = dat)
        
    
        ## compare models
        c1 <- pchisq(deviance(po) - deviance(npo),
          df = df.residual(po) - df.residual(npo),
          lower.tail= FALSE)
        c1 # 0.200
            # this result indicates that NOT using proportional odds 
            # does not significantly improve model fit. 
            # i interpret this to mean we meet the proportional odds assumption
            # (at least for the predFor covariate, but when I added rsDeltaFor
            #  the number actually increased further)
        
        
        ## oh i just learned the ordinal package has actual function to test this
        nominal_test(fe) # awesome, exact same number 
        scale_test(fe) # oh hell, this is significant. and i have no idea what that means.
        help(package = "ordinal")    
          # after going through help file and agresti 2002, learned that this is ok
          # the most important factor in deciding whether to use proportional odds vs
          # other kinds of models is how you want to be able to draw inference
          # and we passed the nominal test at least, so i'm not concerned abt the scale test
        
        
    
    #### Random effect of herd merit increased model complexity? ####
    
        ## model with random effect
        re <- clmm(behavO ~ rsPredForTi + (1|Herd), data = dat)
    
        ## model without random effect
        fe <- clm(behavO ~ rsPredForTi, data = dat)
    
        ## compare models
        anova(re, fe)
          # Pr(>Chisq) = 1.1e-08*** | LR.stat = 32.654
          # this result indicates that the random effect fits the data
          # "significantly" better than not including it
        
    ## takeaway from this section: 
    ## use clmm() to estimate proportional odds ordinal regressions with random effect of herd
    ## but also specify each model using clm() and test proportional odds assumption is met

        
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### #
#### TRANSFORMING SKEWED COVARIATES PRIOR TO STANDARDIZATION ####        
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### #
        
      # histograms of covariate distributions
      par(mfrow=c(3,2))
      hist(dat$PredForAmp, main = "Variation in Forage (NDVI amp)")
      hist(dat$PredForTi, main = "Variation in Forage (time-int NDVI)")
      hist(dat$deltaFor, main = "Delta Forage")
      hist(dat$Dens, main = "Conspecific Density")
      hist(dat$ppnAg, main = "Proportion IrrigAg")
      hist(dat$densOwn, main = "Human Use Intensity")
      # most things (exception of varFor) are right-skewed; try transforming b4 standardize
      
      # log-transformed covariates
      par(mfrow=c(2,2))
      hist(log(dat$deltaFor), main = "LOG Delta Forage")
      hist(log(dat$Dens), main = "LOG Conspecific Density")
      hist(log(dat$ppnAg), main = "LOG Proportion IrrigAg")
      hist(log(dat$densOwn), main = "LOG Human Use Intensity")
      # better, rolling with it    
    # Dens and ppnAg are ok but not great after log transformation
    # inverse better tha log?
    par(mfrow=c(2,2))
    hist(log(dat$Dens), main = "logDens")
    hist(log(dat$ppnAg), main = "logAg")
    hist(dat$Dens^-1, main = "invDens")
    hist(dat$Dens^-1, main = "invAg")
    # no, log is better
    
    summary(log(dat$Dens))
    summary(log(dat$ppnAg)) # oh right, has 0s
    summary(dat$ppnAg)
    
    hist(log(dat$ppnAg+0.001))
    hist(dat$ppnAg+0.001)
    hist(log(dat$ppnAg+0.01))
    hist(log(dat$ppnAg+0.00000001))
    
    test <- filter(dat, ppnAg > 0)
    hist(test$ppnAg)
    hist(log(test$ppnAg))
    hist(log(dat$ppnAg))
    
    # dur, never mind, don't need to transform to compare models
    
    
    
### ### ### ### ### ##
#### PRELIM PLOTS ####        
### ### ### ### ### ##
    
        
    # plots of popn data and ppn mig
    m1 <- ggplot(popdat, aes(x = ppnOld, y = ppnMig, colour = Herd)) +
      labs(x = "", y = "Proportion Migrant") +
      geom_smooth(color = "black") +
      geom_point()
    m2 <- ggplot(popdat, aes(x = Dens, y = ppnMig, colour = Herd)) +
      labs(x = "", y = "") +
      geom_smooth(color = "black") +
      geom_point()
    m3 <- ggplot(popdat, aes(x = PredForTi, y = ppnMig, colour = Herd)) +
      labs(x = "", y = "") +
      geom_smooth(color = "black") +
      geom_point()
    i1 <- ggplot(popdat, aes(x = ppnOld, y = ppnOth, colour = Herd)) +
      labs(x = "", y = "Proportion Intermediate") +
      geom_smooth(color = "black") +
      geom_point()
    i2 <- ggplot(popdat, aes(x = Dens, y = ppnOth, colour = Herd)) +
      labs(x = "", y = "") +
      geom_smooth(color = "black") +
      geom_point()
    i3 <- ggplot(popdat, aes(x = PredForTi, y = ppnOth, colour = Herd)) +
      labs(x = "", y = "") +
      geom_smooth(color = "black") +
      geom_point()
    r1 <- ggplot(popdat, aes(x = ppnOld, y = ppnRes, colour = Herd)) +
      labs(x = "Proportion >10 yrs old", y = "Proportion Resident") +
      geom_smooth(color = "black") +
      geom_point()
    r2 <- ggplot(popdat, aes(x = Dens, y = ppnRes, colour = Herd)) +
      labs(x = "Conspecific density (elk/km^2)", y = "") +
      geom_smooth(color = "black") +
      geom_point()
    r3 <- ggplot(popdat, aes(x = PredForTi, y = ppnRes, colour = Herd)) +
      labs(x = "Forage Predictability (5-yr sd*-1)", y = "") +
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

    
    
    
    
### ### ### ### ### ### ### ### ### ### ### 
#### PRELIM SUMMARIES (FOR DRAFT RSLTS) ####        
### ### ### ### ### ### ### ### ### ### ###     
    
    
        
        # summary df of means and sd's (silly, some of these are skewed, re-report)  
      
          sumbehav <- dat %>%
            group_by(Behav) %>%
            summarise(densMean = mean(Dens, na.rm=T), densSD = sd(Dens, na.rm=T),
              dfMean = mean(deltaFor, na.rm=T), dfSD = sd(deltaFor, na.rm=T),
              pfMean = mean(PredForTi, na.rm=T), pfSD = sd(PredForTi, na.rm=T),
              nOld = sum(Old, na.rm=T), nYoung = n()-nOld, nTot = n(),
              ppnOld = nOld/nTot, ppnYoung = nYoung/nTot)
          write.csv(sumbehav, "summarytable-bybehav.csv", row.names=F)

          length(which(dat$Behav == "migrant"))/nrow(dat)
          
          
      
### ### ### ### ### ### 
#### MISC/NO IDEAR ####        
### ### ### ### ### ###          
         
    
    # mod.dens <- glm(Dens ~ Behav, data = dat)
    # mod.df <- glm(deltaTi ~ Behav, data = dat)
    # mod.pf <- glm(PredForTi ~ Behav, data = dat)
    # 
    # summary(mod.dens)
    # summary(mod.df)
    # summary(mod.pf)
          
          
        

    #### plotting forage per indiv ####
         
         hist(deltafor$MaxTiIn)
         hist(deltafor$deltaFor)
        

     
    
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
    
  # prelim nominal models, just checking stuff out #  
    
    # m1: predictability of variation in forage
    testm1 <- multinom(Behav ~ PredForAmp, data = testdat)
    summary(testm1)
    testzm1 <- summary(testm1)$coefficients/summary(testm1)$standard.errors
    testpm1 <- (1 - pnorm(abs(testzm1), 0, 1)) * 2
    testpm1
    AIC(testm1)
    
    # m2: diff between forage outside and inside winter range
    testm2 <- multinom(Behav ~ deltaFor, data = testdat)
    summary(testm2)
    testzm2 <- summary(testm2)$coefficients/summary(testm2)$standard.errors
    testpm2 <- (1 - pnorm(abs(testzm2), 0, 1)) * 2
    testpm2
    
    # m3: full forage (both variation and difference)
    testm3 <- multinom(Behav ~ PredForAmp + deltaFor, data = testdat)
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
          
    
    
    
    
#### PLAYING WITH NEW IDEAS POST-INITIAL RUN ####
    
    # seeing whether deltaFor coeff overlaps 0 if alone in model
    confint(clmm(behavO ~ deltaFor + (1|Herd), data = dat))
    # it does. lame.
    
    
    #### this may kill my soul, but... reducing to mig/res (int = res) ####
    
        library(lme4)
        
          # new df so i don't taint the old one with such heresy
          testdat <- moddat %>%
            mutate(Mig = ifelse(Behav == "migrant", 1, 0))
        
          # define models #
          tfmh <- glmer(Mig ~ predFor + deltaFor + (1 | Herd), 
            family = binomial(logit), data = testdat)
          tconsdens <- glmer(Mig ~ predFor + deltaFor + Dens + deltaFor*Dens + (1|Herd), 
            family = binomial(logit), data = testdat) # convergence failure
          told <- glmer(Mig ~ predFor + deltaFor + Old + deltaFor*Old + (1|Herd), 
            family = binomial, data = testdat) # convergence failure
          thumshld <- glmer(Mig ~ densOwn + (1|Herd), 
            family = binomial(link = "logit"), data = testdat)
          tagrsub <- glmer(Mig ~ ppnAg + (1|Herd), 
            family = binomial, data = testdat)
          
          # look at CIs of coefficients to assess hypothesis support within topics #
          confint(tfmh) # confusing error in zeta(shiftpar, start = opt[seqpar1][-w]) : profiling detected new, lower deviance
          confint(tconsdens) # similar result to ordinal; overlappy except predfor
          confint(told) # same confusing error
          confint(thumshld) # overlappy
          confint(tagrsub) # all jacked up
          
          # prelim aic and bic [don't forget you had convergence failures]
                # compete with AICc #
          mods <- list()
          modnms <- c("Forage", "Density", "Old", "HumanShld", "AgrSub")
          mods[[1]] <- tfmh
          mods[[2]] <- tconsdens
          mods[[3]] <- told
          mods[[4]] <- thumshld
          mods[[5]] <- tagrsub
          aictab(cand.set = mods, modnames = modnms)
    
          
          
          # compete with BIC #
          tbic <- data.frame(BIC(tfmh, tconsdens, told, thumshld, tagrsub))
          
          
          # the good news: similar results here give me no reason to switch to binary behavior :)
          # the bad news: similar results mean i still have, like, no support for anything :(
          
          
   
                 
          
    #### convincing myself fixed effect of herd is essentially the same as random ####       
          
       
      # define models #
      tfmh <- clm(behavO ~ predFor + deltaFor + Herd, data = moddat)
      tconsdens <- clm(behavO ~ predFor + deltaFor + Dens + deltaFor*Dens + Herd, data = moddat)
      told <- clm(behavO ~ predFor + deltaFor + Old + deltaFor*Old + Herd, data = moddat)
      thumshld <- clm(behavO ~ densOwn + Herd, data = moddat)
      tagrsub <- clm(behavO ~ ppnAg + Herd, data = moddat)
      
      # look at model summaries #
      summary(tfmh)
      summary(tconsdens)
      summary(told)
      summary(thumshld)
      summary(tagrsub)     
      
      ## trying to estimate a per-herd coefficient makes everything screwy
      ## can't get SEs, zvals, etc. warning message says:
      ## (1) Hessian is numerically singular: parameters are not uniquely determined 
      ## In addition: Absolute convergence criterion was met, but relative criterion was not met 
      ## prob has to do with having insufficient data per herd to estimate anything with
      ## although i will note anecdotally the coeffs differ in sign and magnitude bt herds...
      
      # out of curiosity
      therd <- clm(behavO ~ Herd, data = moddat)
      summary(therd)
      # yep same issue
      
      # on to stage 2... figuring out sample size requirements for herd-specific models
      
      # oh lord. stage 2 too depressing. moving on.
      
      
    #### see if results are more clear if split models depending whether majority of herd migs ####
      
      # add ppnMig data per herd
      popnsum <- read.csv("pop-summaries.csv") %>% dplyr::select(Herd, ppnMig)
      testdat <- left_join(testdat, popnsum, by = "Herd") 
      
      tfmhmig <- clmm(behavO ~ predFor + deltaFor + (1|Herd), 
        data = filter(testdat, ppnMig > 0.5))
      tfmhnon <- clmm(behavO ~ predFor + deltaFor + (1|Herd), 
        data = filter(testdat, ppnMig < 0.5))
      
      confint(tfmhmig)
      confint(tfmhnon)
      # shit, that did nothing. actually made things worse, somewhat inexplicably
      
      # for funzies/heartbreak, same thing but just behav~herd
      therdmig <- clm(behavO ~ Herd, data = filter(testdat, ppnMig > 0.5))
      therdnon <- clm(behavO ~ Herd, data = filter(testdat, ppnMig < 0.5))
      summary(therdmig); summary(therdnon)
      confint(therdnon)
 
      
           
    #### diffs based on whether reside in protected area (GYE) or not? ####
      
      testdat$gye <- ifelse(testdat$Herd == "HD314" | testdat$Herd == "Madison" | 
          testdat$Herd == "Dome" | testdat$Herd == "Clarks Fork" | testdat$Herd == "Silver Run" | 
          testdat$Herd == "Mill Creek" | testdat$Herd == "Greeley", 1, 0)
      tgye <- clmm(behavO ~ predFor + deltaFor + gye + (1|Herd), data = testdat)
      summary(tgye)
      confint(tgye)
      
      tgye2 <- clmm(behavO ~ densOwn + gye + I(densOwn*gye) + (1|Herd), data = testdat)
      confint(tgye2)
      # newp
      
    #### interaction with irrig, i know this is cheap, just curious ####
      
      tir <- clmm(behavO ~ predFor + deltaFor + irrig + I(deltaFor*irrig) + (1|Herd), data = moddat)
      summary(tir)
      confint(tir)
      # predfor+ & sig; intrxn+&sig, deltafor&irrig negbutoverlap0
      
      
      
      
      
    #### oh duh, try checking out 90% CI instead, and 80% for funzies ####
      ## aaand 75 and 70 after that did basically nothing... sigh ##
      
      coef(summary(fmh))
      str(coef(summary(fm)))
      attributes(coef(summary(fmh)))
      attributes(coef(summary(fmh)))
      # ok kristin learn to extract data based on attributes later, for now be hacky
      
      # fmh
      df.fmh <- data.frame(coef(summary(fmh))) %>%
        tibble::rownames_to_column() %>%
        rename(coeff = rowname, se = "Std..Error") %>%
        mutate(CI90low = Estimate - (1.645*se), CI90high = Estimate + (1.645*se),
               CI80low = Estimate - (1.28*se), CI80high = Estimate + (1.28*se),
               CI75low = Estimate - (1.15*se), CI75high = Estimate + (1.15*se),
               CI70low = Estimate - (1.04*se), CI70high = Estimate + (1.04*se)) %>%
        mutate(spt90 = ifelse(CI90low < 0 & CI90high > 0, 0, 1),
               spt80 = ifelse(CI80low < 0 & CI80high > 0, 0, 1),
               spt75 = ifelse(CI75low < 0 & CI75high > 0, 0, 1),
               spt70 = ifelse(CI70low < 0 & CI70high > 0, 0, 1))
      
      
      # consdens
      df.consdens <- data.frame(coef(summary(consdens))) %>%
        tibble::rownames_to_column() %>%
        rename(coeff = rowname, se = "Std..Error") %>%
        mutate(CI90low = Estimate - (1.645*se), CI90high = Estimate + (1.645*se),
               CI80low = Estimate - (1.28*se), CI80high = Estimate + (1.28*se),
               CI75low = Estimate - (1.15*se), CI75high = Estimate + (1.15*se),
               CI70low = Estimate - (1.04*se), CI70high = Estimate + (1.04*se)) %>%
        mutate(spt90 = ifelse(CI90low < 0 & CI90high > 0, 0, 1),
               spt80 = ifelse(CI80low < 0 & CI80high > 0, 0, 1),
               spt75 = ifelse(CI75low < 0 & CI75high > 0, 0, 1),
               spt70 = ifelse(CI70low < 0 & CI70high > 0, 0, 1))
      
      
      # old
      df.old <- data.frame(coef(summary(old))) %>%
        tibble::rownames_to_column() %>%
        rename(coeff = rowname, se = "Std..Error") %>%
        mutate(CI90low = Estimate - (1.645*se), CI90high = Estimate + (1.645*se),
               CI80low = Estimate - (1.28*se), CI80high = Estimate + (1.28*se),
               CI75low = Estimate - (1.15*se), CI75high = Estimate + (1.15*se),
               CI70low = Estimate - (1.04*se), CI70high = Estimate + (1.04*se)) %>%
        mutate(spt90 = ifelse(CI90low < 0 & CI90high > 0, 0, 1),
               spt80 = ifelse(CI80low < 0 & CI80high > 0, 0, 1),
               spt75 = ifelse(CI75low < 0 & CI75high > 0, 0, 1),
               spt70 = ifelse(CI70low < 0 & CI70high > 0, 0, 1))
      
      # humshld
      df.humshld <- data.frame(coef(summary(humshld))) %>%
        tibble::rownames_to_column() %>%
        rename(coeff = rowname, se = "Std..Error") %>%
        mutate(CI90low = Estimate - (1.645*se), CI90high = Estimate + (1.645*se),
               CI80low = Estimate - (1.28*se), CI80high = Estimate + (1.28*se),
               CI75low = Estimate - (1.15*se), CI75high = Estimate + (1.15*se),
               CI70low = Estimate - (1.04*se), CI70high = Estimate + (1.04*se)) %>%
        mutate(spt90 = ifelse(CI90low < 0 & CI90high > 0, 0, 1),
               spt80 = ifelse(CI80low < 0 & CI80high > 0, 0, 1),
               spt75 = ifelse(CI75low < 0 & CI75high > 0, 0, 1),
               spt70 = ifelse(CI70low < 0 & CI70high > 0, 0, 1))
      
      # agrsub
      df.agrsub <- data.frame(coef(summary(agrsub))) %>%
        tibble::rownames_to_column() %>%
        rename(coeff = rowname, se = "Std..Error") %>%
        mutate(CI90low = Estimate - (1.645*se), CI90high = Estimate + (1.645*se),
               CI80low = Estimate - (1.28*se), CI80high = Estimate + (1.28*se),
               CI75low = Estimate - (1.15*se), CI75high = Estimate + (1.15*se),
               CI70low = Estimate - (1.04*se), CI70high = Estimate + (1.04*se)) %>%
        mutate(spt90 = ifelse(CI90low < 0 & CI90high > 0, 0, 1),
               spt80 = ifelse(CI80low < 0 & CI80high > 0, 0, 1),
               spt75 = ifelse(CI75low < 0 & CI75high > 0, 0, 1),
               spt70 = ifelse(CI70low < 0 & CI70high > 0, 0, 1))
          
      
      
      # fmhagr
      fmhagr <- clmm(behavO ~ predFor + deltaFor + irrig + I(deltaFor*irrig) + (1|Herd), data = moddat)
      df.fmhagr <- data.frame(coef(summary(fmhagr))) %>%
        tibble::rownames_to_column() %>%
        rename(coeff = rowname, se = "Std..Error") %>%
        mutate(CI90low = Estimate - (1.645*se), CI90high = Estimate + (1.645*se),
               CI80low = Estimate - (1.28*se), CI80high = Estimate + (1.28*se),
               CI75low = Estimate - (1.15*se), CI75high = Estimate + (1.15*se),
               CI70low = Estimate - (1.04*se), CI70high = Estimate + (1.04*se)) %>%
        mutate(spt90 = ifelse(CI90low < 0 & CI90high > 0, 0, 1),
               spt80 = ifelse(CI80low < 0 & CI80high > 0, 0, 1),
               spt75 = ifelse(CI75low < 0 & CI75high > 0, 0, 1),
               spt70 = ifelse(CI70low < 0 & CI70high > 0, 0, 1))
      
            
      
    #### try nominal behavior instead, bc anovas do suggest there should be diffs bt groups, so maybe they're just not ordered really ####      
      
      
      # this is prelim bc still haven't figured out how to include a random effect in a canned r option
      
      # order response with res as baseline for first run
      moddat$behavN <- relevel(moddat$Behav, ref = "resident")
      
      # define models #
      nfmh <- multinom(behavN ~ predFor + deltaFor, data = moddat)
      nconsdens <- multinom(behavN ~ predFor + deltaFor + Dens + deltaFor*Dens, data = moddat)
      nold <- multinom(behavN ~ predFor + deltaFor + Old + deltaFor*Old, data = moddat)
      nhumshld <- multinom(behavN ~ densOwn, data = moddat)
      nagrsub <- multinom(behavN ~ ppnAg, data = moddat) # no converge
      nfmhagr <- multinom(behavN ~ predFor + deltaFor + irrig + I(deltaFor*irrig), data = moddat)
      

      summary(nfmh)
      str(summary(nfmh))
      summary(nfmh)$standard.errors
      summary(nfmh)$coefficients
      str(summary(nfmh)$standard.errors)
      
      
      
      # fmh nominal
      df.nfmh <- data.frame(summary(nfmh)$coefficients) %>%
        tibble::rownames_to_column() %>%
        rename(betaPredFor = predFor, betaDeltaFor = deltaFor) %>%
        bind_cols(data.frame(summary(nfmh)$standard.errors)) %>%
        rename(sePredFor = predFor, seDeltaFor = deltaFor) %>%
        mutate(CI90low = betaPredFor - (1.645*sePredFor), CI90high = betaPredFor + (1.645*sePredFor),
               CI80low = betaPredFor - (1.28*sePredFor), CI80high = betaPredFor + (1.28*sePredFor),
               CI75low = betaPredFor - (1.15*sePredFor), CI75high = betaPredFor + (1.15*sePredFor),
               CI70low = betaPredFor - (1.04*sePredFor), CI70high = betaPredFor + (1.04*sePredFor)) %>%
        mutate(spt90 = ifelse(CI90low < 0 & CI90high > 0, 0, 1),
               spt80 = ifelse(CI80low < 0 & CI80high > 0, 0, 1),
               spt75 = ifelse(CI75low < 0 & CI75high > 0, 0, 1),
               spt70 = ifelse(CI70low < 0 & CI70high > 0, 0, 1))
      
      
      
      
      # fmh nominal
      df.ndens <- data.frame(summary(nconsdens)$coefficients) %>%
        tibble::rownames_to_column() %>%
        rename(betaPredFor = predFor, betaDeltaFor = deltaFor,
          betaDens = Dens, nestIntrxn = deltaFor.Dens) %>%
        bind_cols(data.frame(summary(nconsdens)$standard.errors)) %>%
        rename(sePredFor = predFor, seDeltaFor = deltaFor) %>%
        mutate(CI90low = betaPredFor - (1.645*sePredFor), CI90high = betaPredFor + (1.645*sePredFor),
               CI80low = betaPredFor - (1.28*sePredFor), CI80high = betaPredFor + (1.28*sePredFor),
               CI75low = betaPredFor - (1.15*sePredFor), CI75high = betaPredFor + (1.15*sePredFor),
               CI70low = betaPredFor - (1.04*sePredFor), CI70high = betaPredFor + (1.04*sePredFor)) %>%
        mutate(spt90 = ifelse(CI90low < 0 & CI90high > 0, 0, 1),
               spt80 = ifelse(CI80low < 0 & CI80high > 0, 0, 1),
               spt75 = ifelse(CI75low < 0 & CI75high > 0, 0, 1),
               spt70 = ifelse(CI70low < 0 & CI70high > 0, 0, 1))
      
      

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### # 
#### | CALCING AND STORING 95-70% CIs FROM UNIVARIATE  | ####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### #      
      
      
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
           
      
      
      
      
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### # 
#### | FROM INITIAL RUN; PASTE BACK IN WITH OTHER THINGS LATER | ####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### #
      
      
      # define models #
      fmh <- clmm(behavO ~ predFor + deltaFor + (1|Herd), data = moddat)
      consdens <- clmm(behavO ~ predFor + deltaFor + Dens + deltaFor*Dens + (1|Herd), data = moddat)
      old <- clmm(behavO ~ predFor + deltaFor + Old + deltaFor*Old + (1|Herd), data = moddat)
      humshld <- clmm(behavO ~ densOwn + (1|Herd), data = moddat)
      agrsub <- clmm(behavO ~ ppnAg + (1|Herd), data = moddat)
      fmhagr <- clmm(behavO ~ predFor + deltaFor + irrig + I(deltaFor*irrig) + (1|Herd), data = moddat)
      
      age <- clmm(behavO ~ Old + (1|Herd), data = moddat)
      confint(age)
      
      # look at CIs of coefficients to assess hypothesis support within topics #
      confint(fmh)
      confint(consdens)
      confint(old)
      confint(humshld)
      confint(agrsub)
      confint(fmhagr)
      
      # look at model summaries #
      summary(fmh)
      summary(consdens)
      summary(old)
      summary(humshld)
      summary(agrsub) 
      summary(fmhagr)
      
      
      # compete with AICc #
      mods <- list()
      modnms <- c("Forage", "Density", "Old", "HumanShld", "AgrSub", "ForAgr")
      mods[[1]] <- fmh
      mods[[2]] <- consdens
      mods[[3]] <- old
      mods[[4]] <- humshld
      mods[[5]] <- agrsub
      mods[[6]] <- fmhagr
      aictab(cand.set = mods, modnames = modnms)
      aicres <- data.frame(aictab(cand.set = mods, modnames = modnms))
      
      
      # compete with BIC #
      bicres <- data.frame(BIC(fmh, consdens, old, humshld, agrsub))

      
      #### Assess proportional odds assumption ####
      
      
      
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
#### | KEEPING TRACK OF ALL PRELIM BIG MODEL RUNS | ####
### ### ### ### ### ### ### ### ### ### ### ### ### ### #     
      
      
      #### 1. all new models, using ppnAg ####
      
            # define each model
      m1 <- clmm(behavO ~ betFor + (1|Herd), dat = moddat)
      m2 <- clmm(behavO ~ predFor + betFor + (1|Herd), dat = moddat)
      m3 <- clmm(behavO ~ predFor + betFor + I(betFor*predFor) + (1|Herd), dat = moddat)
      m4 <- clmm(behavO ~ predFor + betFor + Dens + (1|Herd), dat = moddat)
      m5 <- clmm(behavO ~ predFor + betFor + Dens + I(betFor*Dens) + (1|Herd), dat = moddat)
      m6 <- clmm(behavO ~ predFor + betFor + Old + (1|Herd), dat = moddat)
      m7 <- clmm(behavO ~ predFor + betFor + Old + I(betFor*Old) + (1|Herd), dat = moddat)
      m8 <- clmm(behavO ~ predFor + betFor + Old + Dens + (1|Herd), dat = moddat)
      m9 <- clmm(behavO ~ predFor + betFor + Old + Dens + I(Old*Dens) + (1|Herd), dat = moddat)
      m10 <- clmm(behavO ~ densOwn + (1|Herd), dat = moddat)
      m11 <- clmm(behavO ~ predFor + betFor + densOwn + (1|Herd), dat = moddat)
      m12 <- clmm(behavO ~ predFor + betFor + densOwn + I(betFor*predFor) + (1|Herd), dat = moddat)
      m13 <- clmm(behavO ~ predFor + betFor + densOwn + I(betFor*densOwn) + (1|Herd), dat = moddat)
      m14 <- clmm(behavO ~ densOwn + Old + I(densOwn*Old) + (1|Herd), dat = moddat)
      m15 <- clmm(behavO ~ predFor + betFor + densOwn + Old + I(densOwn*Old) + (1|Herd), dat = moddat)
      m16 <- clmm(behavO ~ ppnAg + (1|Herd), dat = moddat)
      m17 <- clmm(behavO ~ densOwn + ppnAg + (1|Herd), dat = moddat)
      m18 <- clmm(behavO ~ predFor + betFor + ppnAg   + (1|Herd), dat = moddat)
      m19 <- clmm(behavO ~ predFor + betFor + ppnAg + I(betFor*ppnAg) + (1|Herd), dat = moddat) #Model is nearly unidentifiable: large eigenvalue ratio - Rescale variables? 
      m20 <- clmm(behavO ~ predFor + betFor + ppnAg + I(predFor*ppnAg) + (1|Herd), dat = moddat)#Model is nearly unidentifiable: large eigenvalue ratio - Rescale variables? 
      m21 <- clmm(behavO ~ ppnAg + Dens + I(ppnAg*Dens) + (1|Herd), dat = moddat) #Model is nearly unidentifiable: large eigenvalue ratio - Rescale variables? 
      m22 <- clmm(behavO ~ predFor + ppnAg + Dens + I(ppnAg*Dens) + (1|Herd), dat = moddat) #Model is nearly unidentifiable: large eigenvalue ratio - Rescale variables? 
      m23 <- clmm(behavO ~ predFor + ppnAg + Old + Dens + I(Old*Dens) + (1|Herd), dat = moddat) #Model is nearly unidentifiable: large eigenvalue ratio - Rescale variables? 
      m24 <- clmm(behavO ~ densOwn + ppnAg + Dens + I(ppnAg*Dens) + (1|Herd), dat = moddat) #Model is nearly unidentifiable: large eigenvalue ratio - Rescale variables? 
      m25 <- clmm(behavO ~ densOwn + ppnAg + Dens + I(densOwn*Dens) + (1|Herd), dat = moddat) #Model is nearly unidentifiable: large eigenvalue ratio - Rescale variables? 
      m26 <- clmm(behavO ~ predFor + ppnAg + densOwn + I(ppnAg*densOwn) + (1|Herd), dat = moddat) #Model is nearly unidentifiable: large eigenvalue ratio - Rescale variables? 

      
      
      
            
### ### ### ### ### ### ### ### ### ### ### ### ### ##
#### exploring models, validation techniques, etc ####
### ### ### ### ### ### ### ### ### ### ### ### ### ##
      
      
      t1 <- m2 # predFor + betFor
      summary(t1)
      t2 <- m4 # pred+bed + Dens
      summary(t2)
      
      anova(t1, t2)
      
      # this is cool, gives LRT for ea covariate while controlling for the others
      drop1(t1, test = "Chi")
      # totally possible i'm misinterpreting this but what i think it says is
      # if you control for whether or not forage is better outside winter range,
      # predictability of forage variation becomes less important
      # (i mean, not by much, and it could have more to do with math than bio?)
      
      # using confint() uses profile likelihood function
      confint(t1, level = 0.90)
      # so you should use this in case that's diff from yr manual calcs
      
      # make predictions about behav per cov based on model
      tData <- expand.grid(betFor = levels(moddat$betFor))
      cbind(tData, predict(t1, newdata = tData)$fit)
      # ahhhh fuck betFor isn't a factor? way to go kristin.
      # rerun everything, and while you're at it change code to use confint()
      
     
      ## ## ## ## ## 
      
      # ok done, inference still same, but now models are less stupid that ever before!
      
      t1 <- m2
      tData <- expand.grid(betFor = levels(moddat$betFor))
      cbind(tData, predict(t1, newdata = tData)$fit)
      # ok nevermind, can't predict from clmm only clm
      # maybe the random variable fucks with it??
      
      
       ## ## ## ## 
      
      # found clmm-specific tutorial (sort of... clmm2...), starting over
      
      # compare model with and without random effect of herd
      t3 <- clm(behavO ~ predFor + betFor, data = moddat)
      anova(t3, t1)
      # ahahah yeah, pretty impt to include that... (5.7e-9)
      
      # profile likelihood confidence interval
      pr1 <- profile(t1)
      # yeah doesn't work with clmm anyway
      
      
      # looking at effects per herd?!
      ci <- t1$ranef + qnorm(0.975) * sqrt(t1$condVar) %o% c(-1, 1)
      ci # these look like estimates per herd, cool
      # oh duh, ranef must be random effects
      ord.re <- order(t1$ranef)
      ci <- ci[order(t1$ranef),]
      plot(1:16, t1$ranef[ord.re], axes = FALSE, ylim = range(ci),
        xlab = "Herd", ylab = "Herd effect")
      axis(1, at = 1:16, labels = ord.re)
      axis(2)
      abline(h = 0, lty = 2)
      # so fucking cool [will be cooler when you label with herd NAMES...]
      
      # fitted or predicted probabilities w herd effects @ conditional modes
      # i.e., for an "average" herd
      # but i don't understand diff btw fitted and predicted
      test <- cbind(moddat, fitted(t1))
      test2 <- cbind(test, pred=predict(t1, newdata = moddat))
      # jk, predict still doesn't work for clmm. fitted it is.
      # oh wait, just found a new thing
      
      test2 <- cbind(test, pred=ordinal::predict(t1, newdata = moddat))
      # newp
      
      test2 <- cbind(test, pred=predict.clm(t1, newdata = moddat))
      # newp
      # ok i dunno what this documentation is talking about, this doesn't work for clmm
      
      head(fitted(t1))
      head(predict(t1))
      head(pred(t1))
      
      
      
      # real quick for funzies, compete nominal and ordinal? can i do that?
      tord <- clm(behavO ~ predFor + deltaFor, data = moddat)
      moddat$behavN <- relevel(moddat$Behav, ref = "resident")
      tnom <- multinom(behavN ~ predFor + deltaFor, data = moddat)
      anova(tord, tnom)
      # nope sure can't (calling anova() with a clm requires you to use anova.clm)
      # also tried with aictab, for the record
      
      
      # ok back to business
      
      # prediction plots, hopefully
      
      ??pred
      
      
      
   #### prediction plot code from petrilankowski.wordpress.com ####
      
      
      # function to calculate predictions from the model
      pred <- function(eta, theta, cat = 1:(length(theta) + 1), inv.link = plogis) {
        Theta <- c(-1000, theta, 1000)
        sapply(cat, function(j) inv.link(Theta[j + 1] - eta) - inv.link(Theta[j] - eta))}
      
      # plot predictions and data distributions
      plot.probabilities3 <- function(grid, model, comp.data=NULL, title="", ylim=NULL) {
        co <- model$coefficients[1:length(model$y.levels)-1]
        pre.mat <- pred(eta=rowSums(grid), theta=co)
        df<-data.frame(levels=as.numeric(as.factor(model$y.levels)))
        df["avg"] <- pre.mat[1,]
        df["low"] <- pre.mat[2,]
        df["high"] <- pre.mat[3,]
        if(!is.null(comp.data)) {
           df["freq"] <- summary(comp.data)/sum(summary(comp.data))
        }
        plot1 <- ggplot(data=df, aes(x=levels, y=avg)) + geom_line() + geom_point() +
        ggtitle(title) + ylab("probability") + xlab("") +
        geom_line(aes(x=levels, y=low), colour="#CCCCCC") +
        geom_line(aes(x=levels, y=high), colour="#CCCCCC")
        if(!is.null(comp.data)) {
           plot1 <- plot1 +
           geom_line(aes(x=levels, y=freq), lty="dotted")
        }
        if(!is.null(ylim)) {
           plot1 <- plot1 + ylim(0, ylim)
        }
        return(plot1)
      }
      
      # model - use hessian (for summary) and use 10 quadrature points (slower but more accurate ML estimate)
      t1 <- clmm(behavO ~ predFor + deltaFor + (1|Herd), data = moddat, Hess = TRUE, nAGQ = 10)
      
      # create df of all combinations of given factors for betFor = 1 and betFor = 1
      mat_worse <- expand.grid(Herd = qnorm(0.95) * c(0, -1, 1) * c(t1$ST$Herd), betFor = c(0))
      mat_better <- expand.grid(Herd = qnorm(0.95) * c(0, -1, 1) * c(t1$ST$Herd), betFor = c(1))
      
      # subset data to split betFors
      dat_worse <- filter(moddat, betFor == 0)
      dat_better <- filter(moddat, betFor == 1)
      
      # plot predicted distn and data distn for when forage better on winter range
      plot1 <- plot.probabilities3(mat_worse, t1, dat_worse$BehavO, title = "Better On Winter")
      plot1
      # omfg so cool (and even cooler that i was able to fix his outdated code, go me)
      # this shows the models predicts an elk will remain resident if forage is better on winter range
      # and will be at least intermediate otherwise
      
      # plot predicted distn and data distn for when forage better off winter range
      plot2 <- plot.probabilities3(mat_better, t1, dat_better$Behav0, title = "Better Off Winter")
      plot2
      # ah shit, doesn't this say the same when forage is better off winter range?
      # hm ok something's funky
      
      
      data(wine)
      fm2 <- clmm(rating ~ temp + contact + (1|judge), data = wine,Hess = TRUE, nAGQ = 10)
      mat_no_cold <- expand.grid(judge = qnorm(0.95) * c(0, -1, 1) * c(fm2$ST$judge), contact = c(0), temp=c(0))
      dat_no_cold.tmp <- filter(wine, contact == "no")
      dat_no_cold <- dat_no_cold.tmp[dat_no_cold.tmp["temp"] == "cold",]
      plot1 <- plot.probabilities3(mat_no_cold, fm2, dat_no_cold$rating, title = "contact No, temp Cold")
      # hm ok this dude's basically just taking the code from the clmm tutorial
      # and putting the output into a ggplot, big whoop
      # do this through tutorial instead so you understand what you're doing
      # because that didn't work quite right
      
      
      
  #### prediction plots, take 2(ish...)####
      
      
      # predict probabilities with herd effect at conditional mode (ie, for an average herd)
      preddat <- cbind(moddat, fitted(t1))
      
      # extreme herd effects (5th and 95th pctile)
      qnorm(0.95) * c(-1, 1) * c(t1$ST$Herd)
      

      # function to calculate predictions from the model
      pred <- function(eta, theta, cat = 1:(length(theta) + 1), inv.link = plogis) {
        Theta <- c(-1000, theta, 1000)
        sapply(cat, function(j) inv.link(Theta[j + 1] - eta) - inv.link(Theta[j] - eta))}
      
      pred(qnorm(0.05) * t1$ST$Herd, t1$Theta)
      # an animal in a 5th percentile herd most likely to migrate?
      
      pred(qnorm(0.51) * t1$ST$Herd, t1$Theta)
      # and an animal in as average herd is too, but slightly more so?
      
      pred(qnorm(0.95) * t1$ST$Herd, t1$Theta)
      # and an animal in a 85% percentile herd is super super likely to migrate?
      
      length(which(moddat$Behav == "migrant"))/nrow(moddat)
      # but overall probability of migrating is only 64%
      # and average ppnMig in a herd is 62%

      
      exp(coef(t1)[3])
      # predFor = 5.96
      # this tells me that the odds ratio of an elk being in a category higher than res? is ~6
      # when forage is better outside the winter range (i think)
      
      
      
  #### comparing model predictions to data, hopefully ####
      
      fit <- fitted(m2) # predFor + betFor
      # for each elk, probability that she had the type of behavior she did
      summary(fit)
      # mean and median are >50%, but not by much (0.59 and 0.61, respectively)
      # does that mean this model predicts accurately about 60% of the time?
      hist(fit)
     
      pred <- function(eta, theta, cat = 1:(length(theta) + 1), inv.link = plogis) {
        Theta <- c(-1000, theta, 1000)
        sapply(cat, function(j) inv.link(Theta[j + 1] - eta) - inv.link(Theta[j] - eta))}
      # ok theta is intercept; eta is maybe random effect i think?
      # but you don't want herd effect; you want to predict across an "average" herd
      # eta = stdev of random effect * whichever percentile you're interested in
      
      # try with their code that predicts for average, 5th and 95th pctile
      # but just look at avg
      mat <- expand.grid(judge = qnorm(0.95) * c(-1, 0, 1) * c(fm2$ST$judge),
        contact = c(0, fm2$beta[2]),
        temp = c(0, fm2$beta[1]))
      
      
      
    ##### PREDICT WITH CLMM2 ####
      
      # compare results of clmm and clmm2
      t1 <- clmm2(behavO ~ predFor + betFor, random = Herd, Hess = TRUE, nAGQ = 10, data = moddat)
      summary(t1)
      t2 <- clmm(behavO ~ predFor + betFor + (1|Herd), Hess = TRUE, nAGQ = 10, data = moddat)
      summary(t2)
      # exactly the same, so far so good...
      
      # predicted probabilty of behav of that animal in an "average" herd
      test <- cbind(moddat, pred=predict(t1, newdata = moddat))
      View(test)
      
      # didn't you see something about it being able to predict per category?
      test2 <- cbind(moddat, pred=predict(t1, type = "class"))
      # this doesn't give behavior class but does give slightly diff numbers
      # and i don't understand why ot what it is, so sticking with regular predict for now
      
      # attempting to "manually" calculate non-cumulative probabilty of migrating 
      # based on forage-only model for an elk in an "average" herd
      plogis(t1$Theta[2] - t1$beta[1]) - plogis(t1$Theta[1] - t1$beta[1])
      # 0.00051128
      
      # and cumulative
      plogis(t1$Theta[2] - t1$beta[1])
      # bigger. but still miniscule. 0.0007854
      
      # ok i think i slightly understand
      # so the pred column in test represents the NON cumulative probabilty
      # it tells you for an elk in an average herd, under the conditions given by the data,
      # how likely the model would have said she was to exhibit the behavior you observed. so,
      summary(test$pred)
      hist(test$pred)
      # it looks like a bimodal distribution with a peak around 0.3
      # which is essentially random chance since there are 3 groups
      # and another in the 0.9-1.0 bin
      # which is more promising
      length(which(test$pred > 0.89))
      # 90% likely to have correctly predicted 79 of 297 elk, ew. Just over a quarter.
      
      # add another model
      t2 <- clmm2(behavO ~ predFor + betFor + Dens + betFor*Dens, random = Herd, Hess = TRUE, nAGQ = 10, data = moddat)
      test2 <- cbind(test, pred2=predict(t2, newdata = moddat))
      summary(test2$pred2)
      hist(test2$pred2)
      test2$bet2 = test2$pred2 - test2$pred
      
      # can i predict residency?
      # only cumulative, not non-cumulative, because it's the base (can't look at pr(cat-1)
      plogis(t1$Theta[0] - t1$beta[1])
      

      # oooh i just saw i could treat some predictors as nominal, let's try it with predfor
      t3 <- clmm2(behavO ~ betFor, nominal = ~ predFor, data = moddat, random = Herd)
      t1
      aictab(cand.set = list(t1, t3), modnames = c("ord", "partnom"))
      # meh, not better-supported by deltaAIC, and aictab doesn't work, and sim log-ls,
      # so stay out of the rabbithole
      
      test99 <- data.frame(predict(t1, moddat, type = "probs"))
      
    
      
        
  #### PREDICT, now with deltaFor! ####
     
      # base forage (for comparison with previous models' predictive power)
      t1 <- clmm2(behavO ~ predFor + deltaFor, random = Herd, Hess = TRUE, nAGQ = 10, data = moddat)
      summary(t1)
        
      # and base with ag intrxn (best-supported)  
      t2 <- clmm2(behavO ~ predFor + deltaFor + ppnAg + (deltaFor+ppnAg), random = Herd, Hess = TRUE, nAGQ = 10, data = moddat)
      summary(t2)
      
      # predict with each and store
      testdelta1 <- cbind(moddat, pred=predict(t1, newdata = moddat))
      testdelta1 <- rename(testdelta1, predFMH = pred)
      testdelt <- cbind(testdelta1, pred=predict(t2, newdata = moddat))
      testdelt <- testdelt %>%
        rename(predTop = pred) %>%
        mutate(predDiff = predTop - predFMH)
      
      # compare to previous and hope for the best
      par(mfrow = c(2, 1))
      hist(test2$pred)
      hist(testdelt$predFMH)
      
      summary(test2$pred)
      summary(testdelt$predFMH)
      # ok so pretty similar, but updated models are a little better i think
      # slightly higher minimum and maximum probability
      # higher median but lower mean which means (heh) there are some worse ones in the distn pulling down the mean
      # but a more appropriate measure of center/spread of the data indicates higher mean value
      
      
  #### checking out univariate for hypothesis comparison? ####
      
      mdens <- clmm2(behavO ~ Dens, random = Herd, Hess = TRUE, nAGQ = 10, data = moddat)
      mold <- clmm2(behavO ~ Old, random = Herd, Hess = TRUE, nAGQ = 10, data = moddat)
      mhum <- clmm2(behavO ~ densOwn, random = Herd, Hess = TRUE, nAGQ = 10, data = moddat)
      mag <- clmm2(behavO ~ irrig, random = Herd, Hess = TRUE, nAGQ = 10, data = moddat)
      
      summary(mdens)
      # Hessian number is < 10e4 so this does not indicate the model is ill-defined
      confint(mdens, level = 0.95)
      # this actually gives different results than the model summary??
      # oh, that's mostly because it exp()s the coeffs for you, how sweet
      # ok anyway...
      
      confint(mdens, level = 0.95) # >0
      confint(mold, level = 0.95) # >0
      confint(mhum, level = 0.95) # >0... wait this is getting suspicious
      # dur... if it's exp()ed it needs to not overlap 1, not 0. because BASIC MATH.
      
      summary(mold)
      summary(mhum)
      # i'm not understanding what confint is giving me; it seems very diff from what's in models
      # using model summary for quick glance for now
      
      summary(mdens) # 0.0707 +- 0.0467 se
      summary(mold) # 0.7157 +- 0.5349 se
      summary(mhum) # -3.8084 +- 3.3773 se
      summary(mag) # -0.2696 +- 0.3248
      

      
  #### model averaging ####
      
      ?modavg
      candset <- list(ftw$Modnames)
      # may need to tweak nobs due to random group effect
      test <- modavg(cand.set = candset)
      # newp, not for this object class
      
      #install.packages("MuMIn")
      library(MuMIn)
      ?model.avg
      # requires clmm, not clmm2
      
      ftw$Modnames
       
      model.avg(print(noquote(candset)))
      candset[[1]][1]
      for (i in 1:nrow(ftw)) { }
      model.avg(ftw$Modnames)
      # fuck it
      
      test <- model.avg(m19, m2, m6, m3, m22, m11, m4) # removed m15 bc "common pitfall"
      summary(test)
      confint(test, level = 0.95)
      confint(test, level = 0.90)
      confint(test, level = 0.85)
      confint(test, level = 0.80)
      
      # when an interaction term is significant but the main effect is not,
      # you interpret that as showing there's no overall effect of the "non
      # significant" term but there is a crossover interaction. That is, the
      # effect of one variable on the response is opposite depending on the 
      # value of the other variable.
      
      summary(test)
      
            
      ### model averaging ###
      
        ftw$Modnames
  
        # pathetically doing this manually due to time constraints...
        topmod <- m19
        avgmod <- model.avg(m19, m2, m6, m3, m22, m11, m4) # removed m15 bc "common pitfall"
        summary(avgmod)
        
        confint(avgmod, level = 0.95)
        confint(avgmod, level = 0.90)
        confint(avgmod, level = 0.85)
        confint(avgmod, level = 0.80)   
        
      
      
  #### competing same models with pr(mig) as response, out of curiosity ####
      
      library(lme4)
      
      testdat <- dat %>%
        mutate(Mig = ifelse(Behav == "migrant", 1, 0)) 

      
      t1 <- glmer(Mig ~ deltaFor + (1|Herd), family = binomial(link = logit), dat = testdat)
      t2 <- glmer(Mig ~ predFor + deltaFor + (1|Herd), family = binomial(link = logit), dat = testdat)
      t3 <- glmer(Mig ~ predFor + deltaFor + (deltaFor*predFor) + (1|Herd), family = binomial(link = logit), dat = testdat)
      t4 <- glmer(Mig ~ predFor + deltaFor + Dens + (1|Herd), family = binomial(link = logit), dat = testdat) #warn
      t5 <- glmer(Mig ~ predFor + deltaFor + Dens + (deltaFor*Dens) + (1|Herd), family = binomial(link = logit), dat = testdat) #warn
      t6 <- glmer(Mig ~ predFor + deltaFor + Old + (1|Herd), family = binomial(link = logit), dat = testdat)
      t7 <- glmer(Mig ~ predFor + deltaFor + Old + (deltaFor*Old) + (1|Herd), family = binomial(link = logit), dat = testdat) #warn
      t8 <- glmer(Mig ~ predFor + deltaFor + Old + Dens + (1|Herd), family = binomial(link = logit), dat = testdat) #warn
      t9 <- glmer(Mig ~ predFor + deltaFor + Old + Dens + (Old*Dens) + (1|Herd), family = binomial(link = logit), dat = testdat) #warn
      t10 <- glmer(Mig ~ densOwn + (1|Herd), family = binomial(link = logit), dat = testdat)
      t11 <- glmer(Mig ~ predFor + deltaFor + densOwn + (1|Herd), family = binomial(link = logit), dat = testdat)
      t12 <- glmer(Mig ~ predFor + deltaFor + densOwn + (deltaFor*predFor) + (1|Herd), family = binomial(link = logit), dat = testdat) #warn
      t13 <- glmer(Mig ~ predFor + deltaFor + densOwn + (deltaFor*densOwn) + (1|Herd), family = binomial(link = logit), dat = testdat)
      t14 <- glmer(Mig ~ densOwn + Old + (densOwn*Old) + (1|Herd), family = binomial(link = logit), dat = testdat)
      t15 <- glmer(Mig ~ predFor + deltaFor + densOwn + Old + (densOwn*Old) + (1|Herd), family = binomial(link = logit), dat = testdat)
      t16 <- glmer(Mig ~ irrig + (1|Herd), family = binomial(link = logit), dat = testdat)
      t17 <- glmer(Mig ~ densOwn + irrig + (1|Herd), family = binomial(link = logit), dat = testdat)
      t18 <- glmer(Mig ~ predFor + deltaFor + irrig   + (1|Herd), family = binomial(link = logit), dat = testdat)
      t19 <- glmer(Mig ~ predFor + deltaFor + irrig + (deltaFor*irrig) + (1|Herd), family = binomial(link = logit), dat = testdat)  
      t20 <- glmer(Mig ~ predFor + deltaFor + irrig + (predFor*irrig) + (1|Herd), family = binomial(link = logit), dat = testdat)
      t21 <- glmer(Mig ~ irrig + Dens + (irrig*Dens) + (1|Herd), family = binomial(link = logit), dat = testdat)
      t22 <- glmer(Mig ~ predFor + irrig + Dens + (irrig*Dens) + (1|Herd), family = binomial(link = logit), dat = testdat)  #warn
      t23 <- glmer(Mig ~ predFor + irrig + Old + Dens + (Old*Dens) + (1|Herd), family = binomial(link = logit), dat = testdat)  
      t24 <- glmer(Mig ~ densOwn + irrig + Dens + (irrig*Dens) + (1|Herd), family = binomial(link = logit), dat = testdat)
      t25 <- glmer(Mig ~ densOwn + irrig + Dens + (densOwn*Dens) + (1|Herd), family = binomial(link = logit), dat = testdat) 
      t26 <- glmer(Mig ~ predFor + irrig + densOwn + (irrig*densOwn) + (1|Herd), family = binomial(link = logit), dat = testdat) 

      # compete with AICc #
      tmods <- list()
      tmodnms <- paste0("t", rep(1:26))
      for (i in 1:length(tmodnms)) { tmods[[i]] <- get(tmodnms[[i]]) }
      aictab(cand.set = tmods, modnames = tmodnms)
      aictab.prMig <- data.frame(aictab(cand.set = mods, modnames = modnms))

      
      
      # found support for essentially the same models
      # trying pr(int) for kicks
      
      testdat <- moddat %>%
        mutate(Mig = ifelse(Behav == "migrant", 1, 0),
          Int = ifelse(Behav == "other", 1, 0)) 

      
      t1 <- glmer(Int ~ deltaFor + (1|Herd), family = binomial(link = logit), dat = testdat)
      t2 <- glmer(Int ~ predFor + deltaFor + (1|Herd), family = binomial(link = logit), dat = testdat)
      t3 <- glmer(Int ~ predFor + deltaFor + (deltaFor*predFor) + (1|Herd), family = binomial(link = logit), dat = testdat)
      t4 <- glmer(Int ~ predFor + deltaFor + Dens + (1|Herd), family = binomial(link = logit), dat = testdat) #warn
      t5 <- glmer(Int ~ predFor + deltaFor + Dens + (deltaFor*Dens) + (1|Herd), family = binomial(link = logit), dat = testdat) #warn
      t6 <- glmer(Int ~ predFor + deltaFor + Old + (1|Herd), family = binomial(link = logit), dat = testdat)
      t7 <- glmer(Int ~ predFor + deltaFor + Old + (deltaFor*Old) + (1|Herd), family = binomial(link = logit), dat = testdat) #warn
      t8 <- glmer(Int ~ predFor + deltaFor + Old + Dens + (1|Herd), family = binomial(link = logit), dat = testdat) #warn
      t9 <- glmer(Int ~ predFor + deltaFor + Old + Dens + (Old*Dens) + (1|Herd), family = binomial(link = logit), dat = testdat) #warn
      t10 <- glmer(Int ~ densOwn + (1|Herd), family = binomial(link = logit), dat = testdat)
      t11 <- glmer(Int ~ predFor + deltaFor + densOwn + (1|Herd), family = binomial(link = logit), dat = testdat)
      t12 <- glmer(Int ~ predFor + deltaFor + densOwn + (deltaFor*predFor) + (1|Herd), family = binomial(link = logit), dat = testdat) #warn
      t13 <- glmer(Int ~ predFor + deltaFor + densOwn + (deltaFor*densOwn) + (1|Herd), family = binomial(link = logit), dat = testdat)
      t14 <- glmer(Int ~ densOwn + Old + (densOwn*Old) + (1|Herd), family = binomial(link = logit), dat = testdat)
      t15 <- glmer(Int ~ predFor + deltaFor + densOwn + Old + (densOwn*Old) + (1|Herd), family = binomial(link = logit), dat = testdat)
      t16 <- glmer(Int ~ irrig + (1|Herd), family = binomial(link = logit), dat = testdat)
      t17 <- glmer(Int ~ densOwn + irrig + (1|Herd), family = binomial(link = logit), dat = testdat)
      t18 <- glmer(Int ~ predFor + deltaFor + irrig   + (1|Herd), family = binomial(link = logit), dat = testdat)
      t19 <- glmer(Int ~ predFor + deltaFor + irrig + (deltaFor*irrig) + (1|Herd), family = binomial(link = logit), dat = testdat)  
      t20 <- glmer(Int ~ predFor + deltaFor + irrig + (predFor*irrig) + (1|Herd), family = binomial(link = logit), dat = testdat)
      t21 <- glmer(Int ~ irrig + Dens + (irrig*Dens) + (1|Herd), family = binomial(link = logit), dat = testdat)
      t22 <- glmer(Int ~ predFor + irrig + Dens + (irrig*Dens) + (1|Herd), family = binomial(link = logit), dat = testdat)  #warn
      t23 <- glmer(Int ~ predFor + irrig + Old + Dens + (Old*Dens) + (1|Herd), family = binomial(link = logit), dat = testdat)  
      t24 <- glmer(Int ~ densOwn + irrig + Dens + (irrig*Dens) + (1|Herd), family = binomial(link = logit), dat = testdat)
      t25 <- glmer(Int ~ densOwn + irrig + Dens + (densOwn*Dens) + (1|Herd), family = binomial(link = logit), dat = testdat) 
      t26 <- glmer(Int ~ predFor + irrig + densOwn + (irrig*densOwn) + (1|Herd), family = binomial(link = logit), dat = testdat) 
      
      
      
      
      # compete with AICc #
      mods <- list()
      modnms <- paste0("t", rep(1:26))
      for (i in 1:length(modnms)) { mods[[i]] <- get(modnms[[i]]) }
      aictab(cand.set = mods, modnames = modnms)
      aictab <- data.frame(aictab(cand.set = mods, modnames = modnms))
      
      
      
#### EFFECTS PER HERD ####
      
      ## plot effects per herd ##
      
      # effects
      
      # this is basically doing center +- spread for the variance per herd
      ci <- m19$ranef + qnorm(0.975) * sqrt(m19$condVar) %o% c(-1, 1) # %o% = genius
      # order effect from low to high
      ord.re <- order(m19$ranef)
      ci <- ci[order(m19$ranef),]
      # and order herds to match their index numbers
      herdnums <- data.frame(Herd = unique(dat$Herd), herdNum = 1:16)
      herdnums <- herdnums[match(ord.re, herdnums$herdNum),]
      #plot estimates
      plot(1:16, m19$ranef[ord.re], axes = FALSE, ylim = range(ci),
        xlab = "", ylab = "Herd effect")
      axis(1, at = 1:16, labels = herdnums$Herd, las=2) #las=2 rotates vert
      axis(2)
      # add 95% CIs
      for(i in 1:16) segments(i, ci[i, 1], i, ci[i, 2])
      abline(h = 0, lty = 2)
      # 4 don't overlap 0; guess those would be considered the "extreme" herds
         # pio, sage ck, blacktail (neg) and silver run (pos)
      
      # alright i know this is lame, but... manually removing extreme for now
      
      extreme <- data.frame(Herd = c("Sage Creek", "Pioneers", "Blacktail", "Silver Run"))
      moddatreg <- moddat %>%
        anti_join(extreme, by = "Herd") %>%
        droplevels()
      str(moddatreg)
      
      
      
      ## this line was wrong from my attempt to manually plot these in a prettier way ##
      herdnums <- data.frame(Herd = unique(topmod$gfList$Herd), herdNum = 1:16) # maybe match herd to correct index #?
      # bc needed to pull factor levels from the dataframe i fed into the model
      # not artificially label factor levels based on the order shown in the model output
      
      # and something about the below is a little jacked as well
      
      # match each herd to correct index number and order by herd effect
      ord.re <- order(topmod$ranef) # order random effect estimates (small to large)
      herdnums <- data.frame(Herd = attributes(dat$Herd)$levels, herdNum = 1:16) # id herd factor levels
      herdnums <- herdnums[match(ord.re, herdnums$herdNum), ] # order herds by random effect
            
      
#### rerunning model selection and averaging without "extreme" herds ####    
      
            
      # define each model
      t1 <- clmm(behavO ~ deltaFor + (1|Herd), dat = moddatreg)
      t2 <- clmm(behavO ~ predFor + deltaFor + (1|Herd), dat = moddatreg)
      t3 <- clmm(behavO ~ predFor + deltaFor + (deltaFor*predFor) + (1|Herd), dat = moddatreg)
      t4 <- clmm(behavO ~ predFor + deltaFor + Dens + (1|Herd), dat = moddatreg)
      t5 <- clmm(behavO ~ predFor + deltaFor + Dens + (deltaFor*Dens) + (1|Herd), dat = moddatreg)
      t6 <- clmm(behavO ~ predFor + deltaFor + Old + (1|Herd), dat = moddatreg)
      t7 <- clmm(behavO ~ predFor + deltaFor + Old + (deltaFor*Old) + (1|Herd), dat = moddatreg)
      t8 <- clmm(behavO ~ predFor + deltaFor + Old + Dens + (1|Herd), dat = moddatreg)
      t9 <- clmm(behavO ~ predFor + deltaFor + Old + Dens + (Old*Dens) + (1|Herd), dat = moddatreg)
      t10 <- clmm(behavO ~ densOwn + (1|Herd), dat = moddatreg)
      t11 <- clmm(behavO ~ predFor + deltaFor + densOwn + (1|Herd), dat = moddatreg)
      t12 <- clmm(behavO ~ predFor + deltaFor + densOwn + (deltaFor*predFor) + (1|Herd), dat = moddatreg)
      t13 <- clmm(behavO ~ predFor + deltaFor + densOwn + (deltaFor*densOwn) + (1|Herd), dat = moddatreg)
      t14 <- clmm(behavO ~ densOwn + Old + (densOwn*Old) + (1|Herd), dat = moddatreg)
      t15 <- clmm(behavO ~ predFor + deltaFor + densOwn + Old + (densOwn*Old) + (1|Herd), dat = moddatreg)
      t16 <- clmm(behavO ~ irrig + (1|Herd), dat = moddatreg)
      t17 <- clmm(behavO ~ densOwn + irrig + (1|Herd), dat = moddatreg)
      t18 <- clmm(behavO ~ predFor + deltaFor + irrig   + (1|Herd), dat = moddatreg)
      t19 <- clmm(behavO ~ predFor + deltaFor + irrig + (deltaFor*irrig) + (1|Herd), dat = moddatreg)  
      t20 <- clmm(behavO ~ predFor + deltaFor + irrig + (predFor*irrig) + (1|Herd), dat = moddatreg)
      t21 <- clmm(behavO ~ irrig + Dens + (irrig*Dens) + (1|Herd), dat = moddatreg)
      t22 <- clmm(behavO ~ predFor + irrig + Dens + (irrig*Dens) + (1|Herd), dat = moddatreg)  
      t23 <- clmm(behavO ~ predFor + irrig + Old + Dens + (Old*Dens) + (1|Herd), dat = moddatreg)  
      t24 <- clmm(behavO ~ densOwn + irrig + Dens + (irrig*Dens) + (1|Herd), dat = moddatreg)
      t25 <- clmm(behavO ~ densOwn + irrig + Dens + (densOwn*Dens) + (1|Herd), dat = moddatreg) 
      t26 <- clmm(behavO ~ predFor + irrig + densOwn + (irrig*densOwn) + (1|Herd), dat = moddatreg) 
      
      
      # compete with AICc #
      mods <- list()
      modnms <- paste0("t", rep(1:26))
      for (i in 1:length(modnms)) { mods[[i]] <- get(modnms[[i]]) }
      aictab(cand.set = mods, modnames = modnms)
      aictab.noext <- data.frame(aictab(cand.set = mods, modnames = modnms))
      
      # identify prelim supported models
      ftw.noext <- subset(aictab.noext, Delta_AICc < 2.1); ftw.noext <- droplevels(ftw.noext)
      ftw.noext$Modnames = as.character(ftw.noext$Modnames)
      
      # average prelim supported models
      ftw.noext$Modnames
      test <- model.avg(t19, t2, t22, t20, t6, t4, t11)
      summary(test)
      confint(test)
      confint(test, level = 0.95)
      confint(test, level = 0.90)
      confint(test, level = 0.80)


      
#### rerunning model selection and averaging just without silver run ####    
      
      moddat2 <- filter(moddat, Herd != "Silver Run")
            
      # define each model
      t1 <- clmm(behavO ~ deltaFor + (1|Herd), dat = moddat2)
      t2 <- clmm(behavO ~ predFor + deltaFor + (1|Herd), dat = moddat2)
      t3 <- clmm(behavO ~ predFor + deltaFor + (deltaFor*predFor) + (1|Herd), dat = moddat2)
      t4 <- clmm(behavO ~ predFor + deltaFor + Dens + (1|Herd), dat = moddat2)
      t5 <- clmm(behavO ~ predFor + deltaFor + Dens + (deltaFor*Dens) + (1|Herd), dat = moddat2)
      t6 <- clmm(behavO ~ predFor + deltaFor + Old + (1|Herd), dat = moddat2)
      t7 <- clmm(behavO ~ predFor + deltaFor + Old + (deltaFor*Old) + (1|Herd), dat = moddat2)
      t8 <- clmm(behavO ~ predFor + deltaFor + Old + Dens + (1|Herd), dat = moddat2)
      t9 <- clmm(behavO ~ predFor + deltaFor + Old + Dens + (Old*Dens) + (1|Herd), dat = moddat2)
      t10 <- clmm(behavO ~ densOwn + (1|Herd), dat = moddat2)
      t11 <- clmm(behavO ~ predFor + deltaFor + densOwn + (1|Herd), dat = moddat2)
      t12 <- clmm(behavO ~ predFor + deltaFor + densOwn + (deltaFor*predFor) + (1|Herd), dat = moddat2)
      t13 <- clmm(behavO ~ predFor + deltaFor + densOwn + (deltaFor*densOwn) + (1|Herd), dat = moddat2)
      t14 <- clmm(behavO ~ densOwn + Old + (densOwn*Old) + (1|Herd), dat = moddat2)
      t15 <- clmm(behavO ~ predFor + deltaFor + densOwn + Old + (densOwn*Old) + (1|Herd), dat = moddat2)
      t16 <- clmm(behavO ~ irrig + (1|Herd), dat = moddat2)
      t17 <- clmm(behavO ~ densOwn + irrig + (1|Herd), dat = moddat2)
      t18 <- clmm(behavO ~ predFor + deltaFor + irrig   + (1|Herd), dat = moddat2)
      t19 <- clmm(behavO ~ predFor + deltaFor + irrig + (deltaFor*irrig) + (1|Herd), dat = moddat2)  
      t20 <- clmm(behavO ~ predFor + deltaFor + irrig + (predFor*irrig) + (1|Herd), dat = moddat2)
      t21 <- clmm(behavO ~ irrig + Dens + (irrig*Dens) + (1|Herd), dat = moddat2)
      t22 <- clmm(behavO ~ predFor + irrig + Dens + (irrig*Dens) + (1|Herd), dat = moddat2)  
      t23 <- clmm(behavO ~ predFor + irrig + Old + Dens + (Old*Dens) + (1|Herd), dat = moddat2)  
      t24 <- clmm(behavO ~ densOwn + irrig + Dens + (irrig*Dens) + (1|Herd), dat = moddat2)
      t25 <- clmm(behavO ~ densOwn + irrig + Dens + (densOwn*Dens) + (1|Herd), dat = moddat2) 
      t26 <- clmm(behavO ~ predFor + irrig + densOwn + (irrig*densOwn) + (1|Herd), dat = moddat2) 
      
      
      # compete with AICc #
      tmods <- list()
      tmodnms <- paste0("t", rep(1:26))
      for (i in 1:length(tmodnms)) { tmods[[i]] <- get(tmodnms[[i]]) }
      aictab(cand.set = mods, modnames = modnms)
      aictab.nosil <- data.frame(aictab(cand.set = mods, modnames = modnms))
      
      # identify prelim supported models
      ftw.nosil <- subset(aictab.nosil, Delta_AICc < 4.0); ftw.nosil <- droplevels(ftw.nosil)
      ftw.nosil$Modnames = as.character(ftw.nosil$Modnames)
      
      # average prelim supported models
      ftw.nosil$Modnames
      test <- model.avg(t19, t2, t6, t3, t22, t11, t4)
      summary(test)
      confint(test) 
      confint(test, level = 0.90)
      confint(test, level = 0.85)
      confint(test, level = 0.80)

      # inference is the same,
      # no compelling reason to drop them
      

  
#### Density high/med/low ####
      
      str(summary(log(moddat$Dens)))
      summary(log(moddat$Dens))[["1st Qu."]]
      
      testdat <- moddat %>%
        mutate(densCat = as.factor(ifelse(log(moddat$Dens) <= summary(log(moddat$Dens))[["1st Qu."]],
          "low", ifelse(log(moddat$Dens) >= summary(log(moddat$Dens))[["3rd Qu."]],
            "high", "med")))) %>%
        mutate(densCat = factor(densCat, levels = c("low", "med", "high"), ordered = FALSE))
      str(testdat)
      
      
            
      # define each model
      t1 <- clmm(behavO ~ deltaFor + (1|Herd), dat = testdat)
      t2 <- clmm(behavO ~ predFor + deltaFor + (1|Herd), dat = testdat)
      t3 <- clmm(behavO ~ predFor + deltaFor + (deltaFor*predFor) + (1|Herd), dat = testdat)
      t4 <- clmm(behavO ~ predFor + deltaFor + densCat + (1|Herd), dat = testdat)
      t5 <- clmm(behavO ~ predFor + deltaFor + densCat + (deltaFor*densCat) + (1|Herd), dat = testdat)
      t6 <- clmm(behavO ~ predFor + deltaFor + Old + (1|Herd), dat = testdat)
      t7 <- clmm(behavO ~ predFor + deltaFor + Old + (deltaFor*Old) + (1|Herd), dat = testdat)
      t8 <- clmm(behavO ~ predFor + deltaFor + Old + densCat + (1|Herd), dat = testdat)
      t9 <- clmm(behavO ~ predFor + deltaFor + Old + densCat + (Old*densCat) + (1|Herd), dat = testdat)
      t10 <- clmm(behavO ~ densOwn + (1|Herd), dat = testdat)
      t11 <- clmm(behavO ~ predFor + deltaFor + densOwn + (1|Herd), dat = testdat)
      t12 <- clmm(behavO ~ predFor + deltaFor + densOwn + (deltaFor*predFor) + (1|Herd), dat = testdat)
      t13 <- clmm(behavO ~ predFor + deltaFor + densOwn + (deltaFor*densOwn) + (1|Herd), dat = testdat)
      t14 <- clmm(behavO ~ densOwn + Old + (densOwn*Old) + (1|Herd), dat = testdat)
      t15 <- clmm(behavO ~ predFor + deltaFor + densOwn + Old + (densOwn*Old) + (1|Herd), dat = testdat)
      t16 <- clmm(behavO ~ irrig + (1|Herd), dat = testdat)
      t17 <- clmm(behavO ~ densOwn + irrig + (1|Herd), dat = testdat)
      t18 <- clmm(behavO ~ predFor + deltaFor + irrig   + (1|Herd), dat = testdat)
      t19 <- clmm(behavO ~ predFor + deltaFor + irrig + (deltaFor*irrig) + (1|Herd), dat = testdat)  
      t20 <- clmm(behavO ~ predFor + deltaFor + irrig + (predFor*irrig) + (1|Herd), dat = testdat)
      t21 <- clmm(behavO ~ irrig + densCat + (irrig*densCat) + (1|Herd), dat = testdat)
      t22 <- clmm(behavO ~ predFor + irrig + densCat + (irrig*densCat) + (1|Herd), dat = testdat)  
      t23 <- clmm(behavO ~ predFor + irrig + Old + densCat + (Old*densCat) + (1|Herd), dat = testdat)  
      t24 <- clmm(behavO ~ densOwn + irrig + densCat + (irrig*densCat) + (1|Herd), dat = testdat)
      t25 <- clmm(behavO ~ densOwn + irrig + densCat + (densOwn*densCat) + (1|Herd), dat = testdat) 
      t26 <- clmm(behavO ~ predFor + irrig + densOwn + (irrig*densOwn) + (1|Herd), dat = testdat) 
      
      
      # compete with AICc #
      mods <- list()
      modnms <- paste0("t", rep(1:26))
      for (i in 1:length(modnms)) { mods[[i]] <- get(modnms[[i]]) }
      aictab(cand.set = mods, modnames = modnms)
      aictab.denscat <- data.frame(aictab(cand.set = mods, modnames = modnms))
      
      # identify prelim supported models
      ftw.denscat <- subset(aictab.denscat, Delta_AICc < 4.0); ftw.denscat <- droplevels(ftw.denscat)
      ftw.denscat$Modnames = as.character(ftw.denscat$Modnames)
      
      # average prelim supported models
      ftw.denscat$Modnames
      test <- model.avg(t19, t2, t6, t5, t3, t11, t15)
      summary(test)
      confint(test) 
      confint(test, level = 0.90)
      confint(test, level = 0.85)
      confint(test, level = 0.80)

      
#### MANUAL CI CALCS ####      
      
      
      # identify top-supported models #
      ftw <- subset(aictab, Delta_AICc < 2.0); ftw <- droplevels(ftw)
      ftw$Modnames = as.character(ftw$Modnames)
      
      # create dataframe to store results in #
      allmods <- data.frame(mod = NA, cov = NA, coeff = NA, dirEffect = NA, spt95 = NA, spt90 = NA)
      
      # extract coeffs and ses from top-supported models #
      for (i in 1:nrow(ftw)) {
        # for each supported model,
        mod <- get(ftw[i, "Modnames"])
        # create dataframe of coefficient estimates and standard errors 
        modsum <- data.frame(coef(summary(mod))) %>%
          # create column of coefficient names (stored as row names by default)
          tibble::rownames_to_column() %>%
          # standardize column names and add which model the estimate came from
          rename(cov = rowname, coeff = Estimate, se = "Std..Error") %>%
          mutate(mod = ftw[i, "Modnames"]) %>%
          # only look at coefficient estimates for now (not thresholds)
          filter(!grepl("other", cov)) %>%
          # calculate 95% and 90% CIs
          mutate(CI95low = coeff - (1.96*se), CI95high = coeff + (1.96*se),
                 CI90low = coeff - (1.645*se), CI90high = coeff + (1.645*se)) %>%
          # determine whether covariate had "significant" effect and in which direction
          mutate(spt95 = ifelse(CI95low < 0 & CI95high > 0, 0, 1),
                 spt90 = ifelse(CI90low < 0 & CI90high > 0, 0, 1)) %>%
          mutate(dirEffect = ifelse(coeff > 0, "pos", "neg")) %>%
          # only keep columns of interest
          dplyr::select(mod, cov, coeff, dirEffect, spt95, spt90)
        
        # store
        allmods <- bind_rows(allmods, modsum)
       
      }

      
      

#### PRELIM MODEL EXPLORATION ####   
      
         # create dataframe to store results in #
      allmods <- data.frame(mod = NA, cov = NA, spt80 = NA, spt85 = NA, spt90 = NA, 
        CI95low = NA, CI95high = NA, spt95 = NA)
      
      # extract coeffs and ses from top-supported models #
      for (i in 1:nrow(ftw)) {
        # for each supported model,
        mod <- get(ftw[i, "Modnames"])
        
        # create dataframe of 95% CI 
        modsum95 <- data.frame(confint(mod)) %>%
          # create column of coefficient names (stored as row names by default)
          tibble::rownames_to_column() %>%
          # standardize column names and add which model the estimate came from
          rename(cov = rowname, CI95low = X2.5.., CI95high = X97.5..) %>%
          mutate(mod = ftw[i, "Modnames"]) %>%
          # only look at coefficient estimates for now (not thresholds)
          filter(!grepl("other", cov)) %>%
          mutate(spt95 = ifelse(CI95low < 0 & CI95high > 0, 0, 1)) 
        
        # does dropping to 90% CI change inference?
        modsum90 <- data.frame(confint(mod, level = 0.90)) %>%
          # create column of coefficient names (stored as row names by default)
          tibble::rownames_to_column() %>%
          # standardize column names and add which model the estimate came from
          rename(cov = rowname, CI90low = X5.., CI90high = X95..) %>%
          mutate(mod = ftw[i, "Modnames"]) %>%
          # only look at coefficient estimates for now (not thresholds)
          filter(!grepl("other", cov)) %>%
          mutate(spt90 = ifelse(CI90low < 0 & CI90high > 0, 0, 1)) %>%
          dplyr::select(mod, cov, spt90) %>%
          left_join(modsum95, by = c("mod", "cov"))
        
        # 85% CI?
        modsum85 <- data.frame(confint(mod, level = 0.85)) %>%
          # create column of coefficient names (stored as row names by default)
          tibble::rownames_to_column() %>%
          # standardize column names and add which model the estimate came from
          rename(cov = rowname, CI85low = X7.5.., CI85high = X92.5..) %>%
          mutate(mod = ftw[i, "Modnames"]) %>%
          # only look at coefficient estimates for now (not thresholds)
          filter(!grepl("other", cov)) %>%
          mutate(spt85 = ifelse(CI85low < 0 & CI85high > 0, 0, 1))  %>%
          dplyr::select(mod, cov, spt85) %>%
          left_join(modsum90, by = c("mod", "cov"))
        
        # ... and 80% CI?
        modsum <- data.frame(confint(mod, level = 0.80)) %>%
          # create column of coefficient names (stored as row names by default)
          tibble::rownames_to_column() %>%
          # standardize column names and add which model the estimate came from
          rename(cov = rowname, CI80low = X10.., CI80high = X90..) %>%
          mutate(mod = ftw[i, "Modnames"]) %>%
          # only look at coefficient estimates for now (not thresholds)
          filter(!grepl("other", cov)) %>%
          mutate(spt80 = ifelse(CI80low < 0 & CI80high > 0, 0, 1))  %>%
          dplyr::select(mod, cov, spt80) %>%
          left_join(modsum85, by = c("mod", "cov"))
        
        
        
        # store
        allmods <- bind_rows(allmods, modsum)
       
      }

      # take prelim look and play with things
      allmods <- filter(allmods, !is.na(mod))
      write.csv(allmods, "topmods-deltaFor.csv", row.names = F)
         
      
            
    #### assessing possible deltaFor covariates (choose 1 as baseline for other models that use forage) #### 
      
      # create new binary covariate for whether forage is better outside winter range or not
      moddat$betFor = as.factor(ifelse(moddat$deltaFor > 0, 1, 0))
      
      # define models
      for1 <- clmm(behavO ~ predFor + deltaFor + (1|Herd), data = moddat)
      for2 <- clmm(behavO ~ predFor + deltaFor + predFor*deltaFor + (1|Herd), data = moddat)
      for3 <- clmm(behavO ~ predFor + betFor + (1|Herd), data = moddat)
      for4 <- clmm(behavO ~ predFor + betFor + predFor*betFor + (1|Herd), data = moddat)

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
      
      
      
#### bootstrapping to predict ####
      
      # use full dat, then subset NA ages for age model i guess
      
      test <- sample(dat, size = 10, replace = TRUE) # samples 10 *columns*
      test <- dat[sample(nrow(dat), 10), ] # this is what i expected
      test <- dat[sample(nrow(dat), 1000, replace = TRUE), ] # mexcellent, now can you specify rows?
      
      test <- dat[sample(nrow(dat), 1000, replace = TRUE), dat$predFor] # newp
      head(dat) # yeah maybe if you actually had created that column it would work...
      test <- dat[sample(nrow(dat), 1000, replace = TRUE), dat$predFor] # still newp
      test <- dat[sample(nrow(dat), 1000, replace = TRUE), "predFor"] # oh.
      
      
#### mo bettah predictions (now with behavioral categories) ####
      
      #         # predict using sampled data #
        pred.top <- cbind(simdat, predict(topmod2, newdata = simdat, type = "probs"))
        
        test <- predict(topmod2)
        test <- predict(topmod2, type = "probs")
        
        ## muddling through rdocumentation for predict.clm2 ####
        
          # number of levels of response, i assume?
          nlev <- 3
          
          # generate factor levels for response
          y <- gl(nlev, 333)
          
          # try with one covariate for now
          x <- simdat$predFor
          
          # tutorial uses clm, rolling with it for now
          # illustrating that it can do the same thins as polr predict()
          fm.clm <- clm2(y ~ x)
          fm.polr <- MASS::polr(y ~ x)
          
          # returns df with #rows = 3Xnumber response categories
          # one column it the factor; the other is the fake covariate data
          # i believe it may be repeating each value of x for each value of y
          ndat <- expand.grid(y = gl(nlev, 1), x = x)
          
          # to predict probs of falling into each behav type
          pmat.clm <- matrix(predict(fm.clm, newdata = simdat), ncol = nlev,
            byrow = TRUE)
          # ohh cool, so this generates the predicted probability
          # of the observation falling within each ofthe 3 response categories
          # based on whatever data you give it
          # because you replicated the same data for each response category

          # can i really just not specify newdata?
          pmat.clm2 <- matrix(predict(fm.clm), ncol = nlev,
            byrow = TRUE)
          View(pmat.clm2)
          # yes but results are the same for this toy example
          
          
          # to predict which category most likely to fall into
          class.clm <- factor(apply(pmat.clm, 1, which.max))

          
 #### transforming all the weirdos ####
          
    ?boxcox
    test <- dat$Dens
    test <- MASS::boxcox(test)
    test <- MASS::boxcox(mod)
    test <- car::box.cox(test)
    test <- VGAM::logit(dat$Dens)
    unique(test)
    
    
    
#### trying to understand centering factors etc for model averaging ####
    
    
    # working thru http://seananderson.ca/2014/07/30/centering-interactions.html
    
      set.seed(999)
      b0 <- 1.4 # intercept
      b1 <- 0.2 # continuous slope
      b2 <- 1.7 # factor level 1 coefficient
      b1.2 <- 0.5 # interaction between b1 and b2
      sigma <- 2.0 # residual standard deviation
      N <- 25 # number of data points
      
      x1 <- runif(N, 0, 20) # continuous predictor data
      x2 <- rbinom(N, size = 1, prob = 0.4) # binary predictor data
      
      # generate response data:
      y <- rnorm(N, mean = b0 +
        b1 * x1 +
        b2 * x2 +
        x1 * x2 * b1.2,
        sd = sigma)
      dat <- data.frame(x1, x2, y)
      head(dat)
        
      # compare models with and without interactions  
      m <- lm(y ~ x1 + x2 + x1 * x2, data = dat)
      m_no_inter <- lm(y ~ x1 + x2, data = dat)
      round(coef(m), 2)
      round(coef(m_no_inter), 2)
      
      # and do the same with my models
      round(coef(m19), 2) # deltafor negative here
      round(coef(m2), 2) # deltafor positive here
      # ok, with it so far...
      
      # now trying the centering trick with my data
      # to see whether it still makes sense for me to center with the mean
      # despite some covariates being skewed
      # i think it should work anyway because the reason for centering
      # is to account for the fact that in the non-interaction model
      # effects are predicted while holding the other covariates at their mean
      
      
      # center the covariates in the 2 models
      testdat <- dat %>%
        mutate(predForCtr = predFor - mean(predFor),
          deltaForCtr = deltaFor - mean(deltaFor),
          irrigCtr = as.numeric(irrig) - mean(as.numeric(irrig)))
      
      # define the 2 models using the centered data, simplistic for now (no hess etc)
      mInt <- clmm(behavO ~ predForCtr + deltaForCtr + deltaForCtr*predForCtr + (1|Herd), data = testdat)
      mNoint <- clmm(behavO ~ predForCtr + deltaForCtr + (1|Herd), data = testdat)

      # look at what the coefficients do when they aren't centered
      # m2 is no interaction, m3 is interaction (forage-only models)
      round(coef(m2), 2) # predfor = 1.79 | deltaFor = 0.01
      round(coef(m3), 2) # predfor = 1.55 | deltaFor = 0.24
      # ok yeah those are pretty different. 
      # and with the centered data...
      
      # look at what the coefficients do when they are centered
      round(coef(mNoint), 2) # predfor = 1.79 | deltafor = 0.00
      round(coef(mInt), 2) # predfor = 1.85 | dentrfor = 0.03
      # yep yep that helps. very cool.
      
      # ok now figure out the binary interaction thing -
      # are you seriously supposed to center that? 
      # i may not be wrapping my head around this fully
      
      # define 2 simple models with and without binary interaction
      t1 <- clmm(behavO ~ deltaFor + irrig + (1|Herd), data= testdat)
      t2 <- clmm(behavO ~ deltaFor + irrig + deltaFor*irrig + (1|Herd), data= testdat)
      
      # and same thing but with centered covariates, including the binary variable
      testdat$irrigCtr <- as.numeric(testdat$irrig) - mean(as.numeric(testdat$irrig))
      t1c <- clmm(behavO ~ deltaForCtr + irrigCtr + (1|Herd), data= testdat)
      t2c <- clmm(behavO ~ deltaForCtr + irrigCtr + deltaForCtr*irrigCtr + (1|Herd), data= testdat)
      
      # look at coeffs with non-centered covariates
      round(coef(t1), 2) # deltafor = 0.00 | irrig = -0.19
      round(coef(t2), 2) # deltafor = -0.04 | irrig = -0.88
      # yep that's a pretty fucking big difference
      
      # and with centered covariates
      round(coef(t1c), 2) # deltafor = 0.00 | irrig = -0.19
      round(coef(t2c), 2) # deltafor = 0.06 | irrig = -0.11
      # and a fuckload better
      
      
      ## sanity check - median doesn't do it better, does it?
      
          testdat <- testdat %>%
            mutate(predForMed = predFor - median(predFor),
              deltaForMed = deltaFor - median(deltaFor),
              irrigMed = as.numeric(irrig) - median(as.numeric(irrig)))
          
          # median-centered covariate models
          t1m <- clmm(behavO ~ deltaForMed + irrigMed + (1|Herd), data= testdat)
          t2m <- clmm(behavO ~ deltaForMed + irrigMed + deltaForMed*irrigMed + (1|Herd), data= testdat)
          
          # look at coeffs with non-centered covariates
          round(coef(t1m), 2) # deltafor = 0.00 | irrig = -0.19
          round(coef(t2m), 2) # deltafor = 0.13 | irrig = -0.54
          # confirmed
          
      
      
    ## TL;DR ##
      
      # if you just center your covariates you can still model average
      # across models with and without interactions.
      # there's no need to scale, and it's appropriate to center using means
      # even for skewed data (because the model is predicting things while
      # holding other effects at their means and that's what you're
      # trying to account for). center factors as well!
      
      
      

    #### continued failures at prediction plots ####
      
      
      #### pull 1000 random samples from data (may need to do differently later) ####
      
        
        # prep dataframe to store samples in
        simdat <- data.frame(matrix(nrow = 999, ncol = 6))
        covs <- c("predFor", "deltaFor", "irrig", "Dens", "Old", "densOwn")
        colnames(simdat) <- covs
        
        # fill dataframe with beautiful data
        simdat$predFor <- dat[sample(nrow(dat), 999, replace = TRUE), "predFor"] 
        simdat$deltaFor <- dat[sample(nrow(dat), 999, replace = TRUE), "deltaFor"] 
        simdat$irrig <- as.factor(rep(0:1, length.out = 999))
        simdat$Dens <- dat[sample(nrow(dat), 999, replace = TRUE), "Dens"] 
        simdat$Old <- as.factor(rep(0:1, length.out = 999))
        simdat$densOwn <- dat[sample(nrow(dat), 999, replace = TRUE), "densOwn"] 

      
      #### predict with top model ####
        
        
        # define model using clmm2 (predict doesn't work with clmm)
        topmod2 <- clmm2(behavO ~ predFor + deltaFor + irrig + (deltaFor*irrig), 
                         random = Herd, Hess = TRUE, nAGQ = 10, dat = dat) 
        summary(topmod2)
        
        
        # duplicate data for each response category
        dupedat <- expand.grid(y = unique(dat$behavO), 
          predFor = unique(dat$predFor),
          deltaFor = unique(dat$deltaFor),
          irrig = as.factor(c(0, 1)),
          Dens = unique(dat$Dens),
          Old = as.factor(c(0, 1)),
          densOwn = unique(dat$densOwn))
          # holy balls that's a lot of combinations
        
        # predict probability of falling into each response category
        pred.prob <- matrix(predict(topmod2), ncol = length(unique(dat$Behav)), byrow = TRUE)
        # WARNING: data length [308] is not a sub-multiple or multiple of number of rows [103]
          # predprob is what has 103 rows
        # this is definitely fucked; each row should sum to 1 but they don't at all.
        # or y'know what, maybe possibly not fucked after all?
        # because this is a cumulative probability, so if an animal's like almost definitely a resident
        # then she's have a shitload of probability in the res category
        # and then slightly more as she moves up thru to migrant maybe?
        # eh, seems not to be what's happening
        
        
        # what does the example do?
        test1 <- data.frame(pmat.clm) %>%
          mutate(sum = X1+X2+X3)
        # ok rows essentially sum to 1; they're just a teensy bit off (which makes sense)
      
      
      
          
### ### ### ### ### ### ### ### ### ### ### ### ### ##
#### | OLDER SUMMARIES ETC (from draft results) | ####
### ### ### ### ### ### ### ### ### ### ### ### ### ##         
              
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
       
       
       
  #### combining the 2 models that include density ####
  
    summary(olddat$Dens)

    # center density to allow model averaging
    # when one has an interaction term
    olddat <- mutate(olddat, ctrDens = Dens - median(Dens))
    
    # use centered density in both models
    ma22 <- clmm(behavO ~ predFor + irrig + ctrDens + (irrig*ctrDens) + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat) 
    ma4 <- clmm(behavO ~ predFor + deltaFor + ctrDens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
    
    # check 'em out
    summary(ma22)
    confint(ma22, level = 0.85)
    summary(ma4)
    confint(ma4, level = 0.85)
    
    # average 'em
    densavg <- model.avg(ma22, ma4) 
    summary(densavg)
    confint(densavg, level = 0.85)
    
    
#### PREDICTION PLOTS - GLOBAL MODEL (you can't actually do this) ####
        
        # define models using clmm2 and re-average them using centered covariates
        m19.2c <- clmm2(behavO ~ predForCtr + deltaForCtr + irrigCtr + (deltaForCtr*irrigCtr), 
                         random = Herd, Hess = TRUE, nAGQ = 10, dat = ctrdat) 
        m2.2c <- clmm2(behavO ~ predForCtr + deltaForCtr, 
                         random = Herd, Hess = TRUE, nAGQ = 10, dat = ctrdat)
        m6.2c <- clmm2(behavO ~ predForCtr + deltaForCtr + oldCtr, 
                         random = Herd, Hess = TRUE, nAGQ = 10, dat = ctrdat)
        m3.2c <- clmm2(behavO ~ predForCtr + deltaForCtr + (deltaForCtr*predForCtr), 
                         random = Herd, Hess = TRUE, nAGQ = 10, dat = ctrdat)
        m22.2c <- clmm2(behavO ~ predForCtr + irrigCtr + densCtr + (irrigCtr*densCtr), 
                         random = Herd, Hess = TRUE, nAGQ = 10, dat = ctrdat)
        m11.2c <- clmm2(behavO ~ predForCtr + deltaForCtr + densOwnCtr, 
                         random = Herd, Hess = TRUE, nAGQ = 10, dat = ctrdat)
        m4.2c <- clmm2(behavO ~ predForCtr + deltaForCtr + densCtr, 
                         random = Herd, Hess = TRUE, nAGQ = 10, dat = ctrdat)
        avgmod2 <- model.avg(m19.2, m2.2, m6.2, m3.2, m22.2, m11.2, m4.2) # error
        test <- model.avg(m2.2, m3.2) # error
        test <- model.avg(m2, m3) # no error
        testpred <- predict(test, newdata = dupedat) # error
        
        #ok... look at just the fmh model then. fuck.
        m3.2 <- clmm2(behavO ~ predFor + deltaFor + (deltaFor*predFor), 
                         random = Herd, Hess = TRUE, nAGQ = 10, dat = dat)
        
        testpredfmh <- predict(m3, newdata = dupedat)   
        
      #### just average the models that contain the same things ####
        
        fmhmod <- model.avg(m2c, m3c)
        densmod <- model.avg(m22c, m4c)
        
        summary(fmhmod)
        round(coef(fmhmod), 2)
        confint(fmhmod, level = 0.85)
        
        summary(densmod)
        confint(densmod, level = 0.85)
        # whos, dens*irrig is actually "significant", that's cool
        
     
        
        
           
    #### first stab at prediction plots ####
        
        ## plot with a random subsample of 10000 rows (for time)
        subdat <- newdat[sample(nrow(newdat), 10000),]
        ggplot(subdat, aes(x = deltaFor, y = predprob, colour = behavO)) +
          geom_smooth() + 
          facet_grid(. ~ irrig)
        # new weirdness:
        # plotting with deltaFor on x-axis is all wonky
        # bc deltaFor doesn't explain behavior very well
        # so i'm not sure how to illustrate the interaction
        
        # options:
            # plot separately for elk with deltafor=0 and >0
        length(which(newdat$deltaFor == 0))/nrow(newdat)
        
        # wonky, doesn't work but you get the idea
        ggplot(subdat[subdat$deltaFor == 0,], aes(x = predFor, y = predprob, colour = behavO))        
 
        
        
#### second stab at prediction plots - just Dens*Ag model ####
      
      
        #### make predictions ####

        
            ## define model using clmm2 (predict doesn't work with clmm) ##
            densmod2 <- clmm2(behavO ~ predFor + irrig + Dens + (irrig*Dens),
              random = Herd, Hess = TRUE, nAGQ = 10, dat = dat)
            
                
            ## predict probability of falling into each response category given those same data
            densprob <- predict(densmod2, newdata = dupedat)
            
                ## sanity check, both should = 1
                sum(densprob[1]+densprob[2]+densprob[3])
                sum(densprob[4]+densprob[5]+densprob[6])
    
    
            ## combine predictions with data used to predict
            densdat <- cbind(dupedat, densprob)
            head(densdat)
    
           

        #### plot predictions ####
            
            
            ## pull random subsample of 10000 predictions (takes forEVer otherwise)
            subdat2 <- newdat[sample(nrow(densdat), 10000),] 

            
            ## dens on Ag
            pp.dn <- ggplot(subdat2, aes(x = Dens, y = predprob, colour = behavO)) +
              geom_smooth() +
              scale_color_hue(name = "", labels = c("Resident", "Intermediate", "Migrant")) +
              labs(x = "Conspecific density (relative index)", y = "Probability") +
              theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5)) + 
              facet_grid(. ~ irrig)
            
            # oh gross, nobody wants to see that, i don't even want to see that
            
       
            
        
 #### extracting ORs and SEs from averaged model ####       
        
        ### nixed this plot bc the scale is insane; using unexponentiated for viz
        estplot <- ggplot(avgdat, 
          aes(y = OR, x = cov, ymin = ciLow95, ymax = ciHigh95)) +
          geom_point(size = 3) +
          geom_errorbar(width = 0.1) +
          geom_hline(yintercept = 1) +
          labs(y = "Odds Ratio", x = "")
        
        
        
           
#### totally unrelated but i just got a wild hair - could i use sapphires to look at actual forage? ####
    
    View(dat[dat$Herd == "Sapphire",])
    # yeah, got a range of deltaFor, and not everybody had access to irrig on winter range... 
            
            
            
#### hm is colon diff from I? ####
            

    ## models ##
 
      tm1 <- clmm(behavO ~ deltaFor + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      tm2 <- clmm(behavO ~ predFor + deltaFor + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      tm3 <- clmm(behavO ~ predFor + deltaFor + deltaFor:predFor + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      tm4 <- clmm(behavO ~ predFor + deltaFor + Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      tm5 <- clmm(behavO ~ predFor + deltaFor + Dens + deltaFor:Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      tm6 <- clmm(behavO ~ predFor + deltaFor + Old + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      tm7 <- clmm(behavO ~ predFor + deltaFor + Old + deltaFor:Old + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      tm8 <- clmm(behavO ~ predFor + deltaFor + Old + Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      tm9 <- clmm(behavO ~ predFor + deltaFor + Old + Dens + Old:Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      tm10 <- clmm(behavO ~ densOwn + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      tm11 <- clmm(behavO ~ predFor + deltaFor + densOwn + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      tm12 <- clmm(behavO ~ predFor + deltaFor + densOwn + deltaFor:predFor + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      tm13 <- clmm(behavO ~ predFor + deltaFor + densOwn + deltaFor:densOwn + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      tm14 <- clmm(behavO ~ densOwn + Old + densOwn:Old + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      tm15 <- clmm(behavO ~ predFor + deltaFor + densOwn + Old + densOwn:Old + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      tm16 <- clmm(behavO ~ irrig + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      tm17 <- clmm(behavO ~ densOwn + irrig + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      tm18 <- clmm(behavO ~ predFor + deltaFor + irrig   + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      tm19 <- clmm(behavO ~ predFor + deltaFor + irrig + deltaFor:irrig + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)  
      tm20 <- clmm(behavO ~ predFor + deltaFor + irrig + predFor:irrig + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      tm21 <- clmm(behavO ~ irrig + Dens + irrig:Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      tm22 <- clmm(behavO ~ predFor + irrig + Dens + irrig:Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)  
      tm23 <- clmm(behavO ~ predFor + irrig + Old + Dens + Old:Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)  
      tm24 <- clmm(behavO ~ densOwn + irrig + Dens + irrig:Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      tm25 <- clmm(behavO ~ densOwn + irrig + Dens + densOwn:Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat) 
      tm26 <- clmm(behavO ~ predFor + irrig + densOwn + irrig:densOwn + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat) 
      
      
      # compete with AICc #
      tmods <- list()
      tmodnms <- paste0("tm", rep(1:26))
      for (i in 1:length(tmodnms)) { tmods[[i]] <- get(tmodnms[[i]]) }
      aictab(cand.set = tmods, modnames = tmodnms)
      taictab <- data.frame(aictab(cand.set = tmods, modnames = tmodnms))

      View(taictab)
      confint(tm19)
      confint(m19)
      # oh whew samesame
  

      
#### ok one more wild hair b4 back to work... look at spread of predictions per group ####
    

      testdat <- newdat  %>%
        group_by(behavO) %>%
        summarise(meanPred = mean(predprob) , varPred = sd(predprob))
      
      # cool, ponder this more later
      # res pred varied a lot more relative to the mean
      
      
      
#### zmisc ####
      
      
      # how well do predictions fit data? #
      
        test <- fitted(mod)
        test[1:10]
        # i think fitted is the prob of the elk 
        # displaying the behavior she did
        # based on the model's prediction of what she'd do
      
      hist(test)
      mean(test); sd(test)
      
      
      
  #### just for funzies... quadratic predfor?? ####
      
      quaddat <- dat %>%
        mutate(predFor2 = predFor^2)
      
      hm1 <- clmm(behavO ~ predFor + predFor2 + deltaFor + irrig + deltaFor:irrig + (1|Herd),
        dat = quaddat, Hess = TRUE, nAGQ = 7)
      summary(hm1)
      # nah, worse LL and AIC than before
      
      
      
      
#### trying to figure out diagnostics ####
      
    

sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

(s <- with(dat, summary(as.numeric(apply) ~ predFor + deltaFor + irrig, fun=sf)))


      
      
      #### still disgnostics - check prob of model predicting that behav? ####
      
      
            ## define model using clmm2 (predict doesn't work with clmm) ##
            mod2 <- clmm2(behavO ~ predFor + deltaFor + irrig + deltaFor:irrig,
              random = Herd, Hess = TRUE, nAGQ = 10, dat = dat)

                
            ## predict probability of falling into each response category given those same data
            mod2prob <- predict(mod2, newdata = dat)
            

            ## combine predictions with data used to predict
            testpred <- cbind(mod2prob, dat)
            head(testpred)
            
            ## assess probabilities
            hist(testpred$mod2prob)
              # peak ~ 0.3 suggests it's not doing that much better than random chance?
            
            
      #### compare hessian to model without deltafor main effect ####
            
            hm <- clmm2(behavO ~ predFor + irrig + deltaFor:irrig, 
              random = Herd, Hess = TRUE, nAGQ = 10, dat = dat)
            summary(hm)
            summary(mod2)
    
            
      #### playing with model options ####
            
          
          # 7 vs 10 quadrature points
            
          hm7 <- clmm(behavO ~ predFor + irrig + deltaFor + deltaFor:irrig + (1|Herd), 
            Hess = TRUE, nAGQ = 7, dat = dat)
          summary(hm7)
          
          hm72 <- clmm2(behavO ~ predFor + irrig + deltaFor + deltaFor:irrig, random = Herd, 
              Hess = TRUE, nAGQ = 7, dat = dat)
          summary(hm72)
          
          hm10 <- clmm(behavO ~ predFor + irrig + deltaFor + deltaFor:irrig + (1|Herd), 
            Hess = TRUE, nAGQ = 10, dat = dat)
          summary(hm10)

          
          
    #### holy fucking shitballs did someone seriously just publish a GOF package? ####
          
          install.packages("generalhoslem")
          library(generalhoslem)
          lipsitz.test(model = mod) # using default g of 10; f&h2013 say it's good
          
          # hm ok needs clm, not clmm. 
          
          # first assess difference when you remove that random effect
          
          mod.clm <- clm(behavO ~ predFor + deltaFor + irrig + deltaFor:irrig, data = dat)
          summary(mod.clm)
          summary(mod)
          
          # there is a difference
          # generally, coef estimates for clm suggest weaker effects and have lower SEs
          # clm, LL -254.07 & AIC is 520.15 | clmm, LL -233.13, AIC 480.25
          # clm conditional Hessian 4.8e3 | clmm 1.8e4 (i don't understand this number tbh)

          # based on the diffs i think it's ok to do a preliminary assessment of GOF
          # using the clm model, because we already know it doesn't fit the data as well
          # (based on LL and AIC). So if it fits, i think we may be able to infer that
          # the mixed effects model does not have an issue with GOF?
          
          # here goes nothin
        
          
            
      ## GOF TEST 1 : lipsitz ##
          
          lipsitz.test(mod.clm) # p-val = 1.263e-5, ROUGH. does have some warnings...
          
        
            
      ## GOF TEST 2 : modified Hosmer-Lemeshow ##
          
          # first try with the clm version
          predprobclm <- data.frame(
            predFor = dat$predFor, 
            deltaFor = dat$deltaFor,
            irrig = dat$irrig)
          fv <- predict(mod.clm, newdata = predprobclm)$fit
          logitgof(dat$behavO, fv, g = 10, ord = TRUE) # p = 0.0005, STILL ROUGH
          

          
          # tricking it into looking at the clmm instead of clm
          
          # predict with original behaviors
          testdat1 <- dat %>%
            dplyr::select(behavO, deltaFor, predFor, irrig)
          testpred1 <- predict(mod2, newdata = testdat1)
          pred1 <- cbind(testdat1, testpred1) %>%
            rename(c("behavO" = "behavior", "testpred1" = "prediction")) %>%
            dplyr::select(behavior, prediction) %>%
            mutate(resident = ifelse(behavior == "resident", prediction, NA),
              other = ifelse(behavior == "other", prediction, NA),
              migrant = ifelse(behavior == "migrant", prediction, NA)) %>%
            dplyr::select(-c(behavior, prediction))
          
          # predict using other instead of resident, resident instead of migrant, migrant instead of other
          testdat2 <- dat %>%
            dplyr::select(behavO, deltaFor, predFor, irrig) %>%
            mutate(behav = ifelse(behavO == "resident", "other",
              ifelse(behavO == "migrant", "resident", "migrant"))) %>%
            dplyr::select(-behavO)
          testdat2$behavO <- factor(testdat2$behav, levels = c("resident", "other", "migrant"), ordered = TRUE)
          testpred2 <- predict(mod2, newdata = testdat2)
          pred2 <- cbind(testdat2, testpred2) %>%
            rename(c("behavO" = "behavior", "testpred2" = "prediction")) %>%
            dplyr::select(behavior, prediction) %>%
            mutate(resident = ifelse(behavior == "resident", prediction, NA),
              other = ifelse(behavior == "other", prediction, NA),
              migrant = ifelse(behavior == "migrant", prediction, NA)) %>%
            dplyr::select(-c(behavior, prediction))
          
          
          # predict final behav option
          testdat3 <- dat %>%
            dplyr::select(behavO, deltaFor, predFor, irrig) %>%
            mutate(behav = ifelse(behavO == "resident", "migrant",
              ifelse(behavO == "other", "resident", "other"))) %>%
            dplyr::select(-behavO)
          testdat3$behavO <- factor(testdat3$behav, levels = c("resident", "other", "migrant"), ordered = TRUE)
          testpred3 <- predict(mod2, newdata = testdat3)
          pred3 <- cbind(testdat3, testpred3) %>%
            rename(c("behavO" = "behavior", "testpred3" = "prediction")) %>%
            dplyr::select(behavior, prediction) %>%
            mutate(resident = ifelse(behavior == "resident", prediction, NA),
              other = ifelse(behavior == "other", prediction, NA),
              migrant = ifelse(behavior == "migrant", prediction, NA)) %>%
            dplyr::select(-c(behavior, prediction))

          # combine 
          preds <- coalesce(pred1, pred2, pred3)
          
          # and make it a numeric matrix
          predmat <- matrix(nrow = nrow(dat), ncol = length(unique(dat$behavO)))
          attributes(predmat)$dimnames[[1]] <- (1:308)
          attributes(predmat)$dimnames[[2]] <- c("resident", "other", "migrant")
          
          # pathetically combine predictions in numeric matrix 
          predmat[1 : nrow(preds)] <- preds$resident
          predmat[(nrow(preds)+1) : (nrow(preds)*2)] <- preds$other
          predmat[((nrow(preds)*2)+1) : (nrow(preds)*3)] <- preds$migrant
          
          # gof test
          logitgof(dat$behavO, predmat, g = 10, ord = TRUE) # even more significant, which i don't get
              # & warning: at least 1 cell in expected frequencies table is <1. Chi-sq approximation may be wrong
          
          
      ## TEST 3:  pulkstenic-robinson test ##
          
          pulkrob.chisq(mod.clm, catvars = c("irrig")) # oh holy fuck, this is ok. p = 0.6491
          
          
          
          
      ## just look at how closely predictions match ##
          
          hm <- preds; hm$actualBehav <- dat$behavO
          test <- colnames(hm)[apply(hm,1,which.max)]
          hm$predBehav <- test
          hm$matchBehav = ifelse(hm$actualBehav == hm$predBehav, 1, 0)
          length(which(hm$matchBehav == 1)) / nrow(hm)
          # predicts correctly 59% of the time
          # slightly better than a random guess so i got that going for me which is nice
          
          
          ## ok how bout you sadly compare that to a regular ol pr(mig) model?
                    
          # new df so i don't taint the old one with such heresy
          testdat <- dat %>%
            mutate(Mig = ifelse(Behav == "migrant", 1, 0))
          
          library(lme4)
        
          # define model #
          modbin <- glmer(Mig ~ predFor + deltaFor + irrig + deltaFor:irrig + (1|Herd),
            family = binomial(logit), data = testdat)
          summary(modbin)
          
          predbin <- predict(modbin)
          predbindf <- data.frame(predProb = predbin) %>%
            mutate(predMig = ifelse(predProb > 0, 1, 0)) %>%
            mutate(actualBehav = testdat$Mig) %>%
            mutate(matchBehav = ifelse(predMig == actualBehav, 1, 0)) 
          length(which(predbindf$matchBehav == 1)) / nrow(predbindf)
          # son of a fucking bitch. 78% correct predictions now.
          
   
          
      ### code working thru the above ###
          
          attributes(fv)
          attributes(fv)$dim
          attributes(fv)$dimnames
          

          attributes(fv2)
          attributes(fv)
          
          fv[1]
          fv[2]
          fv[1,2]
          fv[309]
          
          # trick test 2 to work with clmm?
          testdat <- dat %>%
            mutate(behavO = as.factor(behavO)) %>%
            filter(!is.na(Old))
          testmod <- clmm2(behavO ~ predFor + deltaFor + irrig + deltaFor:irrig, 
                             random = Herd, Hess = TRUE, nAGQ = 10, dat = testdat) 
          fv2 <- predict(testmod, newdata = testdat)
          logitgof(testdat$behavO, fv2, g = 10, ord = TRUE)
          # this doesn't work 
          # because predict is giving the probability of the obs falling in the category it did
          # but we want the probability of it falling into each of the 3 categories
          # so we're gonna hack the shit out of it, hopefully
          
          
      
### ### ### ### ### #
#### |MODEL FIT| ####
### ### ### ### ### #
        
        
    library(generalhoslem)
        
    # define model without random effect of herd (tests can't handle mixed effects)
    # note: model without the random effect fits data worse than the one with it, so...
    mod.clm <- clm(behavO ~ predFor + deltaFor + irrig + deltaFor:irrig, data = dat)  
    
    
    
    
    #### TEST 1 : Lipsitz ####
    
    lipsitz.test(mod.clm) # p-val = 1.263e-5, ROUGH. does have some warnings...
    
    
    
    #### TEST 2 : Modified Hosmer-Lemeshow ####
        
    
        ## with the clm version ##
        
            predprobclm <- data.frame(
              predFor = dat$predFor, 
              deltaFor = dat$deltaFor,
              irrig = dat$irrig)
            fv <- predict(mod.clm, newdata = predprobclm)$fit
            logitgof(dat$behavO, fv, g = 10, ord = TRUE) # p = 0.0005, STILL ROUGH
 
            
        ## trick it into looking at the clmm instead of clm? ##
        
          # predict with original behaviors
          testdat1 <- dat %>%
            dplyr::select(behavO, deltaFor, predFor, irrig)
          testpred1 <- predict(mod2, newdata = testdat1)
          pred1 <- cbind(testdat1, testpred1) %>%
            rename(c("behavO" = "behavior", "testpred1" = "prediction")) %>%
            dplyr::select(behavior, prediction) %>%
            mutate(resident = ifelse(behavior == "resident", prediction, NA),
              other = ifelse(behavior == "other", prediction, NA),
              migrant = ifelse(behavior == "migrant", prediction, NA)) %>%
            dplyr::select(-c(behavior, prediction))
          
          # predict using other instead of resident, resident instead of migrant, migrant instead of other
          testdat2 <- dat %>%
            dplyr::select(behavO, deltaFor, predFor, irrig) %>%
            mutate(behav = ifelse(behavO == "resident", "other",
              ifelse(behavO == "migrant", "resident", "migrant"))) %>%
            dplyr::select(-behavO)
          testdat2$behavO <- factor(testdat2$behav, levels = c("resident", "other", "migrant"), ordered = TRUE)
          testpred2 <- predict(mod2, newdata = testdat2)
          pred2 <- cbind(testdat2, testpred2) %>%
            rename(c("behavO" = "behavior", "testpred2" = "prediction")) %>%
            dplyr::select(behavior, prediction) %>%
            mutate(resident = ifelse(behavior == "resident", prediction, NA),
              other = ifelse(behavior == "other", prediction, NA),
              migrant = ifelse(behavior == "migrant", prediction, NA)) %>%
            dplyr::select(-c(behavior, prediction))
          
          
          # predict final behav option
          testdat3 <- dat %>%
            dplyr::select(behavO, deltaFor, predFor, irrig) %>%
            mutate(behav = ifelse(behavO == "resident", "migrant",
              ifelse(behavO == "other", "resident", "other"))) %>%
            dplyr::select(-behavO)
          testdat3$behavO <- factor(testdat3$behav, levels = c("resident", "other", "migrant"), ordered = TRUE)
          testpred3 <- predict(mod2, newdata = testdat3)
          pred3 <- cbind(testdat3, testpred3) %>%
            rename(c("behavO" = "behavior", "testpred3" = "prediction")) %>%
            dplyr::select(behavior, prediction) %>%
            mutate(resident = ifelse(behavior == "resident", prediction, NA),
              other = ifelse(behavior == "other", prediction, NA),
              migrant = ifelse(behavior == "migrant", prediction, NA)) %>%
            dplyr::select(-c(behavior, prediction))
    
          # combine 
          preds <- coalesce(pred1, pred2, pred3)
          
          # and make it a numeric matrix
          predmat <- matrix(nrow = nrow(dat), ncol = length(unique(dat$behavO)))
          attributes(predmat)$dimnames[[1]] <- (1:308)
          attributes(predmat)$dimnames[[2]] <- c("resident", "other", "migrant")
          
          # pathetically combine predictions in numeric matrix 
          predmat[1 : nrow(preds)] <- preds$resident
          predmat[(nrow(preds)+1) : (nrow(preds)*2)] <- preds$other
          predmat[((nrow(preds)*2)+1) : (nrow(preds)*3)] <- preds$migrant
          
          # gof test
          logitgof(dat$behavO, predmat, g = 10, ord = TRUE) # even more significant, which i don't understand
              # BUT warning: at least 1 cell in expected frequencies table is <1. Chi-sq approximation may be wrong
          
          
          
      #### TEST 3 : Pulkstenic-Robinson ####
        
        pulkrob.chisq(mod.clm, catvars = c("irrig")) # p = 0.6491
        pulkrob.deviance(mod.clm, c("irrig")) # p = 0.6125

    #### rerunning model selection with glmer instead, sadly ####
      
      library(lme4) # this blocks ranef and varCorr from ordinal
          
          
          testdat <- dat %>%
            mutate(Mig = as.factor(ifelse(Behav == "migrant", 1, 0))) %>%
            filter(!is.na(Old)) %>%
            mutate(lDens = log(Dens)) 
          
      #### define a priori models ####
      
      tm1 <- glmer(Mig ~ deltaFor + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat)
      tm2 <- glmer(Mig ~ predFor + deltaFor + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat)
      tm3 <- glmer(Mig ~ predFor + deltaFor + deltaFor:predFor + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat)
      tm4 <- glmer(Mig ~ predFor + deltaFor + lDens + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat) 
      tm5 <- glmer(Mig ~ predFor + deltaFor + lDens + deltaFor:lDens + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat)
      tm6 <- glmer(Mig ~ predFor + deltaFor + Old + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat)
      tm7 <- glmer(Mig ~ predFor + deltaFor + Old + deltaFor:Old + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat) 
      tm8 <- glmer(Mig ~ predFor + deltaFor + Old + lDens + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat) 
      tm9 <- glmer(Mig ~ predFor + deltaFor + Old + lDens + Old:lDens + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat)
      tm10 <- glmer(Mig ~ densOwn + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat)
      tm11 <- glmer(Mig ~ predFor + deltaFor + densOwn + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat)
      tm12 <- glmer(Mig ~ predFor + deltaFor + densOwn + deltaFor:predFor + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat)
      tm13 <- glmer(Mig ~ predFor + deltaFor + densOwn + deltaFor:densOwn + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat)
      tm14 <- glmer(Mig ~ densOwn + Old + densOwn:Old + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat)
      tm15 <- glmer(Mig ~ predFor + deltaFor + densOwn + Old + densOwn:Old + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat)
      tm16 <- glmer(Mig ~ irrig + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat)
      tm17 <- glmer(Mig ~ densOwn + irrig + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat)
      tm18 <- glmer(Mig ~ predFor + deltaFor + irrig   + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat)
      tm19 <- glmer(Mig ~ predFor + deltaFor + irrig + deltaFor:irrig + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat)  
      tm20 <- glmer(Mig ~ predFor + deltaFor + irrig + predFor:irrig + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat)
      tm21 <- glmer(Mig ~ irrig + Dens + irrig:Dens + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat)
      tm22 <- glmer(Mig ~ predFor + irrig + lDens + irrig:lDens + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat)  
      tm23 <- glmer(Mig ~ predFor + irrig + Old + Dens + Old:Dens + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat)  
      tm24 <- glmer(Mig ~ densOwn + irrig + Dens + irrig:Dens + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat)
      tm25 <- glmer(Mig ~ densOwn + irrig + Dens + densOwn:Dens + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat) 
      tm26 <- glmer(Mig ~ predFor + irrig + densOwn + irrig:densOwn + (1|Herd), family = binomial(logit), nAGQ = 7, dat = testdat) 

      
      # compete with AICc #
      tmods <- list()
      tmodnms <- paste0("tm", rep(1:26))
      for (i in 1:length(tmodnms)) { tmods[[i]] <- get(tmodnms[[i]]) }
      aictab(cand.set = tmods, modnames = tmodnms)
      taictab <- data.frame(aictab(cand.set = tmods, modnames = tmodnms))
      write.csv(taictab, file = "aic-prMig.csv", row.names = F)
      
      # center covariates to allow model averagin
      ctrdat2 <- testdat %>%
        mutate(predForCtr = predFor - mean(predFor),
               deltaForCtr = deltaFor - mean(deltaFor),
               irrigCtr = as.numeric(irrig) - mean(as.numeric(irrig)),
               lDensCtr = lDens - mean(lDens),
               oldCtr = as.numeric(Old) - mean(as.numeric(Old)),
               densOwnCtr = densOwn - mean(densOwn))
      
      # define models using centered covariates
      tm15c <- glmer(Mig ~ predForCtr + deltaForCtr + densOwnCtr + oldCtr + densOwnCtr:oldCtr + (1|Herd), family = binomial(logit), nAGQ = 7, dat = ctrdat2)
      tm6c <- glmer(Mig ~ predForCtr + deltaForCtr + oldCtr + (1|Herd), family = binomial(logit), nAGQ = 7, dat = ctrdat2)
      tm2c <- glmer(Mig ~ predForCtr + deltaForCtr + (1|Herd), family = binomial(logit), nAGQ = 7, dat = ctrdat2)
      tm11c <- glmer(Mig ~ predForCtr + deltaForCtr + densOwnCtr + (1|Herd), family = binomial(logit), nAGQ = 7, dat = ctrdat2)
      tm19c <- glmer(Mig ~ predForCtr + deltaForCtr + irrigCtr + deltaForCtr:irrigCtr + (1|Herd), family = binomial(logit), nAGQ = 7, dat = ctrdat2)  
      tm3c <- glmer(Mig ~ predForCtr + deltaForCtr + deltaForCtr:predForCtr + (1|Herd), family = binomial(logit), nAGQ = 7, dat = ctrdat2)
      tm4c <- glmer(Mig ~ predForCtr + deltaForCtr + lDensCtr + (1|Herd), family = binomial(logit), nAGQ = 7, dat = ctrdat2) 
      tm5c <- glmer(Mig ~ predForCtr + deltaForCtr + lDensCtr + deltaForCtr:lDensCtr + (1|Herd), family = binomial(logit), nAGQ = 7, dat = ctrdat2)
      tm8c <- glmer(Mig ~ predForCtr + deltaForCtr + oldCtr + lDensCtr + (1|Herd), family = binomial(logit), nAGQ = 7, dat = ctrdat2) 

      
      #### average the models ####
        tavgmod <- model.avg(tm15c, tm6c, tm2c, tm11c, tm19c, tm3c, tm5c, tm8c, tm4c) 
        summary(tavgmod)
        # write.csv(round(confint(tavgmod, level = 0.85), 2), "testres.csv")
        
        
      #### look at each model individually ####
        
        summary(tm15)
        round(confint(tm15, level = 0.85, parm="beta_",method="Wald"), 2)
        
        summary(tm6)
        round(confint(tm6, level = 0.85, parm="beta_",method="Wald"), 2)

        summary(tm152)
        round(confint(tm2, level = 0.85, parm="beta_",method="Wald"), 2)

        summary(tm11)
        round(confint(tm11, level = 0.85, parm="beta_",method="Wald"), 2)

        summary(tm3)
        round(confint(tm3, level = 0.85, parm="beta_",method="Wald"), 2)

        summary(tm5)
        round(confint(tm5, level = 0.85, parm="beta_",method="Wald"), 2)

        summary(tm8)
        round(confint(tm8, level = 0.85, parm="beta_",method="Wald"), 2)

        summary(tm4)
        round(confint(tm4, level = 0.85, parm="beta_",method="Wald"), 2)
        
        
        
    #### quick prelim check of predictive accurary    
        
        t <- predict(tavgmod, ctrdat2, type = "response")
        testpreds <- cbind(ctrdat2, t) %>%
          rename(predProb = t) %>%
          mutate(predMig = ifelse(predProb > 0.49, 1, 0)) %>%
          mutate(predCorrect = ifelse(predMig == 1 & behavO == "migrant", 1, 0))
        length(which(testpreds$predCorrect == 1)) / nrow(testpreds)
        summary(t)
        # this only predicts correctly 53% of the time, worse than my ordinal model
        
        # try one more again with that top-selected model
        t2 <- predict(tm15, testdat, type = "response")
        testpreds2 <- cbind(testdat, t2) %>%
          rename(predProb = t2) %>%
          mutate(predMig = ifelse(predProb > 0.49, 1, 0)) %>%
          mutate(predCorrect = ifelse(predMig == 1 & behavO == "migrant", 1, 0))
        length(which(testpreds2$predCorrect == 1)) / nrow(testpreds2) 
        summary(t2)
        # and this is also only 53%, interesting
        
        
        # ok so how about model fit (explanatory vs predictive power)
        
        predlog <- cbind(ctrdat2, t) %>%
          rename(predProb = t) %>%
          # partition into 10 groups based on predicted probability
          mutate(g = factor(findInterval(predProb, 
            quantile(predProb, probs = c((0:9)/10))))) %>%
          # calculate observed and expected obs per group
          group_by(g) %>%
          summarise(
            obs = length(which(Mig == 1)) / n(),
            exp = mean(predProb)) %>%
          mutate(oe = (obs - exp)^2 / exp)
        
        chistat <- sum(predlog$oe)
        maybedof <- 10-2 # number of deciles - 2?
        
        pchisq(chistat, maybedof) # oh that is just horrendous

        
        
        
        # and "top" model, for funzies

        killme <- cbind(testdat, fitted(tm15)) %>%
          rename(predProb = "fitted(tm15)") %>%
          # partition into 10 groups based on predicted probability
          mutate(g = factor(findInterval(predProb, 
            quantile(predProb, probs = c((0:9)/10))))) %>%
          # calculate observed and expected obs per group
          group_by(g) %>%
          summarise(
            obs = length(which(Mig == 1)) / n(),
            exp = mean(predProb)) %>%
          mutate(oe = (obs - exp)^2 / exp)
        chiagain <- sum(killme$oe)
        pchisq(chiagain, maybedof) # just-- just no.
        
        hoslem.test(testdat$Mig, fitted(tm15), g = 10)
        # the good news is i correctly figured out the df
        # the bad news is this model fucking blows
                  
        
        
        
        
  #### chi-square/deviance stuff from ordinal clm intro ####
      
      # first get likelihood of a full model (has parameter for each obs)
      # only considering predFor and irrig because deltaFor alone was found to be insignificant
      # and the ordinal vignette, citing Collett 2002 which I don't have access to, recommends
      # removing insignificant terms from full model deviance estimation
      tab <- with(dat, table(predFor, irrig, behavO))
      tab
      pi.hat <- tab / rowSums(tab)
      (ll.full <- sum(tab * ifelse(pi.hat > 0, log(pi.hat), 0))) # -317.8857
      
      # then get likelihood of a reduced model (i.e., your top model)
      summary(mod)
      (ll.red <- mod$logLik) # -233.1266
      
      
      # calculate deviance (-2* likelihood of null - likelihood of full)
      (dev <- -2* (ll.red - ll.full)) # -169.5181
      
      # friendly reminder residual deviance is just -2*ll
      # which is also supposed to = the deviance from a null model ( ~1) - reduced modek
      (dev.red <- -2 * mod$logLik) # 466.2532
      
      
      # if deviance is sim size to DOF of distn, no evidence of lack of fit
      # and if that's the case this is the worst model in human history
      
      
      
      
      
      
    #### mess-ups when trying for the fagerland hosmer approach
      
      
      
        preddat$decile <- with(preddat, factor(
          findInterval(si, c(-Inf,
            quantile(si, probs = c((0:9)/10), Inf), rightmost.closed = TRUE,
            labels = 1:10))))
        # returns 2:12
        
        
        preddat$decile = with(preddat, cut(si,
          breaks = quantile(si, probs = seq(0, 1, by = 0.1)), include.lowest = TRUE))
        preddat$decile <- factor(preddat$decile, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
        # returns NAs
                
      
      
        
        # using df with predicted prob of each behav for each obs
        chidat <- hm %>%
          # rename columns for clarity
          rename(pi1 = resident, pi2 = other, pi3 = migrant) %>%
          # calculate ordinal score per obs
          mutate(si = pi1 + 2*pi2) %>%
          # partition into 10 groups based on ordinal score
          mutate(g = factor(findInterval(si, 
            quantile(si, probs = c((0:9)/10))))) %>%
          # calculate observed and expected obs per group
          group_by(g) %>%
          summarise(
            obsRes = length(which(actualBehav == "resident")),
            obsInt = length(which(actualBehav == "other")), 
            expRes = length(which(predBehav == "resident")),
            expInt = length(which(predBehav == "other")))
        # not enough counts in groups...
        
        
                
        # does using fewer groups help?
        
        # using df with predicted prob of each behav for each obs
        faghos2 <- hm %>%
          # rename columns for clarity
          rename(pi1 = resident, pi2 = other, pi3 = migrant) %>%
          # calculate ordinal score per obs (4)
          mutate(si = pi1 + 2*pi2 + 3*pi3) %>%
          # partition into 10 groups based on ordinal score
          mutate(g = factor(findInterval(si, 
            quantile(si, probs = c((0:4)/5))))) %>%
          # calculate observed and expected obs per group
          group_by(g) %>%
          summarise(
            obsRes = length(which(actualBehav == "resident")) / n(),
            expRes = length(which(predBehav == "resident")) / n(),
            obsInt = length(which(actualBehav == "other")) / n(), 
            expInt = length(which(predBehav == "other")) / n(),
            obsMig = length(which(actualBehav == "migrant")) / n(),
            expMig = length(which(predBehav == "migrant")) / n())
        # no, duh
    
        
      # these are counts not probs, ooops
        
        
        # using df with predicted prob of each behav for each obs
        faghos <- hm %>%
          # rename columns for clarity
          rename(pi1 = resident, pi2 = other, pi3 = migrant) %>%
          # calculate ordinal score per obs (4)
          mutate(si = pi1 + 2*pi2 + 3*pi3) %>%
          # partition into 10 groups based on ordinal score
          mutate(g = factor(findInterval(si, 
            quantile(si, probs = c((0:9)/10))))) %>%
          # calculate observed and expected obs per group
          group_by(g) %>%
          summarise(
            obsRes = round(length(which(actualBehav == "resident")) / n(), 2),
            expRes = round(length(which(predBehav == "resident")) / n(), 2),
            obsInt = round(length(which(actualBehav == "other")) / n(), 2), 
            expInt = round(length(which(predBehav == "other")) / n(), 2),
            obsMig = round(length(which(actualBehav == "migrant")) / n(), 2),
            expMig = round(length(which(predBehav == "migrant")) / n(), 2))
        write.csv(faghos, "failedgroupings.csv", row.names=F)
        
        
        
    #### for wild542 ####
        
        predobs <- cbind(dat, preds) %>%
          rename(prRes = resident, prInt = other, prMig = migrant) %>%
          dplyr::select(AnimalID, Herd, Old, Dens, densOwn, deltaFor, irrig, predFor, behavO,
            prRes, prInt, prMig)
        write.csv(predobs, file = "migdat.csv", row.names=F)
        
        forclass <- cbind(dat, preds) %>%
          # only keep columns of interest, and rename some (just for clarity)
          rename(prRes = resident, prInt = other, prMig = migrant) %>%
          dplyr::select(AnimalID, Herd, deltaFor, irrig, predFor, behavO, prRes, prInt, prMig) %>%
          # calculate ordinal score per observation
          mutate(score = prRes + 2*prInt + 3*prMig) %>%
          # partition into 10 groups based on ordinal score
          mutate(g = factor(findInterval(score, 
            quantile(score, probs = c((0:9)/10)))))
        write.csv(forclass, file = "scoresgroups.csv", row.names=F)
      
      

      
      
  #### curious about intermediacy in particular ####
        
        # after running chi-sq
        
        sum(pulrob$oeInt)
        ((2 * i.pr) - 1) * (1 - 1) - 1 - 1
        pchisq( sum(pulrob$oeInt), ((2 * i.pr) - 1) * (1 - 1) - 1 - 1) #NaN, fair enough
        
                # calculate degrees of freedom
        i.pr <- 2 # I = number of covariate patterns
        j.pr <- 3 # J = number of response categories
        k.pr <- 1 # k = number of categorical terms
        dof.pr <- ((2*i.pr) - 1) * (j.pr - 1) - k.pr - 1
        
        
        
        ## how bout looking at thresholds ##
        
        # compare equidistant to flexible thresholds
          # if not equidistant, intermediacy more similar to one or the other behavior
        
        mod.eq <- clmm(behavO ~ predFor + deltaFor + irrig + deltaFor:irrig + (1|Herd),
          Hess = TRUE, nAGQ = 10, dat = dat,
                         threshold = "equidistant") 
        summary(mod.eq)
        
        anova(mod, mod.eq)
        # these are basically the same model, identical LLs, p=1
        # so conclude evidence that intermediacy may not be more similar
        # to one behavior type than the other
        
        anova(m2c, m22c)
        
        
        
        
  ##### okkkk i know this is an awful idea but i wanna see intermediate as the baseline ####
        
        
    #### define a priori models ####
      
      testdat <- olddat %>% mutate(behavI = factor(behavO, 
        levels = c("other", "resident", "migrant"), ordered = TRUE))
        
      m1 <- clmm(behavI ~ deltaFor + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat)
      m2 <- clmm(behavI ~ predFor + deltaFor + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat)
      m3 <- clmm(behavI ~ predFor + deltaFor + deltaFor:predFor + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat)
      m4 <- clmm(behavI ~ predFor + deltaFor + Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat)
      m5 <- clmm(behavI ~ predFor + deltaFor + Dens + deltaFor:Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat)
      m6 <- clmm(behavI ~ predFor + deltaFor + Old + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat)
      m7 <- clmm(behavI ~ predFor + deltaFor + Old + deltaFor:Old + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat)
      m8 <- clmm(behavI ~ predFor + deltaFor + Old + Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat)
      m9 <- clmm(behavI ~ predFor + deltaFor + Old + Dens + Old:Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat)
      m10 <- clmm(behavI ~ densOwn + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat)
      m11 <- clmm(behavI ~ predFor + deltaFor + densOwn + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat)
      m12 <- clmm(behavI ~ predFor + deltaFor + densOwn + deltaFor:predFor + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat)
      m13 <- clmm(behavI ~ predFor + deltaFor + densOwn + deltaFor:densOwn + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat)
      m14 <- clmm(behavI ~ densOwn + Old + densOwn:Old + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat)
      m15 <- clmm(behavI ~ predFor + deltaFor + densOwn + Old + densOwn:Old + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat)
      m16 <- clmm(behavI ~ irrig + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat)
      m17 <- clmm(behavI ~ densOwn + irrig + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat)
      m18 <- clmm(behavI ~ predFor + deltaFor + irrig   + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat)
      m19 <- clmm(behavI ~ predFor + deltaFor + irrig + deltaFor:irrig + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat)  
      m20 <- clmm(behavI ~ predFor + deltaFor + irrig + predFor:irrig + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat)
      m21 <- clmm(behavI ~ irrig + Dens + irrig:Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat)
      m22 <- clmm(behavI ~ predFor + irrig + Dens + irrig:Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat)  
      m23 <- clmm(behavI ~ predFor + irrig + Old + Dens + Old:Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat)  
      m24 <- clmm(behavI ~ densOwn + irrig + Dens + irrig:Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat)
      m25 <- clmm(behavI ~ densOwn + irrig + Dens + densOwn:Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat) 
      m26 <- clmm(behavI ~ predFor + irrig + densOwn + irrig:densOwn + (1|Herd), Hess = TRUE, nAGQ = 10, dat = testdat) 
      
      
      # compete with AICc #
      mods <- list()
      modnms <- paste0("m", rep(1:26))
      for (i in 1:length(modnms)) { mods[[i]] <- get(modnms[[i]]) }
      aictab(cand.set = mods, modnames = modnms)
      aictab <- data.frame(aictab(cand.set = mods, modnames = modnms))
      View(data.frame(aictab(cand.set = mods, modnames = modnms)))
             
       
    #### test proportional odds assumption - from actual old code ####
        
      # Agresti 2002 says this assumption should be based more on the inference you want to draw than
      # methematical considerations, but out of curiosity... (using models without random herd effect) 
      poddstest <- clm(behavO ~ predFor + deltaFor + irrig + deltaFor:irrig, Hess = TRUE, nAGQ = 10, dat = dat) 
      nominal_test(poddstest) # nothing significant, suggests meet assumption
      scale_test(poddstest) # predfor is significant; try scaling by it
      
      test <- clmm2(behavO ~ predFor + deltaFor + irrig + deltaFor:irrig, scale = ~predFor, dat = dat)
      summary(test) # worse LL and AIC than top model; not changing it.
      
      
      
  #### combining plots with diff y-axes ####
      
      ## turns out you can't do that in ggplot, fiiiine ##
      
              # plot together with ppn(behav) plot
        
        ggplot() +
          geom_bar(
            data = poppns, 
            aes(x = Herd, y = ppn, fill = behav), 
            stat="identity", position='fill') +
          geom_point(
            data = herdeffect,
            aes(y = Est, x = Herd)) +
          geom_errorbar(
            data = herdeffect,
            width = 0.1,
            aes(y = Est, x = Herd, ymin = CIlow, ymax = CIhigh)) +
          geom_hline(yintercept = 0) +
          labs(x = "", y = "Proportion") +
          theme(text = element_text(size=15),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title=element_blank())

        
      p1 <- ggplot(poppns, aes(x = Herd, ppn, fill = behav)) +
        geom_bar(stat="identity",position='fill') +
        labs(x = "", y = "Proportion") +
        theme(text = element_text(size=15),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title=element_blank())
      
      # no legend (makes combo graph confused)        
      p1 <- ggplot(poppns, aes(x = Herd, ppn, fill = behav)) +
        geom_bar(stat="identity",position='fill') +
        labs(x = "", y = "Proportion") +
        theme(text = element_text(size=15),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
          
 
          # plot
         p2 <- ggplot(herdeffect, aes(y = Est, x = Herd,
            ymin = CIlow, ymax = CIhigh)) +
            geom_point() +
              geom_errorbar(width = 0.1) +
              geom_hline(yintercept = 0) 

         multiplot(p1, p2, cols = 1)
              
          # plot
          ggplot(herddat2, aes(y = Est, x = Herd,
            ymin = CIlow, ymax = CIhigh)) +
            geom_point() +
              geom_errorbar(width = 0.1) +
              geom_hline(yintercept = 0) +
            theme(axis.text.x=element_text(angle=90, hjust=1))
          
          library(grid)
          grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))
      